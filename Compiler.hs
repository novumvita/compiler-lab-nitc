{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Grammar(parseTokens)
import Tokens
import SymbolTable
import SyntaxTree

import Link (replaceLabels)
import Data.List
import Data.Maybe
import Data.Tree
import Data.Tuple
import Data.Map
import ParserState
import Control.Monad

data Tree a b c = EmptyNode
            | Node a b c [ Main.Tree a b c ]
            deriving Show

data State = State {
    registers :: [Bool],
    labels :: Int,
    gSymbolTable :: Map String Symbol,
    lSymbolTable :: Map String Symbol,
    breakLabel :: Int,
    continueLabel :: Int,
    inLoop :: Bool,
    code :: String
} deriving Show

translateOp :: String -> String
translateOp op = case op of
    "+" -> "ADD"
    "-" -> "SUB"
    "*" -> "MUL"
    "/" -> "DIV"
    "<" -> "LT"
    ">" -> "GT"
    "<=" -> "LE"
    ">=" -> "GE"
    "!=" -> "NE"
    "==" -> "EQ"


getMem :: Node -> State -> IO (State, Int)

getMem (NodeVar var) state = do
    let (newstate, reg) = getReg state
    case sym of
        LVariable _ lbinding -> do
            appendNewLine $ "MOV R" ++ show reg ++ ", BP"
            appendNewLine $ "ADD R" ++ show reg ++ ", " ++ show lbinding
        Variable _ _ gbinding -> do
            appendNewLine $ "MOV R" ++ show reg ++ ", " ++ show gbinding
    return (newstate, reg)
    where sym = fromMaybe gLookup $ Data.Map.lookup var lvarlist
          gLookup = fromMaybe (error var) $ Data.Map.lookup var gvarlist
          (State a b gvarlist lvarlist c d e f) = state

getMem (NodePtr child) state = evalExp child state

getMem (NodeArray var indexes) state = do
    let sym = varlist ! var
    let (newstate, reg) = getReg state
    (indexRegs, newstate1) <- evalIndexes indexes [] newstate
    appendNewLine $ "MOV R" ++ show reg ++ ", 0"
    newstate2 <- findOffset reg (varSize sym) indexRegs newstate1
    appendNewLine $ "ADD R" ++ show reg ++ ", " ++ show (varBinding sym)
    return (newstate2, reg)
    where (State a b varlist z c d e f) = state

switchOn :: [Bool] -> Int -> [Bool]
switchOn list index = Data.List.take index list ++ True : Data.List.drop (index + 1) list

switchOff :: [Bool] -> Int -> [Bool]
switchOff list index = Data.List.take index list ++ False : Data.List.drop (index + 1) list

getReg :: State -> (State, Int)
getReg (State registers a b z c d e f) = (State (switchOn registers reg) a b z c d e f, reg)
                   where reg = fromJust (elemIndex False registers)

lastUsedReg :: State -> Int
lastUsedReg (State registers a b z c d e f) = case elemIndex False registers of
    Just f -> f - 1
    Nothing -> 20

freeReg :: State -> State
freeReg (State registers a b c z d e f) = State (reverse (switchOff revregisters reg)) a b c z d e f
                    where reg = fromJust (elemIndex True revregisters)
                          revregisters = reverse registers

getLabel :: State -> (State, Int)
getLabel (State a label b z c d e f) = (State a (label+1) b z c d e f, label)

startLoop :: State -> State
startLoop (State a b c z d e _ f) = State a b c z d e True f

endLoop :: State -> State
endLoop (State a b c z d e _ f) = State a b c z d e False f

setLabels :: State -> Int -> Int -> State
setLabels (State a b c z d e f g) label1 label2 = State a b c z label1 label2 f g

evalExp :: Node -> State -> IO (State, Int)

evalExp (NodeInt val)  state = do
    appendNewLine ("MOV R" ++  show reg ++ ", " ++ show val)
    return (newstate, reg)
    where (newstate, reg) = getReg state

evalExp (NodeStr val) state = do
    appendNewLine ("MOV R" ++  show reg ++ ", " ++ val)
    return (newstate, reg)
    where (newstate, reg) = getReg state

evalExp (NodePtr child) state = do
    let (newstate, reg) = getReg state
    (newstate1, childreg) <- evalExp child newstate
    appendNewLine $ "MOV R" ++ show reg ++ ", [R" ++ show childreg ++ "]"
    let newstate2 = freeReg newstate1
    return (newstate2, reg)

evalExp (NodeRef child) state = getMem child state


evalExp (NodeOp op leftNode rightNode) state = do
    (lstate, leftVal) <- evalExp leftNode state
    (rstate, rightVal) <- evalExp rightNode lstate
    appendNewLine (translateOp op ++ " R" ++ show leftVal ++ ", R" ++ show rightVal)
    let newstate = freeReg rstate
    return (newstate, leftVal)

evalExp (NodeBool op leftNode rightNode) state = do
    (lstate, leftVal) <- evalExp leftNode state
    (rstate, rightVal) <- evalExp rightNode lstate
    appendNewLine (translateOp op ++ " R" ++ show leftVal ++ ", R" ++ show rightVal)
    let newstate = freeReg rstate
    return (newstate, leftVal)

evalExp (NodeFnCall name args) state = do
    pushRegs state
    pushArgs (reverse args) state {registers = replicate 20 False}
    appendNewLine "PUSH R0"
    appendNewLine $ "CALL F" ++ show (funcLabel (gSymbolTable state ! name))
    appendNewLine $ "POP R" ++ show reg
    popArgs (length args)
    popRegs state
    return (newstate, reg)
    where (newstate,reg) = getReg state

evalExp var state = do
    let (newstate, reg) = getReg state
    (newstate1, memreg) <- getMem var newstate
    appendNewLine ("MOV R" ++  show reg ++ ", [R" ++ show memreg ++ "]")
    let newstate2 = freeReg newstate1
    return (newstate2, reg)

pushArgs :: [Node] -> State -> IO State

pushArgs [] state = return state

pushArgs args state = do
    (_, reg) <- case args of
        [arg] ->  evalExp arg state
        args ->  evalExp (last args) state
    appendNewLine $ "PUSH R" ++ show reg
    pushArgs (init args) state

popArgs :: Int -> IO ()
popArgs n = case n of
    0 -> return ()
    x -> do
         popReg 0
         popArgs (x - 1) 

popReg :: Int -> IO()
popReg reg = appendNewLine $ "POP R" ++ show reg

pushReg :: Int -> IO()
pushReg reg = appendNewLine $ "PUSH R" ++ show reg

popRegs :: State -> IO()
popRegs state = mapM_ popReg (elemIndices True (registers state))

pushRegs :: State -> IO()
pushRegs state = mapM_ pushReg (reverse (elemIndices True (registers state)))


evalIndexes :: [Node] -> [Int] -> State -> IO ([Int], State)

evalIndexes (leftChild : rest) indexes state = do
    (lstate, leftReg) <- evalExp leftChild state
    evalIndexes rest (leftReg : indexes) lstate

evalIndexes [] indexes state = do return (indexes, state)

findOffset :: Int -> [Int] -> [Int] -> State -> IO State
findOffset reg (firstsize : sizes) (firstindex:indexRegs) state = do
    appendNewLine $ "MUL R" ++ show firstindex ++ ", " ++ show firstsize
    appendNewLine $ "ADD R" ++ show reg ++ ", R" ++ show firstindex
    let newstate = freeReg state
    findOffset reg sizes indexRegs newstate
    where (State registers b varlist z c d e f) = state
findOffset _ _ _ state = do return state


evalStmt :: Node -> State -> IO State

evalStmt (NodeAssg leftNode rightNode) state = do
    (newstate, memreg) <- getMem leftNode state
    (rstate, rightVal) <- evalExp rightNode newstate
    appendNewLine $ "MOV [R" ++ show memreg ++ "], R" ++ show rightVal
    let newstate3 = freeReg rstate
    let newstate4 = freeReg newstate3
    return newstate4
    where (State a b varlist z c d e f) = state

evalStmt (NodeIf leftNode middleNode (NodeConnector [])) state = do
    let (newstate, label) = getLabel state
    (lstate, leftVal) <- evalExp leftNode newstate
    appendNewLine $ "JZ R" ++ show leftVal ++ ", L" ++ show label
    rstate <- evalStmt middleNode lstate
    appendNewLine $ "L" ++ show label ++ ":"
    return rstate

evalStmt (NodeIf leftNode middleNode rightNode) state = do
    (lstate, leftVal) <- evalExp leftNode state
    let (lstate1, label1) = getLabel lstate
    let (lstate2, label2) = getLabel lstate1
    appendNewLine $ "JZ R" ++ show leftVal ++ ", L" ++ show label1
    lstate3 <- evalStmt middleNode lstate2
    appendNewLine $ "JNZ R" ++ show leftVal ++ ", L" ++ show label2
    appendNewLine $ "L" ++ show label1 ++ ":"
    res <- evalStmt rightNode lstate3
    appendNewLine $ "L" ++ show label2 ++ ":"
    return res

evalStmt (NodeWhile leftNode rightNode) state = do
    let (state1, label1) = getLabel state
    appendNewLine $ "L" ++ show label1 ++ ":"
    let state2 = startLoop state1
    (lstate, leftVal) <- evalExp leftNode state2
    let (lstate2, label2) = getLabel lstate
    appendNewLine $ "JZ R" ++ show leftVal ++ ", L" ++ show label2
    let newstate = freeReg lstate2
    let oldBLabel = breakLabel lstate2
    let oldCLabel = continueLabel lstate2
    let newstate1 = setLabels newstate label2 label1
    res <- evalStmt rightNode newstate1
    appendNewLine $ "JMP L" ++ show label1
    appendNewLine $ "L" ++ show label2 ++ ":"
    let newstate1 = endLoop res
    let newstate2 = setLabels newstate1 oldBLabel oldCLabel
    return newstate2

evalStmt (NodeRead var) state = do
    (newstate, memreg) <- getMem var state
    let (newstate1, reg) = getReg newstate
    appendNewLine $ "MOV R" ++ show reg ++ ", 7"
    appendNewLine $ "PUSH R" ++ show reg
    appendNewLine $ "MOV R" ++ show reg ++ ", -1"
    appendNewLine $ "PUSH R" ++ show reg
    appendNewLine $ "MOV R" ++ show reg ++ ", R" ++ show memreg
    appendNewLine $ "PUSH R" ++ show reg
    appendNewLine $ "PUSH R" ++ show reg
    appendNewLine $ "PUSH R" ++ show reg
    appendNewLine "INT 6"
    appendNewLine $ "POP R" ++ show reg
    appendNewLine $ "POP R" ++ show reg
    appendNewLine $ "POP R" ++ show reg
    appendNewLine $ "POP R" ++ show reg
    appendNewLine $ "POP R" ++ show reg
    let newstate2 = freeReg newstate1
    let newstate3 = freeReg newstate2
    return newstate3

evalStmt (NodeWrite node) state = do
    (newstate, val) <- evalExp node state
    let (newstate1, reg) = getReg newstate
    appendNewLine $ "MOV R" ++ show reg ++ ", 5"
    appendNewLine $ "PUSH R" ++ show reg
    appendNewLine $ "MOV R" ++ show reg ++ ", -2"
    appendNewLine $ "PUSH R" ++ show reg
    appendNewLine $ "MOV R" ++ show reg ++ ", R" ++ show val
    appendNewLine $ "PUSH R" ++ show reg
    appendNewLine $ "PUSH R" ++ show reg
    appendNewLine $ "PUSH R" ++ show reg
    appendNewLine "INT 7"
    appendNewLine $ "POP R" ++ show reg
    appendNewLine $ "POP R" ++ show reg
    appendNewLine $ "POP R" ++ show reg
    appendNewLine $ "POP R" ++ show reg
    appendNewLine $ "POP R" ++ show reg
    let newstate2 = freeReg newstate1
    let newstate3 = freeReg newstate2
    return newstate3

evalStmt NodeBreak (State a b c z breakLabel d True f) = do
    appendNewLine $ "JMP L" ++ show breakLabel
    return (State a b c z breakLabel d True f)

evalStmt NodeBreak (State a b c z d e False f) = do
    return (State a b c z d e False f)

evalStmt NodeContinue  (State a b c z d continueLabel True f) = do
    appendNewLine $ "JMP L" ++ show continueLabel
    return (State a b c z continueLabel d True f)

evalStmt NodeContinue (State a b c z d e False f) = do
    return (State a b c z d e False f)

evalStmt (NodeConnector []) state = return state

evalStmt (NodeConnector stmts) state = foldM (flip evalStmt) state stmts

mainCodeGen :: Node -> State -> Int -> IO ()
mainCodeGen node state sp = do
    writeFile "file.txt" "0\n2056\n0\n0\n0\n0\n0\n0\n"
    appendNewLine $ "MOV SP, " ++ show sp
    appendNewLine "BRKP"
    appendNewLine "MOV BP, SP"
    appendNewLine $ "ADD SP, " ++ show lVarCount
    foldM_ (flip evalStmt) state stmts
    appendNewLine "INT 10"
    where lVarCount = Data.Map.size (lSymbolTable state)
          (stmts, retnode) = case children of
              [NodeReturn exp] -> ([], exp)
              f -> (tail f, head f)
          NodeConnector children = node
          NodeReturn retexp = retnode

fnCodeGen :: FDefinition -> State -> IO State
fnCodeGen fdef state = do
    -- print $ fSymbolTable fdef
    appendNewLine $ "F" ++ show (funcLabel func) ++ ":"
    appendNewLine "PUSH BP"
    appendNewLine "MOV BP, SP"
    appendNewLine $ "ADD SP, " ++ show lVarCount
    newstate <- foldM (flip evalStmt) state1 stmts
    (newstate1, reg) <- evalExp retexp newstate
    appendNewLine "SUB BP, 2"
    appendNewLine $ "MOV [BP], R" ++ show reg
    appendNewLine $ "SUB SP, " ++ show lVarCount
    appendNewLine "POP BP"
    appendNewLine "BRKP"
    appendNewLine "RET"
    return newstate1
    where lVarCount = Data.Map.size (fSymbolTable fdef) - length (funcParams func)
          func = gSymbolTable state ! fName fdef
          (stmts, retnode) = case children of
              [NodeReturn exp] -> ([], exp)
              f -> (tail f, head f)
          NodeConnector children = fAST fdef
          NodeReturn retexp = retnode
          state1 = state { lSymbolTable = fSymbolTable fdef }

appendNewLine :: String -> IO ()
appendNewLine string = appendFile "file.txt" (string ++ "\n")

main :: IO ()
main = do
    let file = "input.txt"
    s <- readFile file
    let tokens = scanTokens s
    let (gSymTable, sp, fDef, (mainSymbols, mainAST)) = parseTokens tokens
    let registers = replicate 20 False
    let labels = 0
    let state = State registers labels gSymTable mainSymbols 0 0 False ""
    print gSymTable
    -- print mainSymbols
    mainCodeGen mainAST state sp
    foldM_ (flip fnCodeGen) state fDef
    s <- readFile "file.txt"
    linkedCode <- replaceLabels s
    writeFile "linkedFile.txt" linkedCode
    print "Linked!"