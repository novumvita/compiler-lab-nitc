{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Grammar(parseTokens)
import Tokens
import SymbolTable
import SyntaxTree
import TypeTable

import Link (replaceLabels)
import Data.List
import Data.Maybe
import Data.Map
import Control.Monad

data Tree a b c = EmptyNode
            | Node a b c [ Main.Tree a b c ]
            deriving Show

data State = State {
    registers :: Int,
    labels :: Int,
    typeTable :: TypeTable,
    classTable :: ClassTable,
    gSymbolTable :: SymbolTable,
    lSymbolTable :: SymbolTable,
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
            aNL $ "MOV R" ++ show reg ++ ", BP"
            aNL $ "ADD R" ++ show reg ++ ", " ++ show lbinding
        Variable _ _ gbinding -> do
            aNL $ "MOV R" ++ show reg ++ ", " ++ show gbinding
    return (newstate, reg)
    where sym = getSymbol var state

getMem (NodeField var field) state = do
    (newstate, reg) <- getMem var state
    let vartype = Data.Map.lookup vartypename (typeTable state)
    case vartype of
        Just f -> do
            aNL $ "ADD R" ++ show reg ++ ", " ++ show fieldindex
            return (newstate, reg)
            where fieldindex = fieldIndex $ fromMaybe (error $ show field) (typeFields f !? field)
        Nothing -> do
            aNL $ "ADD R" ++ show reg ++ ", " ++ show fieldindex
            return (newstate, reg)
            where fieldindex = fieldIndex $ fromMaybe (error $ show field) (classFields varclass !? field)
                  varclass = fromMaybe (error $ show vartypename ) (classTable state !? vartypename)
    where vartypename = getObjClass var state

getMem (NodePtr child) state = evalExp child state

getMem (NodeArray var indexes) state = do
    let sym = fromMaybe (error $ show var) (varlist !? var)
    let (newstate, reg) = getReg state
    (indexRegs, newstate1) <- evalIndexes indexes [] newstate
    aNL $ "MOV R" ++ show reg ++ ", 0"
    newstate2 <- findOffset reg (varSize sym) indexRegs newstate1
    aNL $ "ADD R" ++ show reg ++ ", " ++ show (varBinding sym)
    return (newstate2, reg)
    where varlist = gSymbolTable state

getMem NodeNull state = do
    let (newstate, reg) = getReg state
    aNL $ "MOV R" ++ show reg ++ ", \"NULL\""
    return (newstate, reg)

getMem NodeSelf state = do
    let (newstate, reg) = getReg state
    case sym of
        LVariable _ lbinding -> do
            aNL $ "MOV R" ++ show reg ++ ", BP"
            aNL $ "ADD R" ++ show reg ++ ", " ++ show lbinding
    return (newstate, reg)
    where sym = lSymbolTable state ! "self"

getMem f state = error $ show f

getType :: Node -> State -> String
getType (NodePtr var) state = getType var state
getType (NodeVar var) state = varType (getSymbol var state)
getType (NodeField var field) state = fieldtype
    where vartype = getType var state
          tLookup = typeTable state ! vartype
          varfields = typeFields tLookup
          fieldtype = fieldType (varfields ! field)
getType NodeSelf state = varType (lSymbolTable state ! "self")


getSymbol :: String -> State -> Symbol
getSymbol var state = fromMaybe gLookup $ Data.Map.lookup var  (lSymbolTable state)
          where gLookup = fromMaybe (error var) $ Data.Map.lookup var (gSymbolTable state)

switchOn :: [Bool] -> Int -> [Bool]
switchOn list index = Data.List.take index list ++ True : Data.List.drop (index + 1) list

switchOff :: [Bool] -> Int -> [Bool]
switchOff list index = Data.List.take index list ++ False : Data.List.drop (index + 1) list

getReg :: State -> (State, Int)
getReg state = (state {registers=register+1}, register)
                where register = registers state

getLabel :: State -> (State, Int)
getLabel state = (state {labels=label+1}, label)
                where label = labels state

lastUsedReg :: State -> Int
lastUsedReg = registers

freeReg :: State -> State
freeReg state = state {registers=register-1}
                where register = registers state

startLoop :: State -> State
startLoop state = state{inLoop=True}

endLoop :: State -> State
endLoop state = state{inLoop=False}

setLabels :: State -> Int -> Int -> State
setLabels state label1 label2 = state{breakLabel=label1, continueLabel=label2}

evalExp :: Node -> State -> IO (State, Int)

evalExp (NodeInt val)  state = do
    aNL ("MOV R" ++  show reg ++ ", " ++ show val)
    return (newstate, reg)
    where (newstate, reg) = getReg state

evalExp (NodeStr val) state = do
    aNL ("MOV R" ++  show reg ++ ", " ++ val)
    return (newstate, reg)
    where (newstate, reg) = getReg state

evalExp (NodePtr child) state = do
    let (newstate, reg) = getReg state
    (newstate1, childreg) <- evalExp child newstate
    aNL $ "MOV R" ++ show reg ++ ", [R" ++ show childreg ++ "]"
    let newstate2 = freeReg newstate1
    return (newstate2, reg)

evalExp (NodeRef child) state = getMem child state


evalExp (NodeOp op leftNode rightNode) state = do
    (lstate, leftVal) <- evalExp leftNode state
    (rstate, rightVal) <- evalExp rightNode lstate
    aNL (translateOp op ++ " R" ++ show leftVal ++ ", R" ++ show rightVal)
    let newstate = freeReg rstate
    return (newstate, leftVal)

evalExp (NodeBool op leftNode rightNode) state = do
    (lstate, leftVal) <- evalExp leftNode state
    (rstate, rightVal) <- evalExp rightNode lstate
    aNL (translateOp op ++ " R" ++ show leftVal ++ ", R" ++ show rightVal)
    let newstate = freeReg rstate
    return (newstate, leftVal)

evalExp (NodeFnCall name args) state = do
    pushRegs state
    pushArgs (reverse args) state {registers=0}
    aNL "PUSH R0"
    aNL $ "CALL F" ++ show (funcLabel (gSymbolTable state ! name))
    aNL $ "POP R" ++ show reg
    popArgs (length args)
    popRegs state
    return (newstate, reg)
    where (newstate,reg) = getReg state

evalExp (NodeClassFnCall (NodeField var field) args) state = do
    pushRegs state
    pushArgs (reverse args) state {registers=0}
    (newstate2, memreg) <- getMem var state
    aNL $ "MOV R" ++ show reg ++ ", R" ++ show memreg
    pushReg reg
    aNL "PUSH R0"
    aNL $ "CALL F" ++ funclass ++ show (funcLabel (classMethods (classTable state ! funclass) ! field))
    aNL "BRKP"
    aNL $ "POP R" ++ show reg
    popArgs (length args + 1)
    popRegs state
    return (newstate, reg)
    where (newstate,reg) = getReg state
          funclass = getObjClass var state

evalExp NodeNull state = do
    let (newstate, reg) = getReg state
    aNL $ "MOV R" ++ show reg ++ ", \"NULL\""
    return (newstate, reg)

evalExp var state = do
    let (newstate, reg) = getReg state
    (newstate1, memreg) <- getMem var newstate
    aNL ("MOV R" ++  show reg ++ ", [R" ++ show memreg ++ "]")
    let newstate2 = freeReg newstate1
    return (newstate2, reg)

getObjClass :: Node -> State -> String
getObjClass (NodeVar name) state = varType (fromMaybe (fromJust (Data.Map.lookup name (gSymbolTable state))) (Data.Map.lookup name (lSymbolTable state)))
getObjClass (NodePtr v) state = getObjClass v state
getObjClass NodeSelf state = varType (lSymbolTable state ! "self")
getObjClass (NodeField v fl) state = do
    case tLookup of
        Nothing -> fieldType fi
        Just f -> fieldtype
            where varfields = typeFields f
                  fieldtype = fieldType (varfields ! fl)
    where vartype = getType v state
          tLookup = typeTable state !? vartype
          cl = fromMaybe (error "cl") (classTable state !? getObjClass v state)
          fi = fromMaybe (error "fi") (classFields cl !? fl)
-- getObjClass f state = error $ show f

pushArgs :: [Node] -> State -> IO State

pushArgs [] state = return state

pushArgs args state = do
    (_, reg) <- case args of
        [arg] ->  evalExp arg state
        args ->  evalExp (last args) state
    aNL $ "PUSH R" ++ show reg
    pushArgs (init args) state

popArgs :: Int -> IO ()
popArgs n = case n of
    0 -> return ()
    x -> do
         popReg 19
         popArgs (x - 1)

popReg :: Int -> IO()
popReg reg = aNL $ "POP R" ++ show reg

pushReg :: Int -> IO()
pushReg reg = aNL $ "PUSH R" ++ show reg

popRegs :: State -> IO()
popRegs state = case register of
    0 -> return ()
    reg -> mapM_ popReg [0 .. reg - 1]
    where register = registers state

pushRegs :: State -> IO()
pushRegs state = case register of
    0 -> return ()
    reg -> mapM_ pushReg [reg - 1, reg - 2 .. 0]
    where register = registers state

evalIndexes :: [Node] -> [Int] -> State -> IO ([Int], State)

evalIndexes (leftChild : rest) indexes state = do
    (lstate, leftReg) <- evalExp leftChild state
    evalIndexes rest (leftReg : indexes) lstate

evalIndexes [] indexes state = do return (indexes, state)

findOffset :: Int -> [Int] -> [Int] -> State -> IO State
findOffset reg (firstsize : sizes) (firstindex:indexRegs) state = do
    aNL $ "MUL R" ++ show firstindex ++ ", " ++ show firstsize
    aNL $ "ADD R" ++ show reg ++ ", R" ++ show firstindex
    let newstate = freeReg state
    findOffset reg sizes indexRegs newstate
findOffset _ _ _ state = do return state


evalStmt :: Node -> State -> IO State

evalStmt (NodeAssg leftNode rightNode) state = do
    (newstate, memreg) <- getMem leftNode state
    (rstate, rightVal) <- evalExp rightNode newstate
    aNL $ "MOV [R" ++ show memreg ++ "], R" ++ show rightVal
    let newstate3 = freeReg rstate
    let newstate4 = freeReg newstate3
    return newstate4

evalStmt (NodeIf leftNode middleNode (NodeConnector [])) state = do
    let (newstate, label) = getLabel state
    (lstate, leftVal) <- evalExp leftNode newstate
    aNL $ "JZ R" ++ show leftVal ++ ", L" ++ show label
    rstate <- evalStmt middleNode lstate
    aNL $ "L" ++ show label ++ ":"
    return rstate

evalStmt (NodeIf leftNode middleNode rightNode) state = do
    (lstate, leftVal) <- evalExp leftNode state
    let (lstate1, label1) = getLabel lstate
    let (lstate2, label2) = getLabel lstate1
    aNL $ "JZ R" ++ show leftVal ++ ", L" ++ show label1
    lstate3 <- evalStmt middleNode lstate2
    aNL $ "JNZ R" ++ show leftVal ++ ", L" ++ show label2
    aNL $ "L" ++ show label1 ++ ":"
    res <- evalStmt rightNode lstate3
    aNL $ "L" ++ show label2 ++ ":"
    return res

evalStmt (NodeWhile leftNode rightNode) state = do
    let (state1, label1) = getLabel state
    aNL $ "L" ++ show label1 ++ ":"
    let state2 = startLoop state1
    (lstate, leftVal) <- evalExp leftNode state2
    let (lstate2, label2) = getLabel lstate
    aNL $ "JZ R" ++ show leftVal ++ ", L" ++ show label2
    let newstate = freeReg lstate2
    let oldBLabel = breakLabel lstate2
    let oldCLabel = continueLabel lstate2
    let newstate1 = setLabels newstate label2 label1
    res <- evalStmt rightNode newstate1
    aNL $ "JMP L" ++ show label1
    aNL $ "L" ++ show label2 ++ ":"
    let newstate1 = endLoop res
    let newstate2 = setLabels newstate1 oldBLabel oldCLabel
    return newstate2

evalStmt (NodeRead var) state = do
    pushRegs state
    (newstate, memreg) <- getMem var state
    let (newstate1, reg) = getReg newstate
    aNL $ "MOV R" ++ show reg ++ ", \"Read\""
    aNL $ "PUSH R" ++ show reg
    aNL $ "MOV R" ++ show reg ++ ", -1"
    aNL $ "PUSH R" ++ show reg
    aNL $ "MOV R" ++ show reg ++ ", R" ++ show memreg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL "CALL 0"
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    popRegs state
    return state

evalStmt (NodeWrite node) state = do
    pushRegs state
    (newstate, val) <- evalExp node state
    let (newstate1, reg) = getReg newstate
    aNL $ "MOV R" ++ show reg ++ ", \"Write\""
    aNL $ "PUSH R" ++ show reg
    aNL $ "MOV R" ++ show reg ++ ", -2"
    aNL $ "PUSH R" ++ show reg
    aNL $ "MOV R" ++ show reg ++ ", R" ++ show val
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL "CALL 0"
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    popRegs state
    return state

evalStmt (NodeAlloc var) state = do
    pushRegs state
    let size = typeSize (typeTable state ! getType var state)
    let (newstate, reg) = getReg state
    aNL $ "MOV R" ++ show reg ++ ", \"Alloc\""
    aNL $ "PUSH R" ++ show reg
    aNL $ "MOV R" ++ show reg ++ ", " ++ show size
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL "CALL 0"
    let (newstate1, newreg) = getReg newstate
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show newreg
    aNL $ "POP R" ++ show newreg
    aNL $ "POP R" ++ show newreg
    aNL $ "POP R" ++ show newreg
    (leftstate, memreg) <- getMem var newstate1
    aNL $ "MOV [R" ++ show memreg ++ "], R" ++ show reg
    popRegs state
    return state

evalStmt (NodeNew var c) state = do
    pushRegs state
    let s = size (classFields (classTable state ! c))
    let (newstate, reg) = getReg state
    aNL $ "MOV R" ++ show reg ++ ", \"Alloc\""
    aNL $ "PUSH R" ++ show reg
    aNL $ "MOV R" ++ show reg ++ ", " ++ show s
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL "CALL 0"
    let (newstate1, newreg) = getReg newstate
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show newreg
    aNL $ "POP R" ++ show newreg
    aNL $ "POP R" ++ show newreg
    aNL $ "POP R" ++ show newreg
    (leftstate, memreg) <- getMem var newstate1
    aNL $ "MOV [R" ++ show memreg ++ "], R" ++ show reg
    popRegs state
    return state

evalStmt (NodeFree var) state = do
    pushRegs state
    (newstate, memreg) <- getMem var state
    let (newstate1, reg) = getReg newstate
    aNL $ "MOV R" ++ show reg ++ ", \"Free\""
    aNL $ "PUSH R" ++ show reg
    aNL $ "MOV R" ++ show reg ++ ", R" ++ show memreg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL "CALL 0"
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    popRegs state
    return state

evalStmt NodeInit state = do
    pushRegs state
    let (newstate, reg) = getReg state
    aNL $ "MOV R" ++ show reg ++ ", \"Heapset\""
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL $ "PUSH R" ++ show reg
    aNL "CALL 0"
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    aNL $ "POP R" ++ show reg
    popRegs state
    return state

evalStmt NodeBreak state = do
    if inLoop state then aNL ("JMP L" ++ show (breakLabel state)) >> return state else return state

evalStmt NodeContinue state = do
    if inLoop state then aNL ("JMP L" ++ show (continueLabel state)) >> return state else return state

evalStmt (NodeConnector []) state = return state

evalStmt (NodeConnector stmts) state = foldM (flip evalStmt) state stmts

mainCodeGen :: Node -> State -> Int -> IO State
mainCodeGen node state sp = do
    writeFile "file.txt" "0\n2056\n0\n0\n0\n0\n0\n0\n"
    aNL $ "MOV SP, " ++ show sp
    aNL "MOV BP, SP"
    aNL $ "ADD SP, " ++ show lVarCount
    newstate <- foldM (flip evalStmt) state stmts
    aNL "INT 10"
    return newstate
    where lVarCount = Data.Map.size (lSymbolTable state)
          (stmts, retnode) = case children of
              [NodeReturn exp] -> ([], exp)
              f -> (tail f, head f)
          NodeConnector children = node
          NodeReturn retexp = retnode

fnCodeGen :: FDefinition -> State -> IO State
fnCodeGen fdef state = do
    -- print "FSYMTABLE: "
    -- print $ fSymbolTable fdef
    -- print "FAST: "
    -- print $ fAST fdef
    aNL $ "F" ++ show (funcLabel func) ++ ":"
    aNL "PUSH BP"
    aNL "MOV BP, SP"
    aNL $ "ADD SP, " ++ show lVarCount
    newstate <- foldM (flip evalStmt) state1 stmts
    (newstate1, reg) <- evalExp retexp newstate
    aNL "SUB BP, 2"
    aNL $ "MOV [BP], R" ++ show reg
    aNL $ "SUB SP, " ++ show lVarCount
    aNL "POP BP"
    aNL "RET"
    return newstate1{registers=0}
    where lVarCount = Data.Map.size (fSymbolTable fdef) - length (funcParams func)
          func = gSymbolTable state ! fName fdef
          (stmts, retnode) = case children of
              [f] -> ([], f)
              f -> (tail f, head f)
          NodeReturn retexp = retnode
          NodeConnector children = fAST fdef
          state1 = state { lSymbolTable = fSymbolTable fdef }

cFnCodeGen :: FDefinition -> State -> IO State
cFnCodeGen fdef state = do
    -- print "FSYMTABLE: "
    -- print $ fSymbolTable fdef
    -- print "FAST: "
    -- print $ fAST fdef
    aNL $ "F" ++ fClass fdef ++ show (funcLabel func) ++ ":"
    aNL "PUSH BP"
    aNL "MOV BP, SP"
    aNL $ "ADD SP, " ++ show lVarCount
    newstate <- foldM (flip evalStmt) state1 stmts
    (newstate1, reg) <- evalExp retexp newstate
    aNL "SUB BP, 2"
    aNL $ "MOV [BP], R" ++ show reg
    aNL $ "SUB SP, " ++ show lVarCount
    aNL "POP BP"
    aNL "RET"
    -- print $ fName fdef ++ " done"
    return newstate1{registers=0}
    where lVarCount = Data.Map.size (fSymbolTable fdef) - (length (funcParams func) + 1)
          func = classMethods (classTable state ! fClass fdef) ! fName fdef
          (stmts, retnode) = case children of
              [f] -> ([], f)
              f -> (tail f, head f)
          NodeReturn retexp = retnode
          NodeConnector children = fAST fdef
          state1 = state { lSymbolTable = fSymbolTable fdef }

aNL :: String -> IO ()
aNL string = appendFile "file.txt" (string ++ "\n")

main :: IO ()
main = do
    let file = "input.txt"
    s <- readFile file
    let tokens = scanTokens s
    let (typeTable, (cTable, cFDefs), gSymTable, sp, fDef, (mainSymbols, mainAST)) = parseTokens tokens
    let state = State 0 0 typeTable cTable gSymTable mainSymbols 0 0 False ""
    -- print $ "CFDEF: " ++ show cFDefs
    -- print $ "FDEF: " ++ show fDef
    -- print $ "CTABLE: " ++ show cTable
    -- print $ "GSYMTABLE: " ++ show gSymTable
    -- print $ "TTABLE: " ++ show typeTable
    -- print $ "MAINSYMTABLE: " ++ show mainSymbols
    -- print $ "MAINAST: " ++ show mainAST
    newstate <- mainCodeGen mainAST state sp
    newerstate <- foldM (flip fnCodeGen) newstate{registers=0} fDef
    foldM_ (flip cFnCodeGen) newerstate{registers=0} cFDefs
    s <- readFile "file.txt"
    linkedCode <- replaceLabels s
    writeFile "linkedFile.txt" linkedCode
    print "Linked!"