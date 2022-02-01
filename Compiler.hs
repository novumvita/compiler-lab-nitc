{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Grammar
import Tokens

import Link (replaceLabels)
import Data.List
import Data.Maybe

import Data.Tree

import Data.Tuple

data Tree a b c = EmptyNode
            | Node a b c [ Main.Tree a b c ]
            deriving Show

data State = State {
    registers :: [Bool],
    labels :: [Bool],
    variables :: [(String, (Int, [Int], String))],    --(mem, size, typ)
    breakLabel :: Int,
    continueLabel :: Int,
    inLoop :: Bool
} deriving Show

makeTreeStmt :: Stmt -> Main.Tree Int String String

makeTreeStmt (Read exp)                     = Main.Node 0 "IO" "READ" [makeTreeExp exp]

makeTreeStmt (Write exp)                    = Main.Node 0 "IO" "WRITE" [makeTreeExp exp]

makeTreeStmt (Assg e1 e2)                   = if checkType left ["VAR"]
                                              then Main.Node 0 "ASSG" "ASSG" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp e1
                                                    right = makeTreeExp e2

makeTreeStmt (If exp slist)                 = if checkType left ["BOOL"]
                                              then Main.Node 0 "FLOW" "IF" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp exp
                                                    right = makeTreeSList slist

makeTreeStmt (IfElse exp islist eslist)     = if checkType left ["BOOL"]
                                              then Main.Node 0 "FLOW" "IFELSE" [left, middle, right]
                                              else error "Type Error"
                                              where left = makeTreeExp exp
                                                    middle = makeTreeSList islist
                                                    right = makeTreeSList eslist

makeTreeStmt (While exp slist)              = if checkType left ["BOOL"]
                                              then Main.Node 0 "FLOW" "WHILE" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp exp
                                                    right = makeTreeSList slist

makeTreeStmt Break                          = Main.Node 0 "FLOW" "BREAK" []

makeTreeStmt Continue                       = Main.Node 0 "FLOW" "CONTINUE" []


makeTreeSList :: SList -> Main.Tree Int String String

makeTreeSList []                = Main.Node 0 "" "" []

makeTreeSList [a]               = makeTreeStmt a

makeTreeSList [a, DeclStmt _]   = makeTreeStmt a

makeTreeSList [a, b]            = Main.Node 0 "" "CONNECTOR" [makeTreeStmt a, makeTreeStmt b]

makeTreeSList (x:xs)            = Main.Node 0 "" "CONNECTOR" [makeTreeStmt x, makeTreeProgram (ProgramStart xs)]


makeTreeProgram :: Program -> Main.Tree Int String String

makeTreeProgram (ProgramStart slist)    = makeTreeSList slist

makeTreeProgram EmptyProgram            = Main.Node 0 "" "" []


checkType :: Main.Tree Int String String  -> [String] -> Bool

checkType (Main.Node _ _ a _) b = a `elem` b

checkType _ _ = False


makeTreeExp :: Exp -> Main.Tree Int String String

makeTreeExp (Num v)                         = Main.Node v "" "INT" []

makeTreeExp (Val (VarExp v))                = Main.Node 0 v "VAR" []

makeTreeExp (Val (VarExpArray v count))     = Main.Node a b c (children ++ [makeTreeExp count])
                                            where (Main.Node a b c children ) = expTree
                                                  expTree = makeTreeExp (Val v)

makeTreeExp (Pointer var)                   = Main.Node 0 "PTR" "VAR" [makeTreeExp (Val var)]

makeTreeExp (Mem var)                       = Main.Node 0 "MEM" "VAR" [makeTreeExp (Val var)]

makeTreeExp (Str v)                         = Main.Node 0 v "STR" []

makeTreeExp (Plus e1 e2)                    = if checkType left ["INT", "VAR"] && checkType right ["INT", "VAR"]
                                              then Main.Node 0 "ADD" "INT" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp e1
                                                    right = makeTreeExp e2

makeTreeExp (Minus e1 e2)                   = if checkType left ["INT", "VAR"] && checkType right ["INT", "VAR"]
                                              then Main.Node 0 "SUB" "INT" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp e1
                                                    right = makeTreeExp e2

makeTreeExp (Times e1 e2)                   = if checkType left ["INT", "VAR"] && checkType right ["INT", "VAR"]
                                              then Main.Node 0 "MUL" "INT" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp e1
                                                    right = makeTreeExp e2

makeTreeExp (Div e1 e2)                     = if checkType left ["INT", "VAR"] && checkType right ["INT", "VAR"]
                                              then Main.Node 0 "DIV" "INT" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp e1
                                                    right = makeTreeExp e2

makeTreeExp (LessThan e1 e2)                = if checkType left ["INT", "VAR"] && checkType right ["INT", "VAR"]
                                              then Main.Node 0 "LT" "BOOL" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp e1
                                                    right = makeTreeExp e2

makeTreeExp (GreaterThan e1 e2)             = if checkType left ["INT", "VAR"] && checkType right ["INT", "VAR"]
                                              then Main.Node 0 "GT" "BOOL" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp e1
                                                    right = makeTreeExp e2

makeTreeExp (LessThanEq e1 e2)              = if checkType left ["INT", "VAR"] && checkType right ["INT", "VAR"]
                                              then Main.Node 0 "LE" "BOOL" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp e1
                                                    right = makeTreeExp e2

makeTreeExp (GreaterThanEq e1 e2)           = if checkType left ["INT", "VAR"] && checkType right ["INT", "VAR"]
                                              then Main.Node 0 "GE" "BOOL" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp e1
                                                    right = makeTreeExp e2

makeTreeExp (NotEq e1 e2)                   = if checkType left ["INT", "VAR"] && checkType right ["INT", "VAR"]
                                              then Main.Node 0 "NE" "BOOL" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp e1
                                                    right = makeTreeExp e2

makeTreeExp (Eq e1 e2)                      = if checkType left ["INT", "VAR"] && checkType right ["INT", "VAR"]
                                              then Main.Node 0 "EQ" "BOOL" [left, right]
                                              else error "Type Error"
                                              where left = makeTreeExp e1
                                                    right = makeTreeExp e2

-- should be 4096       
allocMem :: (String, [Int]) -> String -> State -> State
allocMem (var, num) typ (State a b [] c d e) = State a b [(var, (5000, num, typ))] c d e
allocMem (var, num) typ (State a b varlist c d e) = State a b (varlist ++ [(var, (mem, num, typ))]) c d e
                    where mem = base + product size
                          (base, size, _) = snd $ last varlist

getMem :: Main.Tree Int String String -> State -> IO (State, Int)

getMem (Main.Node 0 v "VAR" []) state = do
    let (newstate, reg) = getReg state
    appendNewLine $ "MOV R" ++ show reg ++ ", " ++ show mem
    return (newstate, reg)
    where (mem, _, _) = fromJust $ lookup v varlist
          (State a b varlist c d e) = state
              
getMem (Main.Node 0 "PTR" "VAR" [child]) state = evalExp child state

getMem (Main.Node 0 v "VAR" [child]) state = do
    let (mem, _, _) = fromJust $ lookup v varlist
    let (newstate, reg) = getReg state
    (newstate1, index) <- evalExp child newstate
    appendNewLine $ "MOV R" ++ show reg ++ ", " ++ show mem
    appendNewLine $ "ADD R" ++ show reg ++ ", R" ++ show index
    let newstate2 = freeReg newstate1
    return (newstate2, reg)
    where (State a b varlist c d e) = state

getMem (Main.Node 0 v "VAR" children) state = do
    let (mem, sizes, _) = fromJust $ lookup v varlist
    let (newstate, reg) = getReg state
    (indexRegs, newstate1) <- evalIndexes children [] newstate
    appendNewLine $ "MOV R" ++ show reg ++ ", 0"
    newstate2 <- findOffset reg sizes indexRegs newstate1
    appendNewLine $ "ADD R" ++ show reg ++ ", " ++ show mem
    return (newstate2, reg)
    where (State a b varlist c d e) = state

switchOn :: [Bool] -> Int -> [Bool]
switchOn list index = take index list ++ True : drop (index + 1) list

switchOff :: [Bool] -> Int -> [Bool]
switchOff list index = take index list ++ False : drop (index + 1) list

getReg :: State -> (State, Int)
getReg (State registers a b c d e) = (State (switchOn registers reg) a b c d e, reg)
                   where reg = fromJust (elemIndex False registers)

freeReg :: State -> State
freeReg (State registers a b c d e) = State (reverse (switchOff revregisters reg)) a b c d e
                    where reg = fromJust (elemIndex True revregisters)
                          revregisters = reverse registers

getLabel :: State -> (State, Int)
getLabel (State a labels b c d e) = (State a (switchOn labels label) b c d e, label)
                   where label = fromJust (elemIndex False labels)

startLoop :: State -> State
startLoop (State a b c d e _) = State a b c d e True

endLoop :: State -> State
endLoop (State a b c d e _) = State a b c d e False

setLabels :: State -> Int -> Int -> State
setLabels (State a b c d e f) label1 label2 = State a b c label1 label2 f

evalExp :: Main.Tree Int String String -> State -> IO (State, Int)

evalExp (Main.Node v "" "INT" [])  state = do
    appendNewLine ("MOV R" ++  show reg ++ ", " ++ show v)
    return (newstate, reg)
    where (newstate, reg) = getReg state

evalExp (Main.Node 0 v "STR" []) state = do
    appendNewLine ("MOV R" ++  show reg ++ ", " ++ v)
    return (newstate, reg)
    where (newstate, reg) = getReg state

evalExp (Main.Node _ "PTR" _ child) state = do
    let (newstate, reg) = getReg state
    (newstate1, childreg) <- evalExp (head child) newstate
    appendNewLine $ "MOV R" ++ show reg ++ ", [R" ++ show childreg ++ "]"
    let newstate2 = freeReg newstate1
    return (newstate2, reg)

evalExp (Main.Node _ "MEM" _ child) state = getMem (head child) state

evalExp (Main.Node 0 v "VAR" children) state = do
    let (newstate, reg) = getReg state
    (newstate1, memreg) <- getMem (Main.Node 0 v "VAR" children) newstate
    appendNewLine ("MOV R" ++  show reg ++ ", [R" ++ show memreg ++ "]")
    let newstate2 = freeReg newstate1
    return (newstate2, reg)

evalExp (Main.Node _ op _ [leftNode, rightNode]) state = do
    (lstate, leftVal) <- evalExp leftNode state
    (rstate, rightVal) <- evalExp rightNode lstate
    appendNewLine (op ++ " R" ++ show leftVal ++ ", R" ++ show rightVal)
    let newstate = freeReg rstate
    return (newstate, leftVal)

evalIndexes :: [Main.Tree Int String String] -> [Int] -> State -> IO ([Int], State)

evalIndexes (leftChild : rest) indexes state = do
    (lstate, leftReg) <- evalExp leftChild state
    evalIndexes rest (leftReg : indexes) lstate

evalIndexes [] indexes state = do return (indexes, state)

findOffset :: Int -> [Int] -> [Int] -> State -> IO State
findOffset reg (firstsize:sizes) (firstindex:indexRegs) state = do
    appendNewLine $ "MUL R" ++ show firstindex ++ ", " ++ show firstsize
    appendNewLine $ "ADD R" ++ show reg ++ ", R" ++ show firstindex
    let newstate = freeReg state
    findOffset reg sizes indexRegs newstate
    where (State registers b varlist c d e) = state

findOffset _ _ _ state = do return state


evalStmt :: Main.Tree Int String String -> State -> IO State

evalStmt (Main.Node _ _ "ASSG" [Main.Node u v "VAR" children, rightNode]) state = do
    (newstate, memreg) <- getMem (Main.Node u v "VAR" children) state
    (rstate, rightVal) <- evalExp rightNode newstate
    appendNewLine $ "MOV [R" ++ show memreg ++ "], R" ++ show rightVal
    let newstate3 = freeReg rstate
    let newstate4 = freeReg newstate3
    return newstate4
    where (State a b varlist c d e) = state

evalStmt (Main.Node _ _ "IF" [leftNode, rightNode]) state = do
    let (newstate, label) = getLabel state
    (lstate, leftVal) <- evalExp leftNode newstate
    appendNewLine $ "JZ R" ++ show leftVal ++ ", L" ++ show label
    rstate <- evalStmt rightNode lstate
    appendNewLine $ "L" ++ show label ++ ":"
    return rstate

evalStmt (Main.Node _ _ "IFELSE" [leftNode, middleNode, rightNode]) state = do
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

evalStmt (Main.Node _ _ "WHILE" [leftNode, rightNode]) state = do
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

evalStmt (Main.Node _ _ "READ" [Main.Node a v "VAR" children]) state = do
    (newstate, memreg) <- getMem (Main.Node a v "VAR" children) state
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

evalStmt (Main.Node _ _ "WRITE" [node]) state = do
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

evalStmt (Main.Node _ _ "BREAK" _) (State a b c breakLabel d True) = do
    appendNewLine $ "JMP L" ++ show breakLabel
    return (State a b c breakLabel d True)

evalStmt (Main.Node _ _ "BREAK" _) (State a b c d e False) = do
    return (State a b c d e False)

evalStmt (Main.Node _ _ "CONTINUE" _) (State a b c d continueLabel True) = do
    appendNewLine $ "JMP L" ++ show continueLabel
    return (State a b c continueLabel d True)

evalStmt (Main.Node _ _ "CONTINUE" _) (State a b c d e False) = do
    return (State a b c d e False)

evalStmt (Main.Node a b c [leftNode, rightNode]) state = do
    newstate <- evalStmt rightNode state
    evalStmt leftNode newstate


toDataTree :: Main.Tree Int String String -> Data.Tree.Tree [Char]
toDataTree EmptyNode = Data.Tree.Node "LEAF" []
toDataTree (Main.Node a b c d) = Data.Tree.Node (show a ++ show b ++ show c) (map toDataTree d)

codeGen :: Main.Tree Int String String -> State -> IO ()
codeGen ast state = do
    writeFile "file.txt" "0\n2056\n0\n0\n0\n0\n0\n0\n"
    evalStmt ast state
    appendFile "file.txt" "INT 10"

appendNewLine :: String -> IO ()
appendNewLine string = appendFile "file.txt" (string ++ "\n")

getDeclarations :: Program -> [Decl]
getDeclarations (ProgramStart(DeclStmt(Declarations declList):_)) = declList
getDeclarations (ProgramStart (x:xs)) = getDeclarations $ ProgramStart xs

sizeOf :: VarDecl -> [Int] -> [Int]
sizeOf (VarDecl _) sizeArray = sizeArray ++ [1]
sizeOf (PointerDecl name) sizeArray = sizeArray ++ [1]
sizeOf (VarArray var size) sizeArray = sizeArray ++ [size] ++ sizeOf var []

nameOf :: VarDecl -> String
nameOf (VarDecl name) = name
nameOf (PointerDecl name) = name
nameOf (VarArray var _) = nameOf var

isPointer :: VarDecl -> String
isPointer (VarArray var _) = isPointer var
isPointer (VarDecl _) = ""
isPointer (PointerDecl _) = "PTR"

doDeclarations :: [Decl] -> State -> State
doDeclarations (Decl typ var : rest) state = doDeclarations rest newstate
                                              where newstate = allocMem (nameOf var, sizeOf var []) (typ ++ isPointer var) state
doDeclarations [] state = state

main :: IO ()
main = do
    let file = "input.txt"
    s <- readFile file
    let tokens = scanTokens s
    let parsedTokens = parseCalc tokens
    print parsedTokens
    let declarations = getDeclarations parsedTokens
    let registers = replicate 20 False
    let labels = replicate 100 False
    let variables = []
    let state = State registers labels variables 0 0 False
    let declaredState = doDeclarations declarations state
    print declaredState
    let ast = makeTreeProgram parsedTokens
    putStrLn $ drawTree $ toDataTree ast
    codeGen ast declaredState
    s <- readFile "file.txt"
    linkedCode <- replaceLabels s
    writeFile "linkedFile.txt" linkedCode
    print "Linked!"