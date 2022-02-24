{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ParserState where
import Control.Monad.State
import SyntaxTree
import SymbolTable
import TypeTable
import Data.Map as Map
import Data.Maybe
import Prelude hiding (lookup)

type ParserState = (SymbolTable, SymbolTable, TypeTable, ClassTable, ClassTable, Int)

startState = (empty, empty, empty, empty, empty, 0::Int)

doTDecl :: [(String, [(String, String)])] -> State ParserState TypeTable
doTDecl decls = do
    let tTable = genTTable decls
    let newTTable1 = Map.insert "int" Type {typeSize=1, typeFields=empty} tTable
    let newTTable2 = Map.insert "str" Type {typeSize=1, typeFields=empty} newTTable1
    let newTTable3 = Map.insert "bool" Type {typeSize=1, typeFields=empty} newTTable2
    let newTTable4 = Map.insert "null" Type {typeSize=1, typeFields=empty} newTTable3
    put (empty, empty, newTTable4, empty, empty, 0)
    return newTTable4

-- (name, parent), [(type, vname)], [(type, basefun)]
doCDecl :: (String, String) -> [(String, String)] -> [(String, [SymbolFoundation])] -> State ParserState ClassTable
doCDecl (a, p) b c = do
    (gTable, _, tTable, cTable, _, lab) <- get
    let parClass = cTable !? p
    let ((currClassName, x), newlab) = classBuild ((a, p), b, c) parClass (keys tTable) lab
    let (parFun, childFun) = case parClass of
            Just f -> (intersection (classMethods x) (classMethods f), difference (classMethods x) (classMethods f))
            Nothing  -> (empty, classMethods  x)
    let newX = x {classFunOffsets=keys parFun++keys childFun}
    let newCTable = singleton currClassName newX
    let newerCTable = Map.unionWith (error "Class declared multiple times") cTable newCTable
    put (gTable, empty, tTable, newerCTable, newCTable, newlab)
    return newerCTable

doGDecl :: [(String, [SymbolFoundation])] -> State ParserState Int
doGDecl decls = do
    (_, _, tTable, cTable, cTable2, lab) <- get
    let (gSymbolTable, sp, newlab) = genGSymbolTable decls (keys tTable) lab
    let newGSymbolTable = Map.unionWith (error "null is a keyword.") gSymbolTable (Map.singleton "null" Null)
    put (newGSymbolTable, empty, tTable, cTable, empty, newlab)
    return sp

doLDecl :: [(String, [String])] -> State ParserState SymbolTable
doLDecl decls = do
    (gSymbolTable, lArgTable, tTable, cTable, cTable2, lab) <- get
    let lDeclTable = genLSymbolTable decls
    let lSymbolTable = Map.unionWith (error "Arg and local var share name.") lArgTable lDeclTable
    put (gSymbolTable, lSymbolTable, tTable, cTable, cTable2, lab)
    return lSymbolTable

doPDecl :: [(String, String)] -> State ParserState [(String, String)]
doPDecl params = do
    (gSymbolTable, _, tTable, cTable, cTable2, lab) <- get
    let argTable = genPSymbolTable params
    put (gSymbolTable, argTable, tTable, cTable, cTable2, lab)
    return params

doCPDecl :: [(String, String)] -> State ParserState [(String, String)]
doCPDecl params = do
    (gSymbolTable, _, tTable, cTable, currClass, lab) <- get
    let argTable = genCPSymbolTable params
    let newArgTable = unionWith (error "self is keyword.") argTable (singleton "self" LVariable {varType = head $ keys currClass, varLocalBinding = -4})
    let newerArgTable = unionWith (error "vft is keyword.") newArgTable (singleton "vft" LVariable {varType = head $ keys currClass, varLocalBinding = -3})
    put (gSymbolTable, newerArgTable, tTable, cTable, currClass, lab)
    return params

fnDefTypeCheck :: String -> String -> [(String, String)] -> SymbolTable -> Node -> State ParserState FDefinition
fnDefTypeCheck t name params lSymbol lTree = do
    (gSymbolTable, _, tTable, cTable, currClass, lab) <- get
    let fLookup = case Map.lookup name gSymbolTable of
         Just f -> f
         Nothing -> error $ "Function declaration of " ++ name ++ " is missing. " ++ show gSymbolTable
    if funcType fLookup == t && funcParams fLookup == params then
        return FDefinition {fName=name, fType=t, fSymbolTable=lSymbol, fAST=lTree, fClass=head $ keys currClass}
    else typeError $ "at declaration of " ++ name

nodeToType :: Node -> State ParserState String
nodeToType x = do
    (gSymbolTable, lSymbolTable, tTable, cTable, currClass, lab) <- get
    case x of
         NodeOp {} -> return "int"
         NodeBool {} -> return "bool"
         NodeInt _ -> return "int"
         NodeStr _ -> return "str"
         NodeVar var -> return (varType $ Data.Maybe.fromMaybe (Data.Maybe.fromMaybe (argError var) (lookup var gSymbolTable)) (lookup var lSymbolTable))
         NodePtr var -> nodeToType var
         NodeFnCall var _ ->  return (funcType $ Data.Maybe.fromMaybe (Data.Maybe.fromMaybe (argError var) (lookup var gSymbolTable)) (lookup var lSymbolTable))
         NodeField var field -> do
                                t <- nodeToType var
                                let tLookup = lookup t tTable
                                case tLookup of
                                        Nothing -> do
                                            let cLookup = fromJust (lookup t cTable)
                                            let fields = classFields cLookup
                                            let methods = classMethods cLookup
                                            let ffield = lookup field fields
                                            case ffield of
                                                Nothing -> do return (funcType (fromJust (lookup field methods)))
                                                Just f -> do return $ fieldType f
                                        Just f -> return (fieldType (typeFields f! field))
         NodeClassFnCall var _ -> nodeToType var
         NodeSelf -> return $ head $ keys currClass

opTypeCheck :: String -> Node -> Node -> State ParserState Node
opTypeCheck op n1 n2 = do
    t1 <- nodeToType n1
    t2 <- nodeToType n2
    if t1 == "int" && t2 == "int" then return (NodeOp op n1 n2) else typeError ("at " ++ op)

boolTypeCheck :: String -> Node -> Node -> State ParserState Node
boolTypeCheck op n1 n2 = do
    t1 <- nodeToType n1
    t2 <- nodeToType n2
    if t1 == "int" && t2 == "int" then return (NodeBool op n1 n2) else typeError ("at " ++ op)

isDesc :: String -> String -> State ParserState Bool
isDesc c1 c2 = do
    (gSymbolTable, lSymbolTable, tTable, cTable, currClass, lab) <- get
    let cl2 = fromMaybe (typeError "at assignment.") (cTable !? c2)
    case classParent cl2 of
        "Null" -> return False
        x -> if x == c1 then return True else isDesc c1 x

assgTypeCheck :: Node -> Node -> State ParserState Node
assgTypeCheck n1 n2 = do
    t1 <- nodeToType n1
    t2 <- nodeToType n2
    if t1 == t2 then return (NodeAssg n1 n2) else do
        desc <- isDesc t1 t2
        if desc then return (NodeAssg n1 n2) else typeError ("at assignment of " ++ t1 ++ " with " ++ t2)

newTypeCheck :: Node -> String -> State ParserState Node
newTypeCheck n1 t2 = do
    t1 <- nodeToType n1
    if t1 == t2 then return (NodeNew n1 t2) else do
        desc <- isDesc t1 t2
        if desc then return (NodeNew n1 t2) else typeError ("at assignment of " ++ t1 ++ " with " ++ t2)

fnCallTypeCheck :: String -> [Node] -> State ParserState [Node]
fnCallTypeCheck name args = do
    (gSymbolTable, _, _, _, _, _) <- get
    let fLookup = case Map.lookup name gSymbolTable of
         Just f -> f
         Nothing -> error $ "Function declaration of " ++ name ++ " is missing."
    params <- mapM nodeToType args
    if Prelude.map fst (funcParams fLookup) == params then return args
    else typeError $ "at calling of " ++ name

classFnCallTypeCheck :: Node -> [Node] -> State ParserState [Node]
classFnCallTypeCheck (NodeField var fun) args = do
    (gSymbolTable, _, _, cTable, currClass, _) <- get
    cname <- nodeToType var
    let clookup = fromMaybe (error $ "Lookup error at " ++ show var) (lookup cname cTable)
    let cmethodtable = classMethods clookup
    let fdef = case Map.lookup fun cmethodtable of
         Just f -> f
         Nothing -> error $ "Function declaration of " ++ fun ++ " is missing."
    params <- mapM nodeToType args
    if Prelude.map fst (funcParams fdef) == params then return args
    else typeError $ "at calling of " ++ fun

typeError errStr = error $ "Type mismatch " ++ errStr

argError errStr = error $ "Invalid argument: " ++ errStr