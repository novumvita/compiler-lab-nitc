{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module ParserState where
import Control.Monad.State
import SyntaxTree
import SymbolTable
import TypeTable
import Data.Map as Map
import qualified Data.Maybe
import Prelude hiding (lookup)

type ParserState = (SymbolTable, SymbolTable, TypeTable)

startState = (empty, empty, empty)

doTDecl :: [(String, [(String, String)])] -> State ParserState TypeTable
doTDecl decls = do
    let tTable = genTTable decls
    let newTTable1 = Map.insert "int" Type {typeSize=1, typeFields=empty} tTable
    let newTTable2 = Map.insert "str" Type {typeSize=1, typeFields=empty} newTTable1
    let newTTable3 = Map.insert "bool" Type {typeSize=1, typeFields=empty} newTTable2
    let newTTable4 = Map.insert "null" Type {typeSize=1, typeFields=empty} newTTable3
    put (empty, empty, newTTable4)
    return tTable

doGDecl :: [(String, [SymbolFoundation])] -> State ParserState Int
doGDecl decls = do
    (_, _, tTable) <- get
    let (gSymbolTable, sp, _) = genGSymbolTable decls
    let newGSymbolTable = Map.unionWith (error "null is a keyword.") gSymbolTable (Map.singleton "null" Null)
    put (newGSymbolTable, empty, tTable)
    return sp

doLDecl :: [(String, [String])] -> State ParserState SymbolTable
doLDecl decls = do
    (gSymbolTable, lArgTable, tTable) <- get
    let lDeclTable = genLSymbolTable decls
    let lSymbolTable = Map.unionWith (error "Arg and local var share name.") lArgTable lDeclTable
    put (gSymbolTable, lSymbolTable, tTable)
    return lSymbolTable

doPDecl :: [(String, String)] -> State ParserState [(String, String)]
doPDecl params = do
    (gSymbolTable, _, tTable) <- get
    let argTable = genPSymbolTable params
    put (gSymbolTable, argTable, tTable)
    return params

data FDefinition = FDefinition {
    fName :: String,
    fType :: String,
    fSymbolTable :: SymbolTable,
    fAST :: Node
} deriving Show

fnDefTypeCheck :: String -> String -> [(String, String)] -> SymbolTable -> Node -> State ParserState FDefinition
fnDefTypeCheck t name params lSymbol lTree = do
    (gSymbolTable, _, tTable) <- get
    let fLookup = case Map.lookup name gSymbolTable of
         Just f -> f
         Nothing -> error $ "Function declaration of" ++ name ++ "is missing."
    put (gSymbolTable, empty, tTable)
    if funcType fLookup == t && funcParams fLookup == params then
        return FDefinition {fName=name, fType=t, fSymbolTable=lSymbol, fAST=lTree}
    else typeError $ "at declaration of " ++ name

nodeToType :: Node -> State ParserState String
nodeToType x = do
    (gSymbolTable, lSymbolTable, tTable) <- get
    case x of
         NodeOp {} -> return "int"
         NodeInt _ -> return "int"
         NodeStr _ -> return "str"
         NodeVar var -> return (varType $ Data.Maybe.fromMaybe (Data.Maybe.fromMaybe (argError var) (lookup var gSymbolTable)) (lookup var lSymbolTable))
         NodePtr (NodeVar var) -> return (varType $ Data.Maybe.fromMaybe (Data.Maybe.fromMaybe (argError var) (lookup var gSymbolTable)) (lookup var lSymbolTable))
         NodeFnCall var _ ->  return (funcType $ Data.Maybe.fromMaybe (Data.Maybe.fromMaybe (argError var) (lookup var gSymbolTable)) (lookup var lSymbolTable))
         NodeField var field -> do 
                                t <- nodeToType var
                                return (fieldType (typeFields (tTable ! t) ! field))
         s -> argError (show s)

fnCallTypeCheck :: String -> [Node] -> State ParserState [Node]
fnCallTypeCheck name args = do
    (gSymbolTable, _, _) <- get
    let fLookup = case Map.lookup name gSymbolTable of
         Just f -> f
         Nothing -> error $ "Function declaration of" ++ name ++ "is missing."
    params <- mapM nodeToType args
    if Prelude.map fst (funcParams fLookup) == params then return args
    else typeError $ "at calling of " ++ name

typeError errStr = error $ "Type mismatch " ++ errStr

argError errStr = error $ "Invalid argument: " ++ errStr