{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module ParserState where
import Control.Monad.State
import SyntaxTree
import SymbolTable
import Data.Map as Map
import qualified Data.Maybe
import Prelude hiding (lookup)

type ParserState = (SymbolTable, SymbolTable)

startState = (empty, empty)

doGDecl :: [(String, [SymbolFoundation])] -> State ParserState Int
doGDecl decls = do
    let (gSymbolTable, sp, _) = genGSymbolTable decls
    put (gSymbolTable, empty)
    return sp

doLDecl :: [(String, [String])] -> State ParserState SymbolTable
doLDecl decls = do
    (gSymbolTable, lArgTable) <- get
    let lDeclTable = genLSymbolTable decls
    let lSymbolTable = Map.unionWith (error "Arg and local var share name.") lArgTable lDeclTable 
    put (gSymbolTable, lSymbolTable)
    return lSymbolTable

doPDecl :: [(String, String)] -> State ParserState [(String, String)]
doPDecl params = do
    (gSymbolTable, _) <- get 
    let argTable = genPSymbolTable params
    put (gSymbolTable, argTable)
    return params

data FDefinition = FDefinition {
    fName :: String,
    fType :: String,
    fSymbolTable :: SymbolTable,
    fAST :: Node
} deriving Show

fnDefTypeCheck :: String -> String -> [(String, String)] -> SymbolTable -> Node -> State ParserState FDefinition
fnDefTypeCheck t name params lSymbol lTree = do
    (gSymbolTable, _) <- get
    let fLookup = case Map.lookup name gSymbolTable of
         Just f -> f
         Nothing -> error $ "Function declaration of" ++ name ++ "is missing."
    put (gSymbolTable, empty)
    if funcType fLookup == t && funcParams fLookup == params then
        return FDefinition {fName=name, fType=t, fSymbolTable=lSymbol, fAST=lTree}
    else typeError $ "at declaration of " ++ name

nodesToTypes :: [Node] -> State ParserState [String]
nodesToTypes [] = return []
nodesToTypes (x : xs) = do
    (gSymbolTable, lSymbolTable) <- get
    rest <- nodesToTypes xs
    case x of
         NodeOp {} -> return ("int" : rest)
         NodeInt _ -> return ("int" : rest)
         NodeStr _ -> return ("str" : rest)
         NodeVar var -> return ((varType $ Data.Maybe.fromMaybe (Data.Maybe.fromMaybe (argError var) (lookup var gSymbolTable)) (lookup var lSymbolTable)) : rest)
         NodePtr (NodeVar var) -> return ((varType $ Data.Maybe.fromMaybe (Data.Maybe.fromMaybe (argError var) (lookup var gSymbolTable)) (lookup var lSymbolTable)) : rest)
         NodeFnCall var _ ->  return ((funcType $ Data.Maybe.fromMaybe (Data.Maybe.fromMaybe (argError var) (lookup var gSymbolTable)) (lookup var lSymbolTable)) : rest)
         s -> argError (show s)

fnCallTypeCheck :: String -> [Node] -> State ParserState [Node]
fnCallTypeCheck name args = do
    (gSymbolTable, _) <- get
    let fLookup = case Map.lookup name gSymbolTable of
         Just f -> f
         Nothing -> error $ "Function declaration of" ++ name ++ "is missing."
    params <- nodesToTypes args
    if Prelude.map fst (funcParams fLookup) == params then return args
    else typeError $ "at calling of " ++ name

typeError errStr = error $ "Type mismatch at" ++ errStr

argError errStr = error $ "Invalid argument: " ++ errStr