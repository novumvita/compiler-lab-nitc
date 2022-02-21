module TypeTable where

import SymbolTable
import SyntaxTree
import Data.Map
import Data.Maybe

data Type = Type {
    typeSize :: Int,
    typeFields :: FieldTable
} deriving Show

data Field = Field {
    fieldIndex :: Int,
    fieldType :: String

} deriving Show

data Class = Class {
    classParent :: String,
    classFields :: FieldTable,
    classMethods :: SymbolTable
} deriving Show

type FieldTable = Map String Field

type TypeTable = Map String Type

type ClassTable = Map String Class

data FDefinition = FDefinition {
    fName :: String,
    fType :: String,
    fSymbolTable :: SymbolTable,
    fAST :: Node,
    fClass :: String
} deriving Show

genTTable :: [(String, [(String, String)])] -> TypeTable 
genTTable [] = empty 
genTTable (x : xs) = unionWith (error "Type declared multiple times.") map1 map2
    where map1 = genTTable xs
          map2 = typeBuild x

typeBuild :: (String, [(String, String)]) -> TypeTable
typeBuild (tname, fields) = Data.Map.singleton tname Type {typeSize=length fieldlist, typeFields=fieldlist}
    where fieldlist = fromListWith (error "Field declared multiple times.") (zipWith fieldsBuild fields [0..])

fieldsBuild :: (String, String) -> Int -> (String, Field)
fieldsBuild (fname, ftype) findex = (fname, Field {fieldIndex=findex, fieldType=ftype})

genCTable :: [((String, String), [(String, String)], [(String, [SymbolFoundation])])] -> ClassTable
genCTable [] = empty 
genCTable (x : xs) = unionWith (error "Class declared multiple times.") map1 map2
    where map1 = genCTable xs 
          map2 = classBuild x

classBuild :: ((String, String), [(String, String)],  [(String, [SymbolFoundation])]) -> ClassTable
classBuild ((cname, parent), vdecls, fdecls) = Data.Map.singleton cname Class { classParent=parent, classFields=fieldlist, classMethods=functionlist}
    where fieldlist = fromListWith (error "Field declared multiple times.") (zipWith fieldsBuild vdecls [0..])
          functionlist = fromList (zipWith3 (\name pos sym-> (name, sym))(keys flist) [0..] (elems flist))
          (flist, _, _) = genGSymbolTable fdecls