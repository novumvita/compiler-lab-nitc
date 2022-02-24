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
    classMethods :: SymbolTable,
    classFunOffsets :: [String]
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

classBuild :: ((String, String), [(String, String)],  [(String, [SymbolFoundation])]) ->  Maybe Class -> [String] -> Int -> ((String, Class), Int)
classBuild ((cname, parent), vdecls, fdecls) parClass tTable lab = ((cname, Class { classParent=parent, classFields=fieldlist `union` parentfieldlist, classMethods=flist `union` parentfunctionlist, classFunOffsets=[]}), newlab)
    where fieldlist = fromListWith (error "Field declared multiple times.") (zipWith fieldsBuild vdecls [startIndex ..])
          (flist, _, newlab) = genGSymbolTable fdecls tTable lab
          parentfieldlist = maybe empty classFields parClass
          parentfunctionlist = maybe empty classMethods parClass
          startIndex = length parentfunctionlist