module TypeTable where

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

type FieldTable = Map String Field

type TypeTable = Map String Type

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