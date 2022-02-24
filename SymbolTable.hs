module SymbolTable where

import Data.Map
import qualified Data.List as List

data SymbolFoundation = BaseVar String [Int]
                      | BaseFunc String [(String, String)] deriving Show

data Symbol = Variable {
                varType :: String,
                varSize :: [Int],
                varBinding :: Int
            }
            | Function {
                funcType :: String,
                funcParams :: [(String, String)],
                funcLabel :: Int
            }
            | LVariable {
                varType :: String,
                varLocalBinding :: Int
            }
            | Null deriving Show

type SymbolTable = Map String Symbol

gSymbolsBuild :: (String, [SymbolFoundation]) -> Int -> Int -> [String] -> ([(String, Symbol)], Int, Int)
gSymbolsBuild (_, []) addr fLabel tTable = ([], addr, fLabel)
gSymbolsBuild (t, BaseVar name size : xs) addr fLabel tTable = case t of
    "int" ->  ((name, Variable {varType=t, varSize=size, varBinding=addr}) : symbols, a, b)
    "str" ->  ((name, Variable {varType=t, varSize=size, varBinding=addr}) : symbols, a, b)
    t -> if isType then ((name, Variable {varType=t, varSize=size , varBinding=addr-product size}) : symbols, a, b)
    else ((name, Variable {varType=t, varSize=2:tail size, varBinding=addr-2}) : symbols2, a2, b2)
        where isType = t `elem` tTable
    where (symbols, a, b) = gSymbolsBuild (t, xs) (addr - product size) fLabel tTable
          (symbols2, a2, b2) = gSymbolsBuild (t, xs) (addr - 2) fLabel tTable
gSymbolsBuild (t, BaseFunc name params : xs) addr fLabel tTable = ((name, Function {funcType=t, funcParams=params, funcLabel=fLabel}) : symbols, a, b)
    where (symbols, a, b) = gSymbolsBuild (t, xs) addr (fLabel+1) tTable

lSymbolsBuild :: (String, [String]) -> [Int] -> [(String, Symbol)]
lSymbolsBuild (_, []) = const []
lSymbolsBuild (t, x : xs) = \p -> (x, LVariable {varType=t, varLocalBinding=head p}) : symbols (tail p)
    where symbols = lSymbolsBuild (t, xs)

genGSymbolTable :: [(String, [SymbolFoundation])] -> [String] -> Int -> (SymbolTable, Int, Int)
genGSymbolTable [] _ lab = (empty, -1, lab)
genGSymbolTable (x : xs) tTable lab = (unionWith nameError map1 map2, addr2, label2)
    where (map1, addr, label) = genGSymbolTable xs tTable lab
          (symbols, addr2, label2) = gSymbolsBuild x addr label tTable
          map2 = fromListWith nameError symbols

genLSymbolTable :: [(String, [String])] -> SymbolTable
genLSymbolTable [] = empty
genLSymbolTable (x : xs) = unionWith nameError map1 map2
    where map1 = genLSymbolTable xs
          symbols = lSymbolsBuild x
          map2 = fromListWith nameError (symbols [(size map1 + 1) ..])

genPSymbolTable :: [(String, String)] -> SymbolTable
genPSymbolTable [] = empty
genPSymbolTable params = fromListWith nameError (zipWith makeSymbolList params [startAddr ..])
    where startAddr = - 2 - length params
          makeSymbolList = \(t, name) p -> (name, LVariable {varType=t, varLocalBinding=p})

genCPSymbolTable :: [(String, String)] -> SymbolTable
genCPSymbolTable [] = empty
genCPSymbolTable params = fromListWith nameError (zipWith makeSymbolList params [startAddr ..])
    where startAddr = - 4 - length params
          makeSymbolList = \(t, name) p -> (name, LVariable {varType=t, varLocalBinding=p})

nameError = error "Variable declared multiple times."