module SymbolTable where

import Data.Map

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

gSymbolsBuild :: (String, [SymbolFoundation]) -> Int -> Int -> ([(String, Symbol)], Int, Int)
gSymbolsBuild (_, []) addr fLabel = ([], addr, fLabel)
gSymbolsBuild (t, BaseVar name size : xs) addr fLabel = ((name, Variable {varType=t, varSize=size, varBinding=addr}) : symbols, a, b)
    where (symbols, a, b) = gSymbolsBuild (t, xs) (addr + product size) fLabel
gSymbolsBuild (t, BaseFunc name params : xs) addr fLabel = ((name, Function {funcType=t, funcParams=params, funcLabel=fLabel}) : symbols, a, b)
    where (symbols, a, b) = gSymbolsBuild (t, xs) addr (fLabel+1)

lSymbolsBuild :: (String, [String]) -> [Int] -> [(String, Symbol)]
lSymbolsBuild (_, []) = const []
lSymbolsBuild (t, x : xs) = \p -> (x, LVariable {varType=t, varLocalBinding=head p}) : symbols (tail p)
    where symbols = lSymbolsBuild (t, xs)

genGSymbolTable :: [(String, [SymbolFoundation])] -> (SymbolTable, Int, Int)
genGSymbolTable [] = (empty, 4096, 0)
genGSymbolTable (x : xs) = (unionWith nameError map1 map2, addr2, label2)
    where (map1, addr, label) = genGSymbolTable xs
          (symbols, addr2, label2) = gSymbolsBuild x addr label
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
    where startAddr = - 3 - length params
          makeSymbolList = \(t, name) p -> (name, LVariable {varType=t, varLocalBinding=p})

nameError = error "Variable declared multiple times."