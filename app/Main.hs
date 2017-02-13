module Main
  ( main
  ) where

import AST (prettyPrint, Prog)
import Backend (initSymTbl, typeCheckWrap, codeGen)
import Parser (parseInput)
import Scanner (scanToken)
import SymbolTable (toString, toList)
import System.Environment (getArgs)

isValid :: Bool -> String
isValid True = "VALID"
isValid _ = "INVALID"

parseWrapper :: String -> Prog
parseWrapper s = parseInput $! scanToken s

pPrint :: String -> [[Char]] -> IO()
pPrint s a = writeFile (concat a ++ ".pretty.min") s

main = do
  input <- getContents
  args <- getArgs
  let ast = parseWrapper input
  let symtbl = initSymTbl ast
  let pprint = (prettyPrint $! ast)
  if snd $! initSymTbl ast
    then do
      writeFile (concat args ++ ".symbol.txt") (toString $! toList $! fst symtbl)
      pPrint pprint args
      if (typeCheckWrap ast (fst symtbl))
        then writeFile (concat args ++ ".c") (codeGen ast (fst symtbl))
        else do
        error "Error: Failed to type check. Created (partial) symbol table and pretty printed file anways."
    else do
    error "Error: Failed to populate symbol table. Creating (partial) symbol table and pretty printed file anways."
    writeFile (concat args ++ ".symbol.txt") (show $! symtbl)
    pPrint pprint args
