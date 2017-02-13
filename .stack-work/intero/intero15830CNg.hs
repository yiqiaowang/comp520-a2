module Main
  ( main
  ) where

import AST (prettyPrint)
import Backend (initSymTbl, typeCheckWrap, codeGenWrap)
import Parser (parseInput)
import Scanner (scanToken)
import SymbolTable (toString, toList)
import System.Environment (getArgs)

isValid :: Bool -> String
isValid True = "VALID"
isValid _ = "INVALID"

main = do
  input <- getContents
  args <- getArgs
  let ast = (parseInput . scanToken) input
  let symtbl = initSymTbl ast
  let pprint = (prettyPrint $! ast)
  -- writeFile (concat args ++ ".pretty.min") pprint
  if snd $! initSymTbl ast
    then do
      writeFile (concat args ++ ".symbol.txt") (toString $! toList $! fst symtbl)
      if (typeCheckWrap ast (fst symtbl))
        then writeFile (concat args ++ ".c") (codeGenWrap ast (fst symtbl))
        else error "Error"
    else writeFile (concat args ++ ".symbol.txt") (show symtbl)
