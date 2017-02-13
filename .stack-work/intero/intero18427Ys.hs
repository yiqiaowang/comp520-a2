module Main
  ( main
  ) where

import Backend (initSymTbl, typeCheckWrap, codeGenWrap)
import Parser (parseInput)
import Scanner (scanToken)
import AST (prettyPrint)

isValid :: Bool -> String
isValid True = "VALID"
isValid _ = "INVALID"

main = do
  input <- getContents
  let ast = (parseInput . scanToken) input
  let symtbl = initSymTbl ast
  if typeCheckWrap ast symtbl then putStrLn $ codeGenWrap ast symtbl else putStrLn $ isValid $ typeCheckWrap ast symtbl 
  -- putStrLn $ prettyPrint ast
