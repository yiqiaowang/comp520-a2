module Main
  ( main
  ) where

import Backend (initSymTbl, typeCheckWrap)
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
  putStrLn $ show $ symtbl
  putStrLn $ isValid $ typeCheckWrap ast symtbl
  -- putStrLn $ prettyPrint ast
