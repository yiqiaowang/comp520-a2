module Main(main) where

import Scanner (scanToken)
import Parser (parseInput)
import Backend (initSymTbl)
  
main = do
  input <- getContents
  let ast = (parseInput . scanToken) input
  let result = initSymTbl ast
  print $ show result
