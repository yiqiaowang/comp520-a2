module Main(main) where

import Scanner (scanToken)
import Parser (parseInput)
import Backend (initSymTbl
               ,typeCheckWrap)


isValid :: Bool -> String
isValid True = "VALID"
isValid _ = "INVALID"

main = do
  input <- getContents
  let ast = (parseInput . scanToken) input
  let symtbl = initSymTbl ast
  print $ isValid $ typeCheckWrap ast symtbl
