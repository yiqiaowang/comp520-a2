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
  -- putStrLn $ show $ symtbl

  if (snd $ initSymTbl ast) then

    if (typeCheckWrap ast (fst symtbl)) then
      putStrLn $ codeGenWrap ast (fst symtbl) 
    else
      putStrLn "Errors."

  else putStrLn "couldn't even build symbolTable."
