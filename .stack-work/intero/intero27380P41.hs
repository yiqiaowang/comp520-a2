module Main(main) where

import Scanner (scanToken)
import Parser (parseInput)

  
main = do
  input <- getContents
  putStrLn ((show . parseInput . scanToken) input)
