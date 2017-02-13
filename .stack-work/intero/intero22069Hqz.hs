-- All the compiler backend, ast passes, and code generation stuff goes here.
module Backend (initSymTbl) where

import Parser(parseInput)
import SymbolTable
import AST
import Data.Map (Map)
import qualified Data.Map as Map

-- Populate the symbol table, first creates a new one and then populates it
initSymTbl :: Prog -> Map IdName Entry
initSymTbl ast = popSymTbl ((parseDecl . extractDecl) ast) newMap

-- Extract the declarations from an "ast"
extractDecl :: Prog -> [Decl]
extractDecl (Prog d s) = d

type ParsedDecl = (IdName, Type)
-- Parse a decl list into something useful, returns a tuple
parseDecl :: [Decl] -> [ParsedDecl]
parseDecl [] = []
parseDecl (D_Decl (E_Iden s) t:ds) = (s,t) : parseDecl ds


-- Populate SymTbl from a list of declarations.
popSymTbl :: [ParsedDecl] -> Map IdName Entry -> Map IdName Entry
popSymTbl [] m = m
popSymTbl ((s, t):ds) m
  | hasKey s m = error "Key already present!"
  | not (hasKey s m) = popSymTbl ds (addSym s (Entry t ValEmpty) m)


