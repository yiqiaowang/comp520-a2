-- All the compiler backend, ast passes, and code generation stuff goes here.
module Backend
  ( initSymTbl
  , typeCheckWrap
  ) where

import AST
import Data.Map (Map)
import qualified Data.Map as Map
import Parser (parseInput)
import SymbolTable

-- Populate the symbol table, first creates a new one and then populates it
initSymTbl :: Prog -> Map IdName Entry
initSymTbl ast = popSymTbl ((parseDecl . extractDecl) ast) newMap

-- Extract the declarations from an ast
extractDecl :: Prog -> [Decl]
extractDecl (Prog d s) = d

type ParsedDecl = (IdName, Type)

-- Parse a decl list into something useful, returns a tuple
parseDecl :: [Decl] -> [ParsedDecl]
parseDecl [] = []
parseDecl (D_Decl (E_Iden s) t:ds) = (s, t) : parseDecl ds

-- Populate SymTbl from a list of declarations.
popSymTbl :: [ParsedDecl] -> Map IdName Entry -> Map IdName Entry
popSymTbl [] m = m
popSymTbl ((s, t):ds) m
  | hasKey s m = error "Key already present!"
  | not (hasKey s m) = popSymTbl ds (addSym s (Entry t ValEmpty) m)

-- Extract the statements from an ast
extractStmt :: Prog -> [Stmt]
extractStmt (Prog d s) = s

-- Type Checking.
-- type check each statement in a list of statements,
data Op
  = Op_Add
  | Op_Sub
  | Op_Div
  | Op_Mult
  | Op_UMin

-- Resolve types
resolveType :: Op -> Type -> Type -> Type
resolveType _ T_Int T_Int = T_Int
resolveType _ T_Float T_Int = T_Float
resolveType _ T_Int T_Float = T_Float
resolveType Op_Add T_String T_String = T_String
resolveType Op_Mult T_Int T_String = T_String
resolveType Op_Mult T_String T_Int = T_String
resolveType _ _ _ = error "Type Resolve Error"

-- Check if type is UNary Minusable
unaryCompat :: Type -> Bool
unaryCompat T_Float = True
unaryCompat T_Int = True

unaryCompay T_String = False

elseBlock :: ElseBlock -> Bool
elseBlock B_Endif = False
elseBlock (B_Else _) = True

extractElse :: ElseBlock -> [Stmt]
extractElse (B_Else ss) = ss
extractElse B_Endif = []

validLit :: Type -> Bool
validLit T_Float = True
validLit T_String = True
validLit T_Int = True

-- S_READ
typeCheck :: Map IdName Entry -> [Stmt] -> Bool
typeCheck m [] = True
typeCheck m (S_Read (E_Iden id):ss) = (hasKey id m) && (typeCheck m ss)
-- S_PRINT
typeCheck m (S_Print e:ss) = (snd $ typeCheckExpr m e) && (typeCheck m ss)
-- S_ASSIGN
typeCheck m (S_Assign a b:ss) =
  ((fst $ typeCheckExpr m a) == (fst $ typeCheckExpr m b)) && (typeCheck m ss)
-- S_if
typeCheck m (S_If a b (B_Else c):ss) = (snd $ typeCheckExpr m a) &&
    (typeCheck m b) && (typeCheck m c) && (typeCheck m ss)
typeCheck m (S_If a b B_Endif : ss) = (snd $ typeCheckExpr m a) && (typeCheck m b) && (typeCheck m ss)


-- S_While
typeCheck m (S_While a b:ss) =
  (snd $ typeCheckExpr m a) && (typeCheck m b) && (typeCheck m ss)

-- Type Check Expr
typeCheckExpr :: Map IdName Entry -> Expr -> (Type, Bool)
typeCheckExpr m (E_Int _) = (T_Int, True)
typeCheckExpr m (E_Float _) = (T_Float, True)
typeCheckExpr m (E_String _) = (T_String, True)
typeCheckExpr m (E_Div a b) 
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b) && (validLit t)) = ( t, True)
  | otherwise = error "Type Error"
  where t = resolveType Op_Div (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
typeCheckExpr m (E_Add a b)
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b) && (validLit t)) = ( t, True)
  | otherwise = error "Type Error"
  where t = resolveType Op_Add (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
typeCheckExpr m (E_Mult a b)
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b) && (validLit t)) =
    ( t
    , True)
  | otherwise = error "Type Error"
  where t = resolveType Op_Add (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
typeCheckExpr m (E_Sub a b)
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b) && (validLit t)) =
    ( t
    , True)
  | otherwise = error "Type Error"
  where t = resolveType Op_Add (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
typeCheckExpr m (E_UMinus a)
  | ((unaryCompat $ fst $ typeCheckExpr m a) && (snd $ typeCheckExpr m a)) =
    (fst $ typeCheckExpr m a, True)
  | otherwise = error "Type Error"
typeCheckExpr m (E_Iden a)
  | hasKey a m = (fst $ parseEntry $ removeJust $ getSym a m, True)
  | otherwise = error "Identifier not found"

typeCheckWrap :: Prog -> Map IdName Entry -> Bool
typeCheckWrap ast m = ((typeCheck m) . extractStmt) ast
