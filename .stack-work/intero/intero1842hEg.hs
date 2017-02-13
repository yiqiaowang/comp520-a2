-- All the compiler backend, ast passes, and code generation stuff goes here.
module Backend ( initSymTbl , typeCheckWrap , codeGenWrap
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
  | not (hasKey s m) = popSymTbl ds (addSym s (Entry t) m)

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
resolveType :: Op -> Type -> Type -> (Type, Bool)
resolveType _ T_Int T_Int = (T_Int, True)
resolveType _ T_Float T_Int = (T_Float, True)
resolveType _ T_Int T_Float = (T_Float, True)
resolveType Op_Add T_String T_String = (T_String, True)
resolveType Op_Mult T_Int T_String = (T_String, True)
resolveType Op_Mult T_String T_Int = (T_String, True)
resolveType _ _ _ = error "Type Resolve Error"

-- check to see if type is unary minus-able
unaryCompat :: Type -> Bool
unaryCompat T_Float = True
unaryCompat T_Int = True
unaryCompat T_String = False

assertInt :: Type -> Bool
assertInt T_Int = True
assertInt _ = False

assignCompat :: Type -> Type -> Bool
assignCompat T_Int T_Int = True
assignCompat T_Float T_Float = True
assignCompat T_Float T_Int = True
assignCompat T_String T_String = True
assignCompat _ _ = False
-- S_READ
typeCheck :: Map IdName Entry -> [Stmt] -> Bool
typeCheck m [] = True
typeCheck m (S_Read (E_Iden id):ss) = (hasKey id m) && (typeCheck m ss)
-- S_PRINT
typeCheck m (S_Print e:ss) = (snd $ typeCheckExpr m e) && (typeCheck m ss)
-- S_ASSIGN
typeCheck m (S_Assign a b:ss) =
  assignCompat (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b) && (typeCheck m ss)

-- S_if
typeCheck m (S_If a b (B_Else c):ss) =
  (snd $ typeCheckExpr m a) && (assertInt $ fst $ typeCheckExpr m a) &&
  (typeCheck m b) && (typeCheck m c) && (typeCheck m ss)
typeCheck m (S_If a b B_Endif:ss) =
  (snd $ typeCheckExpr m a) && (assertInt $ fst $ typeCheckExpr m a) &&
  (typeCheck m b) && (typeCheck m ss)
-- S_While
typeCheck m (S_While a b:ss) =
  (snd $ typeCheckExpr m a) && (assertInt $ fst $ typeCheckExpr m a) &&
  (typeCheck m b) && (typeCheck m ss)

-- Type Check Expr
typeCheckExpr :: Map IdName Entry -> Expr -> (Type, Bool)
typeCheckExpr m (E_Int _) = (T_Int, True)
typeCheckExpr m (E_Float _) = (T_Float, True)
typeCheckExpr m (E_String _) = (T_String, True)
typeCheckExpr m (E_Div a b)
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b)) =
    t
  | otherwise = error "Type Error"
  where
    t = resolveType Op_Div (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
typeCheckExpr m (E_Add a b)
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b)) =
    t
  | otherwise = error "Type Error"
  where
    t = resolveType Op_Add (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
typeCheckExpr m (E_Mult a b)
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b)) =
    t
  | otherwise = error "Type Error"
  where
    t = resolveType Op_Add (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
typeCheckExpr m (E_Sub a b)
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b)) =
    t
  | otherwise = error "Type Error"
  where
    t = resolveType Op_Add (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
typeCheckExpr m (E_UMinus a)
  | ((unaryCompat $ fst $ typeCheckExpr m a) && (snd $ typeCheckExpr m a)) =
    (fst $ typeCheckExpr m a, True)
  | otherwise = error "Type Error"
typeCheckExpr m (E_Iden a)
  | hasKey a m = (parseEntry $ removeJust $ getSym a m, True)
  | otherwise = error "Identifier not found"

typeCheckWrap :: Prog -> Map IdName Entry -> Bool
typeCheckWrap ast m = ((typeCheck m) . extractStmt) ast

 -- Code generation follows

codeGenWrap :: Prog -> Map IdName Entry -> String
codeGenWrap p m 
  | typeCheckWrap p m = codeGen p m
  | otherwise = error "You shouldn't be reading this tbqh." 
  
codeGen :: Prog -> Map IdName Entry -> String
codeGen (Prog d s) m = "#include <stdio.h>\nint main(void) {\n" ++  codeGenDeclWrap d ++ codeGenStmtWrap m s ++ "return 0;\n}"

typeToStr :: Type -> String
typeToStr T_Float = "float"
typeToStr T_Int = "int"
typeToStr T_String = "char"

strHelper :: Type -> String
strHelper T_String = "[1024]"
strHelper _ = ""


typeInitVal :: Type -> String
typeInitVal T_Int = "0"
typeInitVal T_Float = "0.0"
typeInitVal T_String = "\"\""

codeGenDecl :: Decl -> String
codeGenDecl (D_Decl (E_Iden i) t) = typeToStr t ++ " " ++ i ++ strHelper t ++ " = " ++ typeInitVal t ++ ";"

codeGenDeclWrap :: [Decl] -> String
codeGenDeclWrap [] = ""
codeGenDeclWrap (d:ds) = codeGenDecl d ++ "\n" ++ codeGenDeclWrap ds


getIdType :: IdName -> Map IdName Entry -> Type
getIdType i m
  | hasKey i m = parseEntry $ removeJust $ getSym i m
  | otherwise = error "Didn't find id in map"

getFormatter :: Type -> String
getFormatter T_Int = "%d"
getFormatter T_Float = "%f"
getFormatter T_String = "%s"

codeGenStmt :: Map IdName Entry -> Stmt -> String
codeGenStmt m (S_Read (E_Iden i)) = "scanf(\"" ++ (getFormatter $ getIdType i m) ++ "\",&" ++ i ++ ");"
codeGenStmt m (S_Print (E_Iden i)) = "printf(\"" ++ (getFormatter $ getIdType i m) ++ "\"," ++ i ++ ");"
codeGenStmt m (S_Assign (E_Iden i) e) = i ++ " = " ++ codeGenExpr  e ++ ";"
codeGenStmt m (S_If a b B_Endif) = "if (" ++ codeGenExpr  a ++ ") {" ++ codeGenStmtWrap m b ++ "};"
codeGenStmt m (S_If a b (B_Else c)) = "if (" ++ codeGenExpr  a ++ ") {" ++ codeGenStmtWrap m b ++ "} else {" ++ codeGenStmtWrap m c ++ "};"
codeGenStmt m (S_While a b) = "while (" ++ codeGenExpr  a ++ ") {" ++ codeGenStmtWrap m b ++ "};"

codeGenStmtWrap :: Map IdName Entry -> [Stmt] -> String
codeGenStmtWrap _ [] = ""
codeGenStmtWrap m (s:ss) = codeGenStmt m s ++ "\n" ++  codeGenStmtWrap m ss

codeGenExpr :: Expr -> String
codeGenExpr  (E_Paren e) = "(" ++ codeGenExpr  e ++ ")"
codeGenExpr  (E_UMinus e) = "-(" ++ codeGenExpr  e ++ ")"
codeGenExpr  (E_Add a b) = "(" ++ codeGenExpr  a ++ " + " ++ codeGenExpr  b ++ ")"
codeGenExpr  (E_Sub a b) = "(" ++ codeGenExpr  a ++ " - " ++ codeGenExpr  b ++ ")"
codeGenExpr  (E_Mult a b) = "(" ++ codeGenExpr  a ++ " * " ++ codeGenExpr  b ++ ")"
codeGenExpr  (E_Div a b) = "(" ++ codeGenExpr  a ++ " / " ++ codeGenExpr  b ++ ")"
codeGenExpr  (E_Int a) = show a
codeGenExpr  (E_Float a) = show a
codeGenExpr  (E_String a) = a
codeGenExpr  (E_Iden a) = a
