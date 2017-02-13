-- All the compiler backend, ast passes, and code generation stuff goes here.
module Backend
  ( initSymTbl
  , typeCheckWrap
  , codeGenWrap
  ) where

import AST
import Data.Map (Map)
import qualified Data.Map as Map
import Parser (parseInput)
import SymbolTable

-- Populate the symbol table, first creates a new one and then populates it
initSymTbl :: Prog -> (Map IdName Entry, Bool)
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
popSymTbl :: [ParsedDecl] -> Map IdName Entry -> (Map IdName Entry, Bool)
popSymTbl [] m = (m, True)
popSymTbl ((s, t):ds) m
  | hasKey s m = (m, False)
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
resolveType _ T_Float T_Float = (T_Float, True)
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

extractIden :: Expr -> String
extractIden (E_Iden i) = i
extractIden _ = error "Tried to extractIden from non E_Iden node"

-- S_READ
typeCheck :: Map IdName Entry -> [Stmt] -> Bool
typeCheck m [] = True
typeCheck m (S_Read (E_Iden id):ss) = (hasKey id m) && (typeCheck m ss)
-- S_PRINT
typeCheck m (S_Print e:ss) = (snd $ typeCheckExpr m e) && (typeCheck m ss)
-- S_ASSIGN
typeCheck m (S_Assign a b:ss) =
  assignCompat (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b) &&
  (typeCheck m ss)
-- S_if
typeCheck m (S_If a b (B_Else c):ss) =
  (snd $ typeCheckExpr m a) &&
  (assertInt $ fst $ typeCheckExpr m a) &&
  (typeCheck m b) && (typeCheck m c) && (typeCheck m ss)
typeCheck m (S_If a b B_Endif:ss) =
  (snd $ typeCheckExpr m a) &&
  (assertInt $ fst $ typeCheckExpr m a) && (typeCheck m b) && (typeCheck m ss)
-- S_While
typeCheck m (S_While a b:ss) =
  (snd $ typeCheckExpr m a) &&
  (assertInt $ fst $ typeCheckExpr m a) && (typeCheck m b) && (typeCheck m ss)

-- Type Check Expr
typeCheckExpr :: Map IdName Entry -> Expr -> (Type, Bool)
typeCheckExpr m (E_Int _) = (T_Int, True)
typeCheckExpr m (E_Float _) = (T_Float, True)
typeCheckExpr m (E_String _) = (T_String, True)
typeCheckExpr m (E_Div a b)
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b)) = t
  | otherwise = error "Type Error"
  where
    t = resolveType Op_Div (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
typeCheckExpr m (E_Add a b)
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b)) = t
  | otherwise = error "Type Error"
  where
    t = resolveType Op_Add (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
typeCheckExpr m (E_Mult a b)
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b)) = t
  | otherwise = error "Type Error"
  where
    t = resolveType Op_Mult (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
typeCheckExpr m (E_Sub a b)
  | ((snd $ typeCheckExpr m a) && (snd $ typeCheckExpr m b)) = t
  | otherwise = error "Type Error"
  where
    t = resolveType Op_Sub (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b)
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

preAmble =
  "char* concat(char *str1, char *str2){\
	   \char* str3 = (char *) malloc(1 + strlen(str1)+ strlen(str2) );\
	   \strcpy(str3, str1);\
           \strcat(str3, str2);\
           \return str3;}\
           \char* str_mult(char *str1, int n){\
	   \char* str2 = (char *) malloc(1 + n*strlen(str1));\
	   \for (int i = 0; i < n; i++){\
	   \strcat(str2, str1);\
	   \}\
	   \return str2;\
           \}"

codeGen :: Prog -> Map IdName Entry -> String
codeGen (Prog d s) m =
  "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n" ++
  preAmble ++
  "\nint main(void) {\n" ++
  codeGenDeclWrap d ++ codeGenStmtWrap m s ++ "return 0;\n}"

codeGenDecl :: Decl -> String
codeGenDecl (D_Decl (E_Iden i) t)
  | (t == T_Float) = "float " ++ i ++ " = 0.0;"
  | (t == T_Int) = "int " ++ i ++ " = 0;"
  | (t == T_String) =
    "char* " ++ i ++ " = malloc(1024); strcpy(" ++ i ++ ",\"\");"

codeGenDeclWrap :: [Decl] -> String
codeGenDeclWrap [] = ""
codeGenDeclWrap (d:ds) = codeGenDecl d ++ "\n" ++ codeGenDeclWrap ds

getIdType :: IdName -> Map IdName Entry -> Type
getIdType i m
  | hasKey i m = parseEntry $ removeJust $ getSym i m
  | otherwise = error "Didn't find id in map"

isIden :: Expr -> Bool
isIden (E_Iden _) = True
isIden _ = False

codeGenStmt :: Map IdName Entry -> Stmt -> String
codeGenStmt m (S_Read (E_Iden i))
  | (t == T_String) = "scanf(\"%s\"," ++ i ++ ");"
  | (t == T_Int) = "scanf(\"%d\",&" ++ i ++ ");"
  | (t == T_Float) = "scanf(\"%f\",&" ++ i ++ ");"
  where
    t = (getIdType i m)
codeGenStmt m (S_Print e)
  | (t == T_String) = "printf(\"%s\"," ++ codeGenExpr m e ++ ");"
  | (t == T_Int) = "printf(\"%d\"," ++ codeGenExpr m e ++ ");"
  | (t == T_Float) = "printf(\"%f\"," ++ codeGenExpr m e ++ ");"
  where
    t = fst $ typeCheckExpr m e
codeGenStmt m (S_Assign (E_Iden i) e)
  | t == T_String && (not $ isIden e) = "strcpy(" ++ i ++ "," ++ codeGenExpr m e ++ ");"
  | otherwise = i ++ " = " ++ codeGenExpr m e ++ ";"
  where
    t = (getIdType i m)
codeGenStmt m (S_If a b B_Endif) =
  "if (" ++ codeGenExpr m a ++ ") {" ++ codeGenStmtWrap m b ++ "};"
codeGenStmt m (S_If a b (B_Else c)) =
  "if (" ++
  codeGenExpr m a ++
  ") {" ++ codeGenStmtWrap m b ++ "} else {" ++ codeGenStmtWrap m c ++ "};"
codeGenStmt m (S_While a b) =
  "while (" ++ codeGenExpr m a ++ ") {" ++ codeGenStmtWrap m b ++ "};"

codeGenStmtWrap :: Map IdName Entry -> [Stmt] -> String
codeGenStmtWrap _ [] = ""
codeGenStmtWrap m (s:ss) = codeGenStmt m s ++ "\n" ++ codeGenStmtWrap m ss

codeGenExpr :: Map IdName Entry -> Expr -> String
codeGenExpr m (E_Paren e) = "(" ++ codeGenExpr m e ++ ")"
codeGenExpr m (E_UMinus e) = "-(" ++ codeGenExpr m e ++ ")"
codeGenExpr m (E_Add a b)
  | (t == T_String) =
    "(concat(" ++ codeGenExpr m a ++ "," ++ codeGenExpr m b ++ "))"
  | otherwise = "(" ++ codeGenExpr m a ++ " + " ++ codeGenExpr m b ++ ")"
  where
    t =
      (fst $
       resolveType Op_Add (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b))
codeGenExpr m (E_Sub a b) =
  "(" ++ codeGenExpr m a ++ " - " ++ codeGenExpr m b ++ ")"
codeGenExpr m (E_Mult a b)
  | (t == T_String) && (x == T_Int) && (y == T_String) =
    "(str_mult(" ++ codeGenExpr m b ++ "," ++ codeGenExpr m a ++ "))"
  | (t == T_String) && (x == T_String) && (y == T_Int) =
    "(str_mult(" ++ codeGenExpr m a ++ "," ++ codeGenExpr m b ++ "))"
  | otherwise = "(" ++ codeGenExpr m a ++ " * " ++ codeGenExpr m b ++ ")"
  where
    t =
      (fst $
       resolveType Op_Mult (fst $ typeCheckExpr m a) (fst $ typeCheckExpr m b))
    x = (fst $ typeCheckExpr m a)
    y = (fst $ typeCheckExpr m b)
codeGenExpr m (E_Div a b) =
  "(" ++ codeGenExpr m a ++ " / " ++ codeGenExpr m b ++ ")"
codeGenExpr _ (E_Int a) = show a
codeGenExpr _ (E_Float a) = show a
codeGenExpr _ (E_String a) = a
codeGenExpr _ (E_Iden a) = a
