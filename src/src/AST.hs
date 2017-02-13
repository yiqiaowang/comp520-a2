-- AST Data Structures and Pretty Printer
module AST
  ( Prog(..)
  , Type(..)
  , Decl(..)
  , Stmt(..)
  , ElseBlock(..)
  , Expr(..)
  , prettyPrint
  ) where

import Text.Printf

data Prog =
  Prog [Decl]
       [Stmt]
  deriving (Show, Eq)

data Type
  = T_Int
  | T_Float
  | T_String
  deriving (Show, Eq)

data Decl =
  D_Decl Expr
         Type
  deriving (Show, Eq) 

data Stmt
  = S_Read Expr
  | S_Print Expr
  | S_Assign Expr
             Expr
  | S_If Expr
         [Stmt]
         ElseBlock
  | S_While Expr
            [Stmt]
  deriving (Show, Eq)

data ElseBlock
  = B_Endif
  | B_Else [Stmt]
  deriving (Show, Eq)

data Expr
  = E_Paren Expr
  | E_UMinus Expr
  | E_Add Expr
          Expr
  | E_Sub Expr
          Expr
  | E_Mult Expr
           Expr
  | E_Div Expr
          Expr
  | E_Int Integer
  | E_Float Double
  | E_Iden String
  | E_String String
  deriving (Show, Eq)

duplicate :: Int -> String -> String
duplicate n str = [1..n] >>= const str

ppResolveType :: Type -> String
ppResolveType T_Float = "float"
ppResolveType T_Int = "int"
ppResolveType T_String = "string"


ppDecls :: Int -> [Decl] -> String
ppDecls _ [] = ""
ppDecls x (D_Decl (E_Iden id) t : ds) = (duplicate x "\t") ++  "var " ++ id ++ " : " ++ ppResolveType t ++ ";\n" ++ ppDecls x ds 

ppStmt :: Int -> Stmt -> String
ppStmt x (S_Read e) = (duplicate x "\t") ++ "read " ++ ppExpr e True ++ ";"
ppStmt x (S_Print e) = (duplicate x "\t") ++ "print " ++ ppExpr e True ++ ";"
ppStmt x (S_Assign a b) = (duplicate x "\t") ++ ppExpr a True ++ " = " ++ ppExpr b True ++ ";"
ppStmt x (S_If e s b) = (duplicate x "\t") ++ "if " ++ ppExpr e True ++ " then\n" ++ ppStmts (x+1) s ++ ppElseBlock x b
ppStmt x (S_While e s) = (duplicate x "\t") ++ "while " ++ ppExpr e True ++ " do\n" ++ ppStmts (x+1) s ++ "done"

ppElseBlock :: Int -> ElseBlock -> String
ppElseBlock x B_Endif = (duplicate x "\t") ++ "endif"
ppElseBlock x (B_Else s) = (duplicate x "\t") ++ "else\n" ++ ppStmts (x+1) s ++ (duplicate x "\t") ++  "endif"


ppStmts :: Int -> [Stmt] -> String
ppStmts _ [] = ""
ppStmts x (s:ss) = ppStmt x s ++ "\n" ++ ppStmts x ss

ppExpr :: Expr -> Bool -> String
ppExpr (E_Paren e) True = "(" ++ ppExpr e False ++ ")"
ppExpr (E_Paren e) False = ppExpr e True
ppExpr (E_UMinus e) True = "-(" ++ ppExpr e False ++ ")"
ppExpr (E_UMinus e) False = "-" ++ ppExpr e True
ppExpr (E_Add a b) True = "(" ++ ppExpr a True ++ "+" ++ ppExpr b True ++ ")"
ppExpr (E_Add a b) False = ppExpr a True ++ "+" ++ ppExpr b True
ppExpr (E_Sub a b) True = "(" ++ ppExpr a True ++ "-" ++ ppExpr b True ++ ")"
ppExpr (E_Sub a b) False = ppExpr a True ++ "-" ++ ppExpr b True
ppExpr (E_Mult a b) True = "(" ++ ppExpr a True ++ "*" ++ ppExpr b True ++ ")"
ppExpr (E_Mult a b) False = ppExpr a True ++ "*" ++ ppExpr b True
ppExpr (E_Div a b) True = "(" ++ ppExpr a True ++ "/" ++ ppExpr b True ++ ")"
ppExpr (E_Div a b) False = ppExpr a True ++ "/" ++ ppExpr b True
ppExpr (E_Int a) _ = show a
ppExpr (E_Float a) _ = printf "%f" a
ppExpr (E_Iden a) _ = a
ppExpr (E_String a) _ = a

prettyPrint :: Prog -> String
prettyPrint (Prog ds ss) = ppDecls 0 ds ++ ppStmts 0 ss
