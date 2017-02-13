module AST
  ( Prog(..)
  , Type(..)
  , Decl(..)
  , Stmt(..)
  , ElseBlock(..)
  , Expr(..)
  ) where

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
  deriving (Show, Eq) -- Slightly Dirty

data Stmt
  = S_Read Expr -- Slightly Dirty
  | S_Print Expr
  | S_Assign Expr
             Expr -- Slightly Dirty
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
  = E_UMinus Expr
  | E_Add Expr
          Expr
  | E_Sub Expr
          Expr
  | E_Mult Expr
           Expr
  | E_Div Expr
          Expr
  | E_Int Integer
  | E_Float Float
  | E_Iden String
  | E_String String
  deriving (Show, Eq)
