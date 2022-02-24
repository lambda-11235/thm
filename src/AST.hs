
module AST where

data Statement = ExprS Expr
               | FuncDefS FuncDef

data FuncDef = FuncDef String [Maybe String] Expr
  deriving(Show)

data Expr = Let [FuncDef] Expr
          | Lambda [Maybe String] Expr
          | App Expr Expr
          | Var String
          | Fix
          | Unit
          | Num Int
          | S
          | NatCase
  deriving(Show)
