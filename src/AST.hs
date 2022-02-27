
module AST where

data Statement = TypeDefS TypeDef
               | FuncDefS FuncDef
  deriving(Show)


data TypeDef = TypeDef String [String] [ConsDef]
  deriving(Show)
data ConsDef = ConsDef String [Type]
  deriving(Show)


data Type = FunType Type Type
          | TyVar String
          | TyCons String [Type]
  deriving(Show)

-- Bool indicates if function is fixed
data FuncDef = FuncDef Bool String [Maybe String] Expr
  deriving(Show)

data Expr = Let [FuncDef] Expr
          | Lambda [Maybe String] Expr
          | App Expr Expr
          | Var String
          | Case String
          | Fix
  deriving(Show)
