
module CST where

import qualified AST


data Statement = ExprS Expr
               | FuncDefS FuncDef
  deriving(Eq, Show)


data Type = FunType Type Type
          | TVar String
          | UnitType
          | NatType
  deriving(Eq, Show)

data FuncDef = FuncDef String Expr
  deriving(Eq, Show)

data Expr = Let FuncDef Expr
          | Lambda (Maybe String) Expr
          | App Expr Expr
          | Var String
          | Fix
          | Unit
          | Z
          | S
          | NatCase
  deriving(Eq, Show)


statementFromAST :: AST.Statement -> Statement
statementFromAST (AST.ExprS e) = ExprS (exprFromAST e)
statementFromAST (AST.FuncDefS fdef) = FuncDefS (fdefFromAST fdef)


fdefFromAST :: AST.FuncDef -> FuncDef
fdefFromAST (AST.FuncDef name [] expr) = FuncDef name (exprFromAST expr)
fdefFromAST (AST.FuncDef name vars expr) = FuncDef name (lambdify vars expr)

exprFromAST :: AST.Expr -> Expr
exprFromAST (AST.Let [] e) = exprFromAST e
exprFromAST (AST.Let (f:fs) e)
  = Let (fdefFromAST f) (exprFromAST $ AST.Let fs e) 
exprFromAST (AST.Lambda vs e) = lambdify vs e
exprFromAST (AST.App e1 e2) = App (exprFromAST e1) (exprFromAST e2)
exprFromAST (AST.Var name) = Var name
exprFromAST AST.Fix = Fix
exprFromAST AST.Unit = Unit
exprFromAST AST.Z = Z
exprFromAST AST.S = S
exprFromAST AST.NatCase = NatCase


lambdify [] expr = exprFromAST expr
lambdify (v:vs) expr = Lambda v (lambdify vs expr)


ppType :: Type -> String
ppType (FunType t1 t2) = ppFTLeft t1 ++ " -> " ++ ppType t2
ppType (TVar name) = "'" ++ name
ppType UnitType = "Unit"
ppType NatType = "N"

ppFTLeft :: Type -> String
ppFTLeft e@(FunType _ _) = "(" ++ ppType e ++ ")"
ppFTLeft e = ppType e


ppStatement :: Statement -> String
ppStatement (ExprS e) = ppExpr e
ppStatement (FuncDefS fdef) = ppFuncDef fdef

ppFuncDef :: FuncDef -> String
ppFuncDef (FuncDef name e) = name ++ " = " ++ ppExpr e ++ ";"

ppExpr :: Expr -> String
ppExpr (Let fdef e) = "let " ++ ppFuncDef fdef ++ " in " ++ ppExpr e
ppExpr (Lambda name e) =
  case name of
    Nothing -> "\\_. " ++ ppExpr e
    Just n -> "\\" ++ n ++ ". " ++ ppExpr e
ppExpr (App e1 e2) = ppAppLeft e1 ++ " " ++ ppAppRight e2
ppExpr (Var name) = name
ppExpr Fix = "fix"
ppExpr Unit = "unit"
ppExpr Z = "Z"
ppExpr S = "S"
ppExpr NatCase = "natCase"


ppAppLeft :: Expr -> String
ppAppLeft e@(Lambda _ _) = "(" ++ ppExpr e ++ ")"
ppAppLeft e = ppExpr e

ppAppRight :: Expr -> String
ppAppRight e@(App _ _) = "(" ++ ppExpr e ++ ")"
ppAppRight e@(Lambda _ _) = "(" ++ ppExpr e ++ ")"
ppAppRight e = ppExpr e
