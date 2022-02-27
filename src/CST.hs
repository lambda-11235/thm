
module CST where

import qualified Data.Set as S

import qualified AST


data Statement = TypeDefS TypeDef
               | FuncDefS FuncDef
  deriving(Eq, Show)


data TypeDef = TypeDef String [String] [ConsDef]
  deriving(Eq, Show)
data ConsDef = ConsDef String [Type]
  deriving(Eq, Show)


data Type = FunType Type Type
          | TyVar String
          | TyCons String [Type]
  deriving(Eq, Show)

data FuncDef = FuncDef String Expr
  deriving(Eq, Show)

data Expr = Let FuncDef Expr
          | Lambda (Maybe String) Expr
          | App Expr Expr
          | Var String
          | Case String
          | Fix
  deriving(Eq, Show)


statementFromAST :: AST.Statement -> Statement
statementFromAST (AST.TypeDefS tdef) = TypeDefS (tdefFromAST tdef)
statementFromAST (AST.FuncDefS fdef) = FuncDefS (fdefFromAST fdef)


tdefFromAST :: AST.TypeDef -> TypeDef
tdefFromAST (AST.TypeDef name args cdefs) =
  TypeDef name args (map cdefFromAST cdefs)

cdefFromAST :: AST.ConsDef -> ConsDef
cdefFromAST (AST.ConsDef name targs) =
  ConsDef name (map typeFromAST targs)


typeFromAST :: AST.Type -> Type
typeFromAST (AST.FunType t1 t2) =
  FunType (typeFromAST t1) (typeFromAST t2)
typeFromAST (AST.TyVar name) = TyVar name
typeFromAST (AST.TyCons name targs) =
  TyCons name (map typeFromAST targs)


fdefFromAST :: AST.FuncDef -> FuncDef
fdefFromAST (AST.FuncDef False name vars expr) =
  FuncDef name (lambdify vars expr)
fdefFromAST (AST.FuncDef True name vars expr) =
  FuncDef name (App Fix (Lambda (Just name) (lambdify vars expr)))

exprFromAST :: AST.Expr -> Expr
exprFromAST (AST.Let [] e) = exprFromAST e
exprFromAST (AST.Let (f:fs) e)
  = Let (fdefFromAST f) (exprFromAST $ AST.Let fs e)
exprFromAST (AST.Lambda vs e) = lambdify vs e
exprFromAST (AST.App e1 e2) = App (exprFromAST e1) (exprFromAST e2)
exprFromAST (AST.Var name) = Var name
exprFromAST (AST.Case name) = Case name
exprFromAST AST.Fix = Fix


lambdify [] expr = exprFromAST expr
lambdify (v:vs) expr = Lambda v (lambdify vs expr)


ppStatement :: Statement -> String
ppStatement (TypeDefS tdef) = ppTypeDef tdef
ppStatement (FuncDefS fdef) = ppFuncDef fdef


ppTypeDef :: TypeDef -> String
ppTypeDef (TypeDef name targs cdefs) =
  ("type " ++ name ++ (concatMap (\t -> " '" ++ t) targs) ++ " [" ++
   (concatMap (\cdef -> " " ++ ppConsDef cdef) cdefs)
   ++ " ]")

ppConsDef :: ConsDef -> String
ppConsDef (ConsDef name targs) =
  name ++ "(" ++ ppTArgs ", " targs ++ ")"


ppType :: Type -> String
ppType (FunType t1 t2) = ppFTLeft t1 ++ " -> " ++ ppType t2
ppType (TyVar name) = "'" ++ name
ppType (TyCons name []) = name
ppType (TyCons name targs) =
  name ++ " " ++ ppTArgs " " targs

ppFTLeft e@(FunType _ _) = "(" ++ ppType e ++ ")"
ppFTLeft e = ppType e

ppTArgs :: String -> [Type] -> String
ppTArgs sep [] = ""
ppTArgs sep [t] = ppTAParen t
ppTArgs sep (t:ts) = ppTAParen t ++ sep ++ ppTArgs sep ts

ppTAParen :: Type -> String
ppTAParen e@(FunType _ _) = "(" ++ ppType e ++ ")"
ppTAParen e@(TyCons _ (_:_)) = "(" ++ ppType e ++ ")"
ppTAParen e = ppType e


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
ppExpr (Case name) = "case[" ++ name ++ "]"
ppExpr Fix = "fix"


ppAppLeft :: Expr -> String
ppAppLeft e@(Lambda _ _) = "(" ++ ppExpr e ++ ")"
ppAppLeft e = ppExpr e

ppAppRight :: Expr -> String
ppAppRight e@(App _ _) = "(" ++ ppExpr e ++ ")"
ppAppRight e@(Lambda _ _) = "(" ++ ppExpr e ++ ")"
ppAppRight e = ppExpr e
