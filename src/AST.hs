
module AST where

import qualified Data.Set as S

import qualified CST


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


statementFromCST :: CST.Statement -> Statement
statementFromCST (CST.TypeDefS tdef) = TypeDefS (tdefFromCST tdef)
statementFromCST (CST.FuncDefS fdef) = FuncDefS (fdefFromCST fdef)


tdefFromCST :: CST.TypeDef -> TypeDef
tdefFromCST (CST.TypeDef name args cdefs) =
  TypeDef name args (map cdefFromCST cdefs)

cdefFromCST :: CST.ConsDef -> ConsDef
cdefFromCST (CST.ConsDef name targs) =
  ConsDef name (map typeFromCST targs)


typeFromCST :: CST.Type -> Type
typeFromCST (CST.FunType t1 t2) =
  FunType (typeFromCST t1) (typeFromCST t2)
typeFromCST (CST.TyVar name) = TyVar name
typeFromCST (CST.TyCons name targs) =
  TyCons name (map typeFromCST targs)


fdefFromCST :: CST.FuncDef -> FuncDef
fdefFromCST (CST.FuncDef False name vars expr) =
  FuncDef name (lambdify vars expr)
fdefFromCST (CST.FuncDef True name vars expr) =
  FuncDef name (App Fix (Lambda (Just name) (lambdify vars expr)))

exprFromCST :: CST.Expr -> Expr
exprFromCST (CST.Let [] e) = exprFromCST e
exprFromCST (CST.Let (f:fs) e)
  = Let (fdefFromCST f) (exprFromCST $ CST.Let fs e)
exprFromCST (CST.Lambda vs e) = lambdify vs e
exprFromCST (CST.App e1 e2) = App (exprFromCST e1) (exprFromCST e2)
exprFromCST (CST.Var name) = Var name
exprFromCST (CST.Case name) = Case name
exprFromCST CST.Fix = Fix


lambdify [] expr = exprFromCST expr
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
