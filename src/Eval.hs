
module Eval where

import CST

import qualified Data.Map as M


type Bindings = M.Map String Data
type Result a = Either String a
          
data Data = DUnit
          | DNum Int
          | DFix (Maybe Data)
          | DS
          | DNatCase (Maybe Int) (Maybe Data)
          | DFunction Bindings (Maybe String) Expr
  deriving (Show)


evalExpr :: Bindings -> Expr -> Result Data
evalExpr ctx (Let (FuncDef name body) e) =
  do dbody <- evalExpr ctx body
     evalExpr (M.insert name dbody ctx) e
evalExpr ctx (Lambda name body) =
  return (DFunction ctx name body)
evalExpr ctx (App e1 e2) =
  do d1 <- evalExpr ctx e1
     d2 <- evalExpr ctx e2
     apply d1 d2
evalExpr ctx (Var name) =
  case M.lookup name ctx of
    Just x -> return x
    Nothing -> Left ("Unbound variable " ++ name)
evalExpr _ Fix = return (DFix Nothing)
evalExpr _ Unit = return DUnit
evalExpr _ (Num x) = return (DNum x)
evalExpr _ S = return DS
evalExpr _ NatCase = return (DNatCase Nothing Nothing)


apply :: Data -> Data -> Result Data
apply DS (DNum n) = return (DNum $ n + 1)
apply (DFix Nothing) d = return (DFix $ Just d)
apply f@(DFix (Just df)) d =
  do f' <- apply df f
     apply f' d
apply (DNatCase Nothing Nothing) (DNum n) =
  return (DNatCase (Just n) Nothing)
apply (DNatCase (Just n) Nothing) dx =
  return (DNatCase (Just n) (Just dx))
apply (DNatCase (Just n) (Just dx)) df =
  natCase n dx df
apply (DFunction ctx name body) x =
  case name of
    Nothing -> evalExpr ctx body
    Just s -> evalExpr (M.insert s x ctx) body
apply f x = Left ("Failed to apply " ++ ppData f ++ " to " ++ ppData x)


natCase n dx df =
  if n <= 0 then
    apply dx DUnit
  else
    apply df (DNum $ n - 1)


ppData :: Data -> String
ppData DUnit = "unit"
ppData (DNum n) = show n
ppData (DFix Nothing) = "fix"
ppData (DFix (Just x)) = "fix " ++ ppParen x
ppData DS = "S"
ppData (DNatCase Nothing Nothing) = "natCase"
ppData (DNatCase Nothing (Just x)) = "natCase <error> " ++ ppParen x
ppData (DNatCase (Just n) Nothing) = "natCase " ++ show n
ppData (DNatCase (Just n) (Just x)) = "natCase " ++ show n ++ " " ++ ppParen x
ppData (DFunction _ Nothing body) = "\\_. " ++ ppExpr body
ppData (DFunction _ (Just name) body) = "let <context> in \\" ++ name ++ ". " ++ ppExpr body

ppParen :: Data -> String
ppParen x@(DFix _) = "(" ++ ppData x ++ ")"
ppParen x@(DNatCase _ _) = "(" ++ ppData x ++ ")"
ppParen x@(DFunction _ _ _) = "(" ++ ppData x ++ ")"
ppParen x = ppData x
