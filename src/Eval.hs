
module Eval ( Bindings (..)
            , Result (..)
            , Data (..)
            , evalExpr, force
            , ppData
            ) where

import Data.List (elemIndex)
import qualified Data.Map as M

import CST
import qualified TypeDef as TD


type Bindings = M.Map String Data
type Result a = Either String a
          
data Data = DFix (Maybe Data)
          | DCase String [Data]
          | DCons String [Data]
          | DThunk Bindings Expr
          | DFunction Bindings (Maybe String) Expr
  deriving (Show)


force :: TD.Bindings -> Data -> Result Data
force tdbs (DCons name xs) =
  do xs' <- mapM (force tdbs) xs
     return (DCons name xs')
force tdbs f@(DFix (Just x)) =
  do f' <- apply tdbs x f
     force tdbs f'
force tdbs (DThunk ctx e) =
  do d <- evalExpr tdbs ctx e
     force tdbs d
force _ d = return d


evalExpr :: TD.Bindings -> Bindings -> Expr -> Result Data
evalExpr tdbs ctx (Let (FuncDef name body) e) =
  do dbody <- evalExpr tdbs ctx body
     evalExpr tdbs (M.insert name dbody ctx) e
evalExpr _ ctx (Lambda name body) =
  return (DFunction ctx name body)
evalExpr tdbs ctx (App e1 e2) =
  do d1 <- evalExpr tdbs ctx e1
     let d2 = DThunk ctx e2
     apply tdbs d1 d2
evalExpr (TD.Bindings _ cdbs) ctx (Var name) =
  case M.lookup name ctx of
    Just x -> return x
    Nothing ->
      if M.member name cdbs then
        return (DCons name [])
      else
        Left ("Unbound variable " ++ name)
evalExpr _ _ (Case name) = return (DCase name [])
evalExpr _ _ Fix = return (DFix Nothing)


apply :: TD.Bindings -> Data -> Data -> Result Data
apply tdbs (DFix Nothing) d = return (DFix $ Just d)
apply tdbs f@(DFix (Just _)) d =
  do f' <- force tdbs f
     apply tdbs f' d
apply tdbs (DFunction ctx name body) x =
  case name of
    Nothing -> evalExpr tdbs ctx body
    Just s -> evalExpr tdbs (M.insert s x ctx) body
apply tdbs f@(DThunk _ _) x =
  do f' <- force tdbs f
     apply tdbs f' x
apply tdbs (DCons name xs) x = return (DCons name (xs ++ [x]))
apply tdbs@(TD.Bindings tdbs' _) (DCase name args) x =
  case M.lookup name tdbs' of
    Nothing -> Left ("Case undefined type " ++ name)
    Just (_, cons) ->
      if length args < length cons then
        return (DCase name (args ++ [x]))
      else
        do x' <- force tdbs x
           case x' of
             (DCons cname xs) ->
               case elemIndex cname cons of
                 Nothing -> Left ("Constructor " ++ cname ++ " does not belong to type " ++ name)
                 Just idx -> applyMany tdbs (args !! idx) xs
             _ -> Left ("Bad case application " ++ ppData x')
--apply f x = Left ("Failed to apply " ++ ppData f ++ " to " ++ ppData x)


applyMany :: TD.Bindings -> Data -> [Data] -> Result Data
applyMany tdbs f [] = return f
applyMany tdbs f (x:xs) =
  do f' <- apply tdbs f x
     applyMany tdbs f' xs


ppData :: Data -> String
ppData (DFix Nothing) = "fix"
ppData (DFix (Just x)) = "fix " ++ ppParen x
ppData (DCons name xs) = name ++ ppArgs xs
ppData (DCase name xs) = "case[" ++ name ++ "]" ++ ppArgs xs
ppData (DThunk _ _) = "<thunk>"
ppData (DFunction _ _ _) = "<function>"

ppArgs xs = concatMap (\x -> " " ++ ppParen x) xs

ppParen :: Data -> String
ppParen x@(DFix _) = "(" ++ ppData x ++ ")"
ppParen x@(DCons _ (_:_)) = "(" ++ ppData x ++ ")"
ppParen x@(DCase name (_:_)) = "(" ++ ppData x ++ ")"
--ppParen x@(DFunction _ _ _) = "(" ++ ppData x ++ ")"
ppParen x = ppData x
