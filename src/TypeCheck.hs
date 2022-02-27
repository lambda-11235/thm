
module TypeCheck (Context (..), runContext,
                  UnInstTyVars (..), Bindings (..), insert,
                  checkExpr) where

import Data.Char (chr, ord)
import Control.Monad.State (StateT, get, put, lift,
                            runStateT, evalStateT, execStateT)
import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace (traceM)

import CST
import Unify
import qualified TypeDef as TD



type Context a = StateT (Int, Unification) (Either String) a


failure :: String -> Context a
failure s = lift (Left s)

runContext :: Context a -> Either String a
runContext c = evalStateT c (0, [])


tyvarFromInt :: Int -> String
tyvarFromInt n =
  if n < 26 then [chr $ n + ord 'a'] else "tv" ++ (show $ n - 26)
  
newtyvar :: Context String
newtyvar =
  do (n, u) <- get
     put (n + 1, u)
     return (tyvarFromInt n)

unify :: String -> Type -> Type -> Context ()
unify errMsg t1 t2 =
  do (n, u) <- get
     case prependEq (t1, t2) u of
       Just u' -> put (n, u')
       Nothing -> failure (errMsg ++ ": failed to unify (" ++
                           ppType t1 ++ ") and ( " ++ ppType t2 ++ ")")


-- A binding maps a variable to pair.
-- The pair is (quantified variable, type)
type UnInstTyVars = S.Set String
type Bindings = M.Map String (UnInstTyVars, Type)

tvarsIn :: Type -> UnInstTyVars
tvarsIn (FunType t1 t2) = S.union (tvarsIn t1) (tvarsIn t2)
tvarsIn (TyVar name) = S.singleton name
tvarsIn (TyCons _ ts) = S.unions (map tvarsIn ts)

insert :: String -> UnInstTyVars -> Type -> Bindings -> Bindings
insert name uitvs t bs = 
  let uitvs' = S.intersection uitvs (tvarsIn t) in
    M.insert name (uitvs', t) bs

subsReturn :: (UnInstTyVars, Type) -> Context (UnInstTyVars, Type)
subsReturn (uitvs, t) =
  do (_, u) <- get
     return (uitvs, subst u t)


checkExpr :: Expr -> TD.Bindings -> Bindings -> Context (UnInstTyVars, Type)
checkExpr (Let (FuncDef name body) e) tdbs bs =
  do (uitvs, t) <- checkExpr body tdbs bs
     checkExpr e tdbs (insert name uitvs t bs)
checkExpr (Lambda mname body) tdbs bs =
  do t <- newtyvar
     let bs' = case mname of
                 Just name -> insert name S.empty (TyVar t) bs
                 Nothing -> bs
     (uitvs, tb) <- checkExpr body tdbs bs'
     subsReturn (S.insert t uitvs, FunType (TyVar t) tb)
checkExpr (App e1 e2) tdbs bs =
  do (ut1, t1) <- checkExpr e1 tdbs bs
     (ut2, t2) <- checkExpr e2 tdbs bs
     t <- newtyvar
     unify "app" t1 (FunType t2 (TyVar t))
     (_, u) <- get
     subsReturn (S.insert t (S.union ut1 ut2), TyVar t)
checkExpr (Var name) (TD.Bindings tdbs cdbs) bs =
  case M.lookup name bs of
    Just (qtvs, t) -> instQTypes qtvs t
    Nothing ->
      case M.lookup name cdbs of
        Just (tcname, targs) -> consType tcname (M.lookup tcname tdbs) targs
        Nothing -> failure ("Variable not bound " ++ name)
checkExpr Fix _ _ =
  do tname <- newtyvar
     let t = TyVar tname
     subsReturn (S.singleton tname, FunType (FunType t t) t)
checkExpr (Case name) (TD.Bindings tdbs cdbs) _ =
  caseType name tdbs cdbs




consType :: String -> Maybe ([String], [String]) -> [Type]
         -> Context (UnInstTyVars, Type)
consType tcname Nothing _ = failure ("Constructor " ++ tcname ++ " has no matching type constructor")
consType tcname (Just (tyvars, _)) ts =
  instQTypes (S.fromList tyvars) (funForm ts (TyCons tcname (map TyVar tyvars)))

caseType :: String -> TD.TypeDefMap -> TD.ConsDefMap -> Context (UnInstTyVars, Type)
caseType name tdbs cdbs =
  case M.lookup name tdbs of
    Nothing -> failure ("Case; type constructor " ++ name ++ " not defined")
    Just (tyvars, cdefs) ->
      do (uitvs, m) <- getReplTyVars tyvars
         retTypeS <- newtyvar
         let retType = TyVar retTypeS
         fts <- mapM (caseConsType retType cdbs) cdefs
         let ct = funForm fts (FunType (TyCons name (TyVar <$> tyvars)) retType)
         return (S.insert retTypeS uitvs,
                 replaceTyVars m ct)

caseConsType :: Type -> TD.ConsDefMap -> String
             -> Context Type
caseConsType retType cdbs name =
  case M.lookup name cdbs of
    Nothing -> failure ("Case; data constructor " ++ name ++ " not defined")
    Just (_, ts) -> return (funForm ts retType)

funForm :: [Type] -> Type -> Type
funForm [] retType = retType
funForm (t:ts) retType = FunType t (funForm ts retType)


instQTypes :: UnInstTyVars -> Type -> Context (UnInstTyVars, Type)
instQTypes uitvs t =
  do (uitvs', m) <- getReplTyVars (S.toList uitvs)
     return (uitvs', replaceTyVars m t)

getReplTyVars :: [String] -> Context (UnInstTyVars, M.Map String String)
getReplTyVars [] = return (S.empty, M.empty)
getReplTyVars (t:ts) =
  do t' <- newtyvar
     (uitvs, m) <- getReplTyVars ts
     return (S.insert t' uitvs, M.insert t t' m)

replaceTyVars :: M.Map String String -> Type -> Type
replaceTyVars m (FunType t1 t2) =
  FunType (replaceTyVars m t1) (replaceTyVars m t2)
replaceTyVars m (TyVar name) =
  case M.lookup name m of
    Nothing -> TyVar name
    Just name' -> TyVar name'
replaceTyVars m (TyCons name ts) =
  TyCons name (map (replaceTyVars m) ts)
