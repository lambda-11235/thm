
import Data.Char (chr, ord)
import Control.Monad.State (StateT, get, put, lift,
                            runStateT, evalStateT, execStateT)
import qualified Data.Map as M
import qualified Data.Set as S

import CST
import Unify



type Context a = StateT (Int, Unification) (Either String) a


failure :: String -> Context a
failure s = lift (Left s)


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
type UnInstTVars = S.Set String
type Bindings = M.Map String (UnInstTVars, Type)

subsReturn :: (UnInstTVars, Type) -> Context (UnInstTVars, Type)
subsReturn (uitvs, t) =
  do (_, u) <- get
     return (uitvs, subst u t)


checkExpr :: Expr -> Bindings -> Context (UnInstTVars, Type)
checkExpr (Let (FuncDef name body) e) bs =
  do (uitvs, t) <- checkExpr body bs
     checkExpr e (M.insert name (uitvs, t) bs)
checkExpr (Lambda mname body) bs =
  do t <- newtyvar
     let bs' = case mname of
                 Just name -> M.insert name (S.empty, TVar t) bs
                 Nothing -> bs
     (uitvs, tb) <- checkExpr body bs'
     subsReturn (S.insert t uitvs, FunType (TVar t) tb)
checkExpr (App e1 e2) bs =
  do (ut1, t1) <- checkExpr e1 bs
     (ut2, t2) <- checkExpr e2 bs
     t <- newtyvar
     unify "app" t1 (FunType t2 (TVar t))
     (_, u) <- get
     subsReturn (S.insert t (S.union ut1 ut2), TVar t)
checkExpr (Var name) bs =
  case M.lookup name bs of
    Just (qtvs, t) -> instQTypes qtvs t
    Nothing -> failure "var"
checkExpr Fix _ =
  do t1 <- newtyvar
     t2 <- newtyvar
     let ft = (FunType (TVar t1) (TVar t2))
     subsReturn (S.fromList [t1, t2], FunType (FunType ft ft) ft)
checkExpr Unit _ = subsReturn (S.empty, UnitType)
checkExpr Z _ = subsReturn (S.empty, NatType)
checkExpr S _ = subsReturn (S.empty, FunType NatType NatType)
checkExpr NatCase _ =
  do t <- newtyvar
     subsReturn (S.fromList [t],
             FunType NatType (FunType (FunType UnitType (TVar t))
                              (FunType (FunType NatType (TVar t)) (TVar t))))


replaceTVar :: String -> String -> Type -> Type
replaceTVar old new (FunType t1 t2) =
  FunType (replaceTVar old new t1) (replaceTVar old new t2)
replaceTVar old new (TVar name) =
  if name == old then TVar new else TVar name
replaceTVar _ _ UnitType = UnitType
replaceTVar _ _ NatType = NatType

instQTypes :: UnInstTVars -> Type -> Context (UnInstTVars, Type)
instQTypes uitvs t = instQTypes' (S.toList uitvs) t
  where
    instQTypes' [] t = subsReturn (S.empty, t)
    instQTypes' (tv:tvs) t =
      do tv' <- newtyvar
         (uitvs, t') <- instQTypes' tvs (replaceTVar tv tv' t)
         subsReturn (S.insert tv' uitvs, t')
