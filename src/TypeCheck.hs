
module TypeCheck (Env (..), runEnv,
                  Bindings (..), Context (..),
                  empty, insert, insertLet,
                  checkExpr) where

import Data.Char (chr, ord)
import Control.Monad.State (StateT, get, put, lift,
                            runStateT, evalStateT, execStateT)
import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace (traceM)

import AST
import Unify
import qualified TypeDef as TD



type Env a = StateT (Int, Unification) (Either String) a


failure :: String -> Env a
failure s = lift (Left s)

runEnv :: Env a -> Either String a
runEnv c = evalStateT c (0, [])


tyvarFromInt :: Int -> String
tyvarFromInt n =
  if n < 26 then [chr $ n + ord 'a'] else "tv" ++ (show $ n - 26)

newtyvar :: Env String
newtyvar =
  do (n, u) <- get
     put (n + 1, u)
     return (tyvarFromInt n)

unify :: String -> Type -> Type -> Env ()
unify errMsg t1 t2 =
  do (n, u) <- get
     case prependEq (t1, t2) u of
       Just u' -> put (n, u')
       Nothing -> failure (errMsg ++ ": failed to unify (" ++
                           ppType t1 ++ ") and ( " ++ ppType t2 ++ ")")


-- A binding maps a variable to pair.
-- The pair is (quantified variables, type)
type Bindings = M.Map String (S.Set String, Type)
-- Set of free variables and the variable bindings
data Context = Context (S.Set String) Bindings

tvarsIn :: Type -> S.Set String
tvarsIn (FunType t1 t2) = S.union (tvarsIn t1) (tvarsIn t2)
tvarsIn (TyVar name) = S.singleton name
tvarsIn (TyCons _ ts) = S.unions (map tvarsIn ts)

empty :: Context
empty = Context S.empty M.empty

insert :: String -> Type -> Context -> Context
insert name t (Context ftvs bs) =
  let ftvs' = S.union (tvarsIn t) ftvs in
    Context ftvs' (M.insert name (S.empty, t) bs)

insertLet :: String -> Type -> Context -> Context
insertLet name t (Context ftvs bs) =
  let qtvs = S.difference (tvarsIn t) ftvs in
    Context ftvs (M.insert name (qtvs, t) bs)

reifyContext :: Context -> Env Context
reifyContext (Context ftvs bs) =
  do (_, u) <- get
     let f t = tvarsIn (subst u t)
     let ftvs' = S.unions (map (f . TyVar) (S.toList ftvs))
     return (Context ftvs' bs)

subsReturn :: Type -> Env Type
subsReturn t =
  do (_, u) <- get
     return (subst u t)


checkExpr :: Expr -> TD.Bindings -> Context -> Env Type
checkExpr (Let (FuncDef name body) e) tdbs ctx =
  do t <- checkExpr body tdbs ctx
     ctx' <- reifyContext ctx
     checkExpr e tdbs (insertLet name t ctx')
checkExpr (Lambda mname body) tdbs ctx =
  do t <- newtyvar
     let ctx' = case mname of
                  Just name -> insert name (TyVar t) ctx
                  Nothing -> ctx
     tb <- checkExpr body tdbs ctx'
     subsReturn (FunType (TyVar t) tb)
checkExpr (App e1 e2) tdbs ctx =
  do t1 <- checkExpr e1 tdbs ctx
     t2 <- checkExpr e2 tdbs ctx
     t <- newtyvar
     unify "app" t1 (FunType t2 (TyVar t))
     subsReturn (TyVar t)
checkExpr (Var name) (TD.Bindings tdbs cdbs) (Context _ bs) =
  case M.lookup name bs of
    Just (qtvs, t) -> instQTypes qtvs t
    Nothing ->
      case M.lookup name cdbs of
        Just (tcname, targs) -> consType tcname (M.lookup tcname tdbs) targs
        Nothing -> failure ("Variable not bound " ++ name)
checkExpr Fix _ _ =
  do tname <- newtyvar
     let t = TyVar tname
     subsReturn (FunType (FunType t t) t)
checkExpr (Case name) (TD.Bindings tdbs cdbs) _ =
  caseType name tdbs cdbs




consType :: String -> Maybe ([String], [String]) -> [Type]
         -> Env Type
consType tcname Nothing _ = failure ("Constructor " ++ tcname ++ " has no matching type constructor")
consType tcname (Just (tyvars, _)) ts =
  instQTypes (S.fromList tyvars) (funForm ts (TyCons tcname (map TyVar tyvars)))

caseType :: String -> TD.TypeDefMap -> TD.ConsDefMap -> Env Type
caseType name tdbs cdbs =
  case M.lookup name tdbs of
    Nothing -> failure ("Case; type constructor " ++ name ++ " not defined")
    Just (tyvars, cdefs) ->
      do m <- getReplTyVars tyvars
         retTypeS <- newtyvar
         let retType = TyVar retTypeS
         fts <- mapM (caseConsType retType cdbs) cdefs
         let ct = FunType (TyCons name (TyVar <$> tyvars)) (funForm fts retType)
         return (replaceTyVars m ct)

caseConsType :: Type -> TD.ConsDefMap -> String
             -> Env Type
caseConsType retType cdbs name =
  case M.lookup name cdbs of
    Nothing -> failure ("Case; data constructor " ++ name ++ " not defined")
    Just (_, ts) -> return (funForm ts retType)

funForm :: [Type] -> Type -> Type
funForm [] retType = retType
funForm (t:ts) retType = FunType t (funForm ts retType)


instQTypes :: S.Set String -> Type -> Env Type
instQTypes ftvs t =
  do m <- getReplTyVars (S.toList ftvs)
     return (replaceTyVars m t)

getReplTyVars :: [String] -> Env (M.Map String String)
getReplTyVars [] = return M.empty
getReplTyVars (t:ts) =
  do t' <- newtyvar
     m <- getReplTyVars ts
     return (M.insert t t' m)

replaceTyVars :: M.Map String String -> Type -> Type
replaceTyVars m (FunType t1 t2) =
  FunType (replaceTyVars m t1) (replaceTyVars m t2)
replaceTyVars m (TyVar name) =
  case M.lookup name m of
    Nothing -> TyVar name
    Just name' -> TyVar name'
replaceTyVars m (TyCons name ts) =
  TyCons name (map (replaceTyVars m) ts)
