
-- Unification based on Alberto Martelli and Ugo Montanari's paper
-- "An Efficient Unification Algorithm", specifically algorithm 1.

module Unify (Unification,
              fromList, prependEq, union,
              getVar, subst
             )
where

import AST


type Unification = [(Type, Type)]

data Result = Success Unification
            | NoAction
            | Failure
type UnifRule = (Type, Type) -> Unification -> Result


fromList :: Unification -> Maybe Unification
fromList eqs =
    case tryRules rules eqs of
        Success eqs' -> fromList eqs'
        NoAction -> Just eqs
        Failure -> Nothing

prependEq :: (Type, Type) -> Unification -> Maybe Unification
prependEq eq eqs = fromList $ eq:eqs

union :: Unification -> Unification -> Maybe Unification
union eqs1 eqs2 = fromList $ eqs1 ++ eqs2

getVar :: String -> Unification -> Maybe Type
getVar _ [] = Nothing
getVar name ((TyVar name', t):eqs) =
  if name == name' then Just t else getVar name eqs

subst :: Unification -> Type -> Type
subst u (FunType t1 t2) = FunType (subst u t1) (subst u t2)
subst u (TyVar name) =
  case getVar name u of
    Just t -> t
    Nothing -> TyVar name
subst u (TyCons name targs) = TyCons name (map (subst u) targs)


tryRules :: [UnifRule] -> Unification -> Result
tryRules [] _ = NoAction
tryRules (r:rs) eqs =
  case tryRule r eqs of
    Success x -> Success x
    NoAction -> tryRules rs eqs
    Failure -> Failure

tryRule :: UnifRule -> Unification -> Result
tryRule f eqs = tryRuleSub f [] eqs

tryRuleSub :: UnifRule -> Unification -> Unification -> Result
tryRuleSub _ _ [] = NoAction
tryRuleSub f beqs (eq:feqs) =
  case f eq (beqs ++ feqs) of
    Success x -> Success x
    NoAction -> tryRuleSub f (eq:beqs) feqs
    Failure -> Failure


occursType :: String -> Type -> Bool
occursType name (FunType t1 t2) =
  occursType name t1 || occursType name t2
occursType name (TyVar name') = name == name'
occursType name (TyCons _ targs) = any (occursType name) targs

occursUnif :: String -> Unification -> Bool
occursUnif _ [] = False
occursUnif name ((t1, t2):eqs) =
  occursType name t1 || occursType name t2 || occursUnif name eqs

subsType :: String -> Type -> Type -> Type
subsType name t tv@(TyVar name') =
  if name == name' then t else tv
subsType name t (FunType t1 t2) =
  FunType (subsType name t t1) (subsType name t t2)
subsType name t (TyCons tcname targs) =
  TyCons tcname (map (subsType name t) targs)

subsUnif :: String -> Type -> Unification -> Unification
subsUnif name t =
  fmap (\(t1, t2) -> (subsType name t t1, subsType name t t2))


rules :: [UnifRule]
rules = [flipRule, reflRule, consRule, reduceRule]


flipRule :: UnifRule
flipRule (TyVar _, TyVar _) _ = NoAction
flipRule (t, tv@(TyVar _)) eqs = Success ((tv, t):eqs)
flipRule _ _ = NoAction

reflRule :: UnifRule
reflRule (t1, t2) eqs =
  if t1 == t2 then
    Success eqs
  else
    NoAction

consRule :: UnifRule
consRule (FunType t1 t2, FunType t3 t4) eqs =
  Success ((t1, t3):(t2, t4):eqs)
consRule (TyCons name1 targs1, TyCons name2 targs2) eqs =
  if name1 /= name2 || length targs1 /= length targs2 then
    Failure
  else
    Success ((zipWith (,) targs1 targs2) ++ eqs)
consRule (TyVar _, _) _ = NoAction
consRule (_, TyVar _) _ = NoAction
consRule _ _ = Failure

reduceRule :: UnifRule
reduceRule eq@(TyVar name, t) eqs =
  if occursType name t then
    Failure
  else if occursUnif name eqs then
    Success (eq:(subsUnif name t eqs))
  else
    NoAction
reduceRule _ _ = NoAction
