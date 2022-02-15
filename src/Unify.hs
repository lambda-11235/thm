
-- Unification based on Alberto Martelli and Ugo Montanari's paper
-- "An Efficient Unification Algorithm", specifically algorithm 1.

module Unify where

import CST

import qualified Data.Map as Map


type Substitution = Map.Map String Type
type Equations = [(Type, Type)]

data Result = Success Equations
            | NoAction
            | Failure
type UnifRule = (Type, Type) -> Equations -> Result


mgu :: Equations -> Maybe Substitution
mgu eqs =
    case tryRules rules eqs of
        Success eqs' -> mgu eqs'
        NoAction -> toSubstition eqs
        Failure -> Nothing

toSubstition :: Equations -> Maybe Substitution
toSubstition = (fmap Map.fromList) . sequence . (fmap eqToSubs)
  where
    eqToSubs (TVar name, t) = Just (name, t)
    eqToSubs _ = Nothing


tryRules :: [UnifRule] -> Equations -> Result
tryRules [] _ = NoAction
tryRules (r:rs) eqs =
  case tryRule r eqs of
    Success x -> Success x
    NoAction -> tryRules rs eqs
    Failure -> Failure

tryRule :: UnifRule -> Equations -> Result
tryRule f eqs = tryRuleSub f [] eqs

tryRuleSub :: UnifRule -> Equations -> Equations -> Result
tryRuleSub _ _ [] = NoAction
tryRuleSub f beqs (eq:feqs) =
  case f eq (beqs ++ feqs) of
    Success x -> Success x
    NoAction -> tryRuleSub f (eq:beqs) feqs
    Failure -> Failure


occursType :: String -> Type -> Bool
occursType name (FunType t1 t2) =
  occursType name t1 || occursType name t2
occursType name (TVar name') = name == name'
occursType _ UnitType = False
occursType _ NatType = False

occursUnif :: String -> Equations -> Bool
occursUnif _ [] = False
occursUnif name ((t1, t2):eqs) =
  occursType name t1 || occursType name t2 || occursUnif name eqs

subsType :: String -> Type -> Type -> Type
subsType name t tv@(TVar name') =
  if name == name' then t else tv
subsType name t (FunType t1 t2) =
  FunType (subsType name t t1) (subsType name t t2)
subsType _ _ t = t

subsUnif :: String -> Type -> Equations -> Equations
subsUnif name t =
  fmap (\(t1, t2) -> (subsType name t t1, subsType name t t2))


rules :: [UnifRule]
rules = [flipRule, reflRule, consRule, reduceRule]


flipRule :: UnifRule
flipRule (TVar _, TVar _) _ = NoAction
flipRule (t, tv@(TVar _)) eqs = Success ((tv, t):eqs)
flipRule _ _ = NoAction

reflRule :: UnifRule
reflRule (TVar name1, TVar name2) eqs =
  if name1 == name2 then
    Success eqs
  else
    NoAction
reflRule _ _ = NoAction

consRule :: UnifRule
consRule (FunType t1 t2, FunType t3 t4) eqs =
  Success ((t1, t3):(t2, t4):eqs)
consRule (UnitType, UnitType) _ = NoAction
consRule (NatType, NatType) _ = NoAction
consRule (TVar _, _) _ = NoAction
consRule (_, TVar _) _ = NoAction
consRule _ _ = Failure

reduceRule :: UnifRule
reduceRule eq@(TVar name, t) eqs =
  if occursType name t then
    Failure
  else if occursUnif name eqs then
    Success (eq:(subsUnif name t eqs))
  else
    NoAction
reduceRule _ _ = NoAction
