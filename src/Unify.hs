
module Unify where

import CST

import qualified Data.map as Map


type Substitution = [(String, Type)]

empty :: Substitution
empty = []

single :: String -> Type -> Substitution
single name x = [(name, x)]


union :: Substitution -> Substitution -> Maybe Substitution

union [] s2 = Just s2
union ((name, t):s1) s2 =
  do s2' <- (substSubs name t s2)
     return (unionSub s1 s2')

substSubs name1 t1 ((name2, t2):s) =
  if name1 == name2 then
    do s' <- unify t1 t2
       union s s'
  else
    (name2, subst name1 t1 t2):(substSubs name1 t1 s)

subst name1 t1 (FunType t2 t3) =
  FunType (subst name1 t1 t2) (subst name1 t1 t3)
subst name1 t1 (TVar name2) =
  if name1 == name2 then t1 else TVar name2
subst _ _ UnitType = UnitType
subst _ _ NatType = NatType
    


unify :: Type -> Type -> Maybe Substitution
unify (FunType t1 t2) (FunType t3 t4) =
  union (unify t1 t2) (unify t3 t4)
unify (TVar name) x =
  if occurs name x then Nothing else Just $ single name x
unify x (TVar name) =
  if occurs name x then Nothing else Just $ single name x
unify UnitType UnitType = empty
unify NatType NatType = empty
