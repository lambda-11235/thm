
module TypeDef ( TypeDefMap (..)
               , ConsDefMap (..)
               , Bindings (..)
               , Result (..)
               , empty, updateBindings
               ) where

import Data.Foldable (foldr')
import qualified Data.Map as M
import qualified Data.Set as S

import AST


-- TypeDefMap is a map from Type constructors to their TyVars and
-- data constructors name.
type TypeDefMap = M.Map String ([String], [String])

-- ConsDefMap is a map from data constructor names to their type
-- constructor name and their type arguments
type ConsDefMap = M.Map String (String, [Type])

data Bindings = Bindings TypeDefMap ConsDefMap

type Result a = Either String a


empty :: Bindings
empty = Bindings M.empty M.empty


updateBindings :: TypeDef -> Bindings -> Result Bindings
updateBindings tdef bs =
  do let bs' = addTypeDef tdef bs
     checkTypeDef bs' tdef
     return bs'


addTypeDef :: TypeDef -> Bindings -> Bindings
addTypeDef (TypeDef name targs cdefs) (Bindings tdbs cdbs) =
  let cdnames = map (\(ConsDef s _) -> s) cdefs
      tdbs' = M.insert name (targs, cdnames) tdbs
      ff (ConsDef s targs) bs = M.insert s (name, targs) bs
      cdbs' = foldr' ff cdbs cdefs
  in Bindings tdbs' cdbs'


checkTypeDef :: Bindings -> TypeDef -> Result ()
checkTypeDef bs (TypeDef name targs cdefs) =
  do checkTArgs S.empty targs
     mapM_ (checkCDef bs (S.fromList targs)) cdefs

checkTArgs :: S.Set String -> [String] -> Result ()
checkTArgs _ [] = return ()
checkTArgs seen (t:ts) =
  if S.member t seen then
    Left ("Duplicate TyVar '" ++ t)
  else
    checkTArgs (S.insert t seen) ts

checkCDef :: Bindings -> S.Set String -> ConsDef -> Result ()
checkCDef bs targs (ConsDef _ ts) =
  mapM_ (checkCDefArg bs targs) ts

checkCDefArg :: Bindings -> S.Set String -> Type -> Result ()
checkCDefArg bs targs (FunType t1 t2) =
  do checkCDefArg bs targs t1
     checkCDefArg bs targs t2
checkCDefArg bs targs (TyVar name) =
  if S.member name targs then
    return ()
  else
    Left ("Unparamerterized TyVar '" ++ name)
checkCDefArg bs@(Bindings tdbs _) targs (TyCons name ts) =
  case M.lookup name tdbs of
    Nothing -> Left ("Undefined type constructor " ++ name)
    Just (tvars, _) ->
      if length tvars == length ts then
        mapM_ (checkCDefArg bs targs) ts
      else
        Left ("Wrong number of type arguments for " ++ name)
