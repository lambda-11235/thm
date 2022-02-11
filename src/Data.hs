
module Data where

import qualified CST

import qualified Data.Map as Map
import qualified Data.Set as Set


data QuantifiedType = QType (Set.Set String) CST.Type

type Context = Map.Map String Data
type TypeContext = Map.Map String QuantifiedType

          
data Data = Unit
          | Nat Int
          | Function Context String CST.Expr
  deriving (Show)
