{-# LANGUAGE GADTs #-}
module Language.Drasil.Space
  (Space(..), DomainDesc(..), RealInterval(..),Equal(..), RTopology(..), Inclusive(..),
  getActorName, getInnerSpace) where

import Language.Drasil.Symbol (Symbol)

-- FIXME: These need to be spaces and not just types.

-- | Spaces
data Space =
    Integer
  | Rational
  | Real
  | Natural
  | Boolean
  | Char
  | String
  | Radians
  | Vect Space
  | Array Space
  | Actor String
  | DiscreteI [Int]  --ex. let A = {1, 2, 4, 7}
  | DiscreteD [Double]
  | DiscreteS [String] --ex. let Meal = {"breakfast", "lunch", "dinner"}
  | Void
  deriving (Eq, Show)

-- The 'spaces' below are all good.

-- | Topology of a subset of reals.
data RTopology = Continuous | Discrete

data DomainDesc a b where
  BoundedDD :: Symbol -> RTopology -> a -> b -> DomainDesc a b
  AllDD :: Symbol -> RTopology -> DomainDesc a b

data Inclusive = Inc | Exc

-- | RealInterval. A |RealInterval| is a subset of |Real| (as a |Space|).
-- These come in different flavours.
-- For now, embed |Expr| for the bounds, but that will change as well.
data RealInterval a b where
  Bounded :: (Inclusive, a) -> (Inclusive, b) -> RealInterval a b -- (x .. y)
  UpTo :: (Inclusive, a) -> RealInterval a b -- (-infinity .. x)
  UpFrom :: (Inclusive, b) -> RealInterval a b -- (x .. infinity)

data Equal a where
  ExactlyEqual :: a -> Equal a

getActorName :: Space -> String
getActorName (Actor n) = n
getActorName _ = error "getActorName called on non-actor space"

getInnerSpace :: Space -> Space
getInnerSpace (Vect s) = s
getInnerSpace _ = error "getInnerSpace called on non-vector space"