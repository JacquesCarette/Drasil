{-# LANGUAGE GADTs #-}
module Language.Drasil.Space
  (Space(..), DomainDesc(..), RealInterval(..), RTopology(..), Inclusive(..),
  getActorName, getInnerSpace, mkPrimitiveMapping) where

import Language.Drasil.Symbol (Symbol)

-- FIXME: These need to be spaces and not just types.

-- | The difference kinds of spaces that may exist. This type holds
-- numerical spaces (such as the set of integers, rationals, etc.),
-- a space for booleans, a space for characters, dimensional spaces (vectors, arrays, etc.),
-- a space for Actors, discrete sets (both for numbers and strings), and a void space.
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
  | DiscreteD [Double]
  | DiscreteS [String] --ex. let Meal = {"breakfast", "lunch", "dinner"}
  | Mapping [Space] Space -- TODO: this one shouldn't be exposed directly, only through a smart constructor
  | Void
  deriving (Eq, Show)

mkPrimitiveMapping :: [Space] -> Space -> Space
mkPrimitiveMapping _   (Mapping _ _) = error "Target must be basic"
mkPrimitiveMapping ins trg
  | any isMapping ins                = error "All inputs must be basic"
  | otherwise                        = Mapping ins trg

isMapping :: Space -> Bool
isMapping (Mapping _ _) = True
isMapping _             = False

-- The 'spaces' below are all good.

-- | Topology of a subset of reals.
data RTopology = Continuous | Discrete

-- | Describes the domain of a 'Symbol' given a topology. Can be bounded or encase all of the domain.
data DomainDesc a b where
  BoundedDD :: Symbol -> RTopology -> a -> b -> DomainDesc a b
  AllDD     :: Symbol -> RTopology -> DomainDesc a b

-- | Inclusitivity of a 'Space'.
data Inclusive = Inc | Exc

-- | A RealInterval is a subset of 'Real' (as a 'Space').
-- These come in different flavours.
-- For now, we embed 'Expr' for the bounds, but that will change as well.
data RealInterval a b where
  Bounded :: (Inclusive, a) -> (Inclusive, b) -> RealInterval a b -- ^ Interval from (x .. y).
  UpTo    :: (Inclusive, a) -> RealInterval a b                   -- ^ Interval from (-infinity .. x).
  UpFrom  :: (Inclusive, b) -> RealInterval a b                   -- ^ Interval from (x .. infinity).

-- | Gets the name of an 'Actor'.
getActorName :: Space -> String
getActorName (Actor n) = n
getActorName _ = error "getActorName called on non-actor space"

-- | Gets the inner 'Space' of a vector.
getInnerSpace :: Space -> Space
getInnerSpace (Vect s) = s
getInnerSpace _ = error "getInnerSpace called on non-vector space"
