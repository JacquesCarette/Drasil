{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

-- | Number space types and functions.
module Language.Drasil.Space (
  -- * Types
  Space(..), 
  RealInterval(..), Inclusive(..),
  DomainDesc(..), RTopology(..), DiscreteDomainDesc, ContinuousDomainDesc,
  -- * Class
  HasSpace(..),
  -- * Functions
  getActorName, getInnerSpace, mkFunction
) where

import qualified Data.List.NonEmpty as NE

import Language.Drasil.Symbol (Symbol)
import Control.Lens (Lens')
import Language.Drasil.WellTyped

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
  | Matrix Int Int Space
  | Array Space
  | Actor String
  | DiscreteD [Double]
  | DiscreteS [String] --ex. let Meal = {"breakfast", "lunch", "dinner"}
  | Function (NE.NonEmpty Primitive) Primitive
  | Void
  deriving (Eq, Show)

-- | HasSpace is anything which has a 'Space'.
class HasSpace c where
  -- | Provides a 'Lens' to the 'Space'.
  typ      :: Lens' c Space

type Primitive = Space

mkFunction :: [Primitive] -> Primitive -> Space
mkFunction []  = error "Function space creation requires at least 1 input Space"
mkFunction ins = Function (NE.fromList ins)

-- The 'spaces' below are all good.

-- | Topology of a subset of reals.
data RTopology = Continuous | Discrete

-- | Describes the domain of a 'Symbol' given a topology. Can be bounded or encase all of the domain.
data DomainDesc (tplgy :: RTopology) a b where
  BoundedDD :: Symbol -> RTopology -> a -> b -> DomainDesc 'Discrete a b
  AllDD     :: Symbol -> RTopology -> DomainDesc 'Continuous a b

type DiscreteDomainDesc a b = DomainDesc 'Discrete a b
type ContinuousDomainDesc a b = DomainDesc 'Continuous a b

-- | Inclusive or exclusive bounds.
data Inclusive = Inc | Exc

-- | A RealInterval is a subset of 'Real' (as a 'Space').
-- These come in different flavours.
-- For now, we embed 'Expr' for the bounds, but that will change as well.
data RealInterval a b where
  Bounded :: (Inclusive, a) -> (Inclusive, b) -> RealInterval a b -- ^ Interval from (x .. y).
  UpTo    :: (Inclusive, a) -> RealInterval a b                   -- ^ Interval from (-infinity .. x).
  UpFrom  :: (Inclusive, b) -> RealInterval a b                   -- ^ Interval from (x .. infinity).

-- Typed (RealInterval Expr Expr) Space
instance (Typed a Space) => Typed (RealInterval a a) Space where
  infer :: Typed a Space => TypingContext Space -> RealInterval a a -> Either Space TypeError
  infer cxt (Bounded (_, l) (_, r)) = case (infer cxt l, infer cxt r) of
    (Left Real, Right Real) -> Left Boolean
  infer cxt (UpTo (_, x0)) = case infer cxt x0 of
    Left Real -> Left Boolean
    Left sp   -> Right $ "Expression in 'real interval' not Real-typed, but `" ++ show sp ++ "`-typed"
    x         -> x
  infer cxt (UpFrom (_, x0)) = case infer cxt x0 of
    Left Real -> Left Boolean
    Left sp   -> Right $ "Expression in 'real interval' not Real-typed, but `" ++ show sp ++ "`-typed"
    x         -> x

-- | Gets the name of an 'Actor'.
getActorName :: Space -> String
getActorName (Actor n) = n
getActorName _ = error "getActorName called on non-actor space"

-- | Gets the inner 'Space' of a vector.
getInnerSpace :: Space -> Space
getInnerSpace (Vect s) = s
getInnerSpace _ = error "getInnerSpace called on non-vector space"
