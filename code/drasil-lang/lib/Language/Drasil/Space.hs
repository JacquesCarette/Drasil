{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Number space types and functions.
module Language.Drasil.Space (
  -- * Types
  Space(..), Primitive,
  RealInterval(..), Inclusive(..),
  DomainDesc(..), RTopology(..), DiscreteDomainDesc, ContinuousDomainDesc,
  -- * Class
  HasSpace(..),
  -- * Functions
  getActorName, getInnerSpace, mkFunction, isBasicNumSpace,
  assertReal, assertNumeric, assertNonNatNumeric, assertEquivNumeric, assertIndexLike, assertBoolean,
  assertSet,
  assertVector, assertNumericVector, assertNonNatNumVector, assertRealVector,
  assertFunction, assertNonFunction
) where

import qualified Data.List.NonEmpty        as NE

import           Control.Lens              (Getter)
import           Language.Drasil.Symbol    (Symbol)

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
  | Vect Space -- TODO: Length for vectors?
  | Set Space
  | Matrix Int Int Space
  | Array Space
  | Actor String 
  | Function (NE.NonEmpty Primitive) Primitive
  | Void
  deriving (Eq, Show)

-- | HasSpace is anything which has a 'Space'.
class HasSpace c where
  -- | Provides a 'Getter' to the 'Space'.
  typ      :: Getter c Space

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

-- | Gets the name of an 'Actor'.
getActorName :: Space -> String
getActorName (Actor n) = n
getActorName _         = error "getActorName called on non-actor space"

-- | Gets the inner 'Space' of a vector or set.
getInnerSpace :: Space -> Space
getInnerSpace (Vect s) = s
getInnerSpace (Set s) = s
getInnerSpace _        = error "getInnerSpace called on non-vector space"

-- | Is this Space a basic numeric space?
isBasicNumSpace :: Space -> Bool
isBasicNumSpace Integer     = True
isBasicNumSpace Rational    = True
isBasicNumSpace Real        = True
isBasicNumSpace Natural     = True
isBasicNumSpace Boolean     = False
isBasicNumSpace Char        = False
isBasicNumSpace String      = False
isBasicNumSpace Set {}      = False
isBasicNumSpace Vect {}     = False
isBasicNumSpace Matrix {}   = False
isBasicNumSpace Array {}    = False
isBasicNumSpace Actor {}    = False
isBasicNumSpace Function {} = False
isBasicNumSpace Void        = False

-- | Assert that a 'Space' is 'Real' or return a formatted error message.
assertReal :: Space -> (String -> String) -> Either String ()
assertReal Real _   = Right ()
assertReal s    msg = Left $ msg $ show s

-- | Assert that a 'Space' is "numeric" or return a formatted error message.
assertNumeric :: Space -> (String -> String) -> Either String ()
assertNumeric s msg
  | isBasicNumSpace s = Right ()
  | otherwise         = Left $ msg $ show s

-- | Assert that a numeric 'Space' is not 'Natural' or return a formatted error
-- message.
assertNonNatNumeric :: Space -> (String -> String) -> Either String ()
assertNonNatNumeric s msg
  | isBasicNumSpace s && s /= Natural = Right ()
  | otherwise                         = Left $ msg $ show s

-- | Assert that two numeric 'Space's are equivalent or return a formatted error
-- message.
assertEquivNumeric ::  Space -> Space -> (String -> String -> String) -> Either String ()
assertEquivNumeric l r msg 
  | isBasicNumSpace l && l == r = Right ()
  | otherwise                   = Left $ msg (show l) (show r)

-- | Assert that a 'Space' is an index-like type (Integer or Natural) or return a
-- formatted error message.
assertIndexLike :: Space -> (String -> String) -> Either String ()
assertIndexLike Integer _   = Right ()
assertIndexLike Natural _   = Right ()
assertIndexLike t       msg = Left $ msg $ show t

-- | Assert that a 'Space' is a 'Boolean' or return a formatted error message.
assertBoolean :: Space -> (String -> String) -> Either String ()
assertBoolean Boolean _   = Right ()
assertBoolean sp      msg = Left $ msg $ show sp

-- | Assert that a 'Space' is a 'Set' and return either the element type or a
-- formatted error message.
assertSet :: Space -> (String -> String) -> Either String Space
assertSet (Set t) _   = Right t
assertSet s       msg = Left $ msg $ show s

-- | Assert that a 'Space' is a 'Vect' and return either the element type or a
-- formatted error message.
assertVector :: Space -> (String -> String) -> Either String Space
assertVector (Vect t) _   = Right t
assertVector s        msg = Left $ msg $ show s

-- | Assert that a 'Space' is a numeric vector (i.e., a vector of a basic
-- numeric type) and return either the numeric type or a formatted error
-- message.
assertNumericVector :: Space -> (String -> String) -> Either String Space
assertNumericVector (Vect t) _ | isBasicNumSpace t = Right t
assertNumericVector s msg                          = Left $ msg $ show s

-- | Assert that a 'Space' is a non-'Natural' numeric vector and return either
-- the numeric type or a formatted error message.
assertNonNatNumVector :: (String -> String) -> Space -> Either String Space
assertNonNatNumVector msg vn@(Vect Natural)              = Left $ msg $ show vn
assertNonNatNumVector _   (Vect et) | isBasicNumSpace et = Right et
assertNonNatNumVector msg t                              = Left $ msg $ show t

-- | Assert that a 'Space' is a 'Vect' of 'Real's or return a formatted error
-- message.
assertRealVector :: Space -> (String -> String) -> Either String ()
assertRealVector (Vect Real) _   = Right ()
assertRealVector t           msg = Left $ msg $ show t

-- | Assert that a 'Space' is a 'Function', returning either the parameters and
-- output type or a formatted error message.
assertFunction :: Space -> (String -> String) -> Either String (NE.NonEmpty Primitive, Primitive)
assertFunction (Function params out) _   = Right (params, out)
assertFunction s                     msg = Left $ msg $ show s

-- | Assert that a 'Space' is anything but a 'Function' or return a formatted
-- error message.
assertNonFunction :: Space -> (String -> String) -> Either String ()
assertNonFunction f@Function{} msg = Left $ msg $ show f
assertNonFunction _            _   = Right ()
