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
  getActorName, getInnerType, mkFunction, isBasicNumSpace,
  Dimension(..), scalarS, vect2DS, vect3DS, vectS, vectNDS, 
  bivector2DS, bivector3DS, bivectorS, bivectorNDS,
  multivector2DS, multivector3DS, multivectorS, multivectorNDS,
  ClifKind(..)

) where

import qualified Data.List.NonEmpty        as NE

import           Control.Lens              (Getter)
import           Language.Drasil.Symbol    (Symbol)
import           Numeric.Natural           (Natural)


-- FIXME: These need to be spaces and not just types.

-- | The difference kinds of spaces that may exist. This type holds
-- numerical spaces (such as the set of integers, rationals, etc.),
-- a space for booleans, a space for characters, dimensional spaces (vectors, arrays, etc.),
-- a space for Actors, discrete sets (both for numbers and strings), and a void space.
data Space where
    Integer   :: Space
    Rational  :: Space
    Real      :: Space
    Natural   :: Space
    Boolean   :: Space
    Char      :: Space
    String    :: Space
    Set       :: Space -> Space
    Matrix    :: Int -> Int -> Space -> Space
    Array     :: Space -> Space
    Actor     :: String -> Space
    Function  :: (NE.NonEmpty Primitive) -> Primitive -> Space
    Void      :: Space
    ClifS     :: Dimension -> ClifKind -> Space -> Space
  deriving (Eq, Show)

data ClifKind = Scalar | Vector | Bivector | Multivector 
  deriving (Eq, Show)

data Dimension where
  Fixed :: Natural -> Dimension
  VDim  :: String -> Dimension
  deriving (Eq, Show)

checkClifSpace :: Space -> Bool
checkClifSpace Real = True
checkClifSpace _ = True --error $ "Non-real clif spaces unsupported"

-- | Helper that constructs a scalar Clifford object
scalarS :: Space -> Space
scalarS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 1) Scalar s
scalarS s = s  -- fallback: leave the space unchanged 

vect2DS :: Space -> Space
vect2DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 2) Vector s
vect2DS s = s

vect3DS :: Space -> Space
vect3DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 3) Vector s
vect3DS s = s

vectS :: Natural -> Space -> Space
vectS n s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed n) Vector s
vectS _ s = s

vectNDS :: String -> Space -> Space
vectNDS x s | isBasicNumSpace s && checkClifSpace s = ClifS (VDim x) Vector s
vectNDS _ s = s

bivector2DS :: Space -> Space
bivector2DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 2) Bivector s
bivector2DS s = s

bivector3DS :: Space -> Space
bivector3DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 3) Bivector s
bivector3DS s = s

bivectorS :: Natural -> Space -> Space
bivectorS n s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed n) Bivector s
bivectorS _ s = s

bivectorNDS :: String -> Space -> Space
bivectorNDS x s | isBasicNumSpace s && checkClifSpace s = ClifS (VDim x) Bivector s
bivectorNDS _ s = s

multivector2DS :: Space -> Space
multivector2DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 2) Multivector s
multivector2DS s = s

multivector3DS :: Space -> Space
multivector3DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 3) Multivector s
multivector3DS s = s

multivectorS :: Natural -> Space -> Space
multivectorS n s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed n) Multivector s
multivectorS _ s = s

multivectorNDS :: String -> Space -> Space
multivectorNDS x s | isBasicNumSpace s && checkClifSpace s = ClifS (VDim x) Multivector s
multivectorNDS _ s = s

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

-- | Gets the inner 'Type' 
getInnerType :: Space -> Space
getInnerType (Set s) = s
getInnerType (Array s)      = s
getInnerType (Matrix _ _ s) = s
getInnerType (ClifS _ _ s) = s
getInnerType _        = error "getInnerType called on non-vector space"

-- | Is this Space a basic numeric space?
isBasicNumSpace :: Space -> Bool
isBasicNumSpace Integer      = True
isBasicNumSpace Rational     = True
isBasicNumSpace Real         = True
isBasicNumSpace Natural      = True
isBasicNumSpace Boolean      = False
isBasicNumSpace Char         = False
isBasicNumSpace String       = False
isBasicNumSpace Set {}       = False
isBasicNumSpace Matrix {}    = False
isBasicNumSpace Array {}     = False
isBasicNumSpace Actor {}     = False
isBasicNumSpace Function {}  = False
isBasicNumSpace Void         = False
isBasicNumSpace (ClifS _ _ s) = isBasicNumSpace s