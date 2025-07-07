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
    -- | Clifford algebra objects (Clifs) with a dimension and kind (e.g., Scalar, Vector, Bivector, Multivector)
    ClifS     :: Dimension -> ClifKind -> Space -> Space
  deriving (Eq, Show)

-- | Kinds of Clifford algebra objects.
data ClifKind = Scalar | Vector | Bivector | Multivector
  deriving (Eq, Show)


-- Suggestion: In order to support things like Fixed (a + b) in the future, 
-- we want to use an Expr or symbolic type instead of Natural / String. 
-- Not needed now, but worth keeping in mind.

-- | The dimension of a clif
data Dimension where
  -- | Fixed dimension
  Fixed :: Natural -> Dimension
  -- | Variable dimension
  VDim  :: String -> Dimension
  deriving (Eq, Show)

-- Example of a 3D vector of real numbers using ClifS
-- exampleVector :: Space
-- exampleVector = ClifS (Fixed 3) Vector Real

-- A scalar (real number) using ClifS
-- exampleScalar :: Space
-- exampleScalar = ClifS (Fixed 1) Scalar Real

-- An n-dimensional multivector of real numbers:
-- ClifS (VDim "n") Multivector Real

-- TODO: check if non-real numbers in Clifs make any sense; allowing for now to avoid errors in offending examples
-- as we figure out matrices
-- | Only allow Real as the inner space for now.
checkClifSpace :: Space -> Bool
checkClifSpace Real = True
checkClifSpace _ = True --error $ "Non-real clif spaces unsupported"

-- | Helper that constructs a scalar Clifford object
scalarS :: Space -> Space
scalarS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 1) Scalar s

vect2DS :: Space -> Space
vect2DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 2) Vector s

vect3DS :: Space -> Space
vect3DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 3) Vector s

vectS :: Natural -> Space -> Space
vectS n s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed n) Vector s

vectNDS :: String -> Space -> Space
vectNDS x s | isBasicNumSpace s && checkClifSpace s = ClifS (VDim x) Vector s

-- | 2D bivector in Clifford algebra
bivector2DS :: Space -> Space
bivector2DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 2) Bivector s

-- | 3D bivector in Clifford algebra
bivector3DS :: Space -> Space
bivector3DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 3) Bivector s

-- | n-dimensional bivector in Clifford algebra
bivectorS :: Natural -> Space -> Space
bivectorS n s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed n) Bivector s

-- | n-dimensional bivector (symbolic dimension) in Clifford algebra
bivectorNDS :: String -> Space -> Space
bivectorNDS x s | isBasicNumSpace s && checkClifSpace s = ClifS (VDim x) Bivector s

-- | 2D multivector in Clifford algebra
multivector2DS :: Space -> Space
multivector2DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 2) Multivector s

-- | 3D multivector in Clifford algebra
multivector3DS :: Space -> Space
multivector3DS s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed 3) Multivector s

-- | n-dimensional multivector in Clifford algebra
multivectorS :: Natural -> Space -> Space
multivectorS n s | isBasicNumSpace s && checkClifSpace s = ClifS (Fixed n) Multivector s

-- | n-dimensional multivector (symbolic dimension) in Clifford algebra
multivectorNDS :: String -> Space -> Space
multivectorNDS x s | isBasicNumSpace s && checkClifSpace s = ClifS (VDim x) Multivector s

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
isBasicNumSpace ClifS {}     = False
