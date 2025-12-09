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
  ClifKind(..), realVect, vecDim, vect2D, vect3D

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
    Vect      :: Space -> Space
  deriving (Eq, Show)

-- | Kinds of Clifford algebra objects.
data ClifKind = Scalar | Vector | Bivector | Multivector
  deriving (Eq, Show)

-- | The dimension of a clif
data Dimension where
  -- | Fixed dimension
  Fixed :: Natural -> Dimension
  -- | Variable dimension
  VDim  :: String -> Dimension
  deriving (Eq, Show)
checkClifSpace :: Space -> Bool
checkClifSpace Real = True
checkClifSpace _ = True --error $ "Non-real clif spaces unsupported"

-- | Helper function to create Clifford vector spaces of a given dimension
realVect :: Dimension -> Space
realVect d = ClifS d Vector Real

-- | Common dimension constants
vecDim :: Dimension
vecDim = Fixed 2

-- | Direct 2D vector space (optimized)
vect2D :: Space
vect2D = ClifS (Fixed 2) Vector Real

-- | Direct 3D vector space (optimized)
vect3D :: Space
vect3D = ClifS (Fixed 3) Vector Real
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
isBasicNumSpace (Vect s)     = isBasicNumSpace s

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
