{-# Language GADTs #-}
-- | Defines types and functions for constrained values.
module Language.Drasil.Constraint (
  -- * Types
  Constraint(..), ConstraintE, ConstraintReason(..),
  -- * Functions
  physRange, sfwrRange, physElem, sfwrElem, isPhysC, isSfwrC
  ) where

import Language.Drasil.Expr.Lang
import Language.Drasil.Space (RealInterval(..), Space(..))

-- | The reason behind the constraint's existence.
data ConstraintReason = Physical | Software

-- | Type synonym for 'ConstraintE'
type ConstraintE = Constraint Expr

-- | Holds constraints. May occur between an interval of 'Expr', a list of 'Double's, or a list of 'String's.
data Constraint a where
  -- | By default, physical and software constraints are ranges.
  Range          :: ConstraintReason -> RealInterval a a -> Constraint a

  Elem           :: ConstraintReason -> [Expr] -> Constraint a

-- | Smart constructor for range of 'Physical' constraints between two given expressions.
physRange :: RealInterval Expr Expr -> ConstraintE
physRange = Range Physical

physElem :: [Expr] -> ConstraintE
physElem = Elem Physical

-- | Smart constructor for range of 'Software' constraints between two given expressions.
sfwrRange :: RealInterval Expr Expr -> ConstraintE
sfwrRange = Range Software

sfwrElem :: [Expr] -> ConstraintE
sfwrElem = Elem Software

isPhysC, isSfwrC:: Constraint e -> Bool

-- | Helpful for filtering for Physical constraints. True if constraint is 'Physical'.
isPhysC (Range Physical _) = True
isPhysC (Elem Physical _) = True
isPhysC _ = False

-- | Helpful for filtering for Software constraints. True if constraint is 'Software'.
isSfwrC (Range Software _) = True
isSfwrC (Elem Software _) = True
isSfwrC _ = False
