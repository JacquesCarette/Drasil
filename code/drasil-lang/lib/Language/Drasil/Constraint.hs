{-# Language GADTs #-}
-- | Defines types and functions for constrained values.
module Language.Drasil.Constraint (
  -- * Types
  Constraint(..), ConstraintE, ConstraintReason(..),
  -- * Functions
  physc, sfwrc, elem, isPhysC, isSfwrC
  ) where

import Language.Drasil.Expr.Lang
import Language.Drasil.Space (RealInterval(..), Set(..))

-- | The reason behind the constraint's existence.
data ConstraintReason = Physical | Software

-- | Type synonym for 'ConstraintE'
type ConstraintE = Constraint Expr

-- | Holds constraints. May occur between an interval of 'Expr', a list of 'Double's, or a list of 'String's.
data Constraint a where
  -- | By default, physical and software constraints are ranges.
  Range          :: ConstraintReason -> RealInterval a a -> Constraint a

  Elem           :: ConstraintReason -> Set a -> Constraint a

-- | Smart constructor for range of 'Physical' constraints between two given expressions.
physc :: RealInterval Expr Expr -> ConstraintE
physc = Range Physical

constrElem :: Set Expr -> ConstraintE
constrElem = Elem Physical

-- | Smart constructor for range of 'Software' constraints between two given expressions.
sfwrc :: RealInterval Expr Expr -> ConstraintE
sfwrc = Range Software

isPhysC, isSfwrC, isConstrElem :: Constraint e -> Bool

-- | Helpful for filtering for Physical constraints. True if constraint is 'Physical'.
isPhysC (Range Physical _) = True
isPhysC _ = False

-- | Helpful for filtering for Software constraints. True if constraint is 'Software'.
isSfwrC (Range Software _) = True
isSfwrC _ = False

isConstrElem (Elem Physical _) = True
isConstrElem _ = False
