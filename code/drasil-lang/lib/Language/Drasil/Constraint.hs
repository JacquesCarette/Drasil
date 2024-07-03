{-# Language GADTs #-}
-- | Defines types and functions for constrained values.
module Language.Drasil.Constraint (
  -- * Types
  Constraint(..), ConstraintE, ConstraintReason(..),
  -- * Functions
  physc, sfwrc, isPhysC, isSfwrC
  ) where

import Language.Drasil.Expr.Lang
import Language.Drasil.Space (RealInterval(..))
import Data.Set (Set)

-- | The reason behind the constraint's existence.
data ConstraintReason = Physical | Software

-- | Type synonym for 'ConstraintE'
type ConstraintE = Constraint Expr

-- | Holds constraints. May occur between an interval of 'Expr', a list of 'Double's, or a list of 'String's.
data Constraint a where
  -- | By default, physical and software constraints are ranges.
  Range          :: ConstraintReason -> RealInterval a a -> Constraint a

  Elem :: ConstraintReason -> Set a -> Constraint a

-- | Smart constructor for range of 'Physical' constraints between two given expressions.
physc :: RealInterval Expr Expr -> ConstraintE
physc = Range Physical

enumc :: Set Expr -> ConstraintE
enumc = Elem Physical

-- | Smart constructor for range of 'Software' constraints between two given expressions.
sfwrc :: RealInterval Expr Expr -> ConstraintE
sfwrc = Range Software

isPhysC, isSfwrC :: Constraint e -> Bool

-- | Helpful for filtering for Physical constraints. True if constraint is 'Physical'.
isPhysC (Range Physical _) = True
isPhysC (Elem Physical _) = True
isPhysC _ = False

-- | Helpful for filtering for Software constraints. True if constraint is 'Software'.
isSfwrC (Range Software _) = True
isPhysC (Elem Software _) = True
isSfwrC _ = False
