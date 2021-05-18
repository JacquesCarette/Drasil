{-# Language GADTs #-}
module Language.Drasil.Constraint (
    Constraint(..), ConstraintReason(..)
  , physc, sfwrc, enumc, isPhysC, isSfwrC
  ) where

import Language.Drasil.Expr (Expr(..))
import Language.Drasil.Space (RealInterval(..))

data ConstraintReason = Physical | Software
data Constraint where
  Range          :: ConstraintReason -> RealInterval Expr Expr -> Constraint
  EnumeratedReal :: ConstraintReason -> [Double]               -> Constraint
  EnumeratedStr  :: ConstraintReason -> [String]               -> Constraint

-- by default, physical and software constraints are ranges
-- | Range of 'Physical' constraints
physc :: RealInterval Expr Expr -> Constraint
physc = Range Physical
-- | Range of 'Software' constraints
sfwrc :: RealInterval Expr Expr -> Constraint
sfwrc = Range Software

-- but also for enumeration of values; right now, always physical
-- | Enumeration of values, default to 'Physical'
enumc :: [Double] -> Constraint
enumc = EnumeratedReal Physical

-- | Helpful for filtering for Physical / Software constraints
isPhysC, isSfwrC :: Constraint -> Bool
isPhysC (Range Physical _) = True
isPhysC (EnumeratedReal Physical _) = True
isPhysC (EnumeratedStr Physical _) = True
isPhysC _ = False

isSfwrC (Range Software _) = True
isSfwrC (EnumeratedReal Software _) = True
isSfwrC (EnumeratedStr Software _) = True
isSfwrC _ = False
