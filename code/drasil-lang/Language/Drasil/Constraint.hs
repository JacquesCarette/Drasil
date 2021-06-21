{-# Language GADTs #-}
module Language.Drasil.Constraint (
    Constraint(..), ConstraintReason(..)
  , physc, sfwrc, enumc, isPhysC, isSfwrC
  ) where

import Language.Drasil.Expr (Expr(..))
import Language.Drasil.Space (RealInterval(..))

-- | The reason behind the constraint's existence.
data ConstraintReason = Physical | Software
-- | Holds constraints. May occur between an interval of 'Expr', a list of 'Double's, or a list of 'String's.
data Constraint where
  -- | By default, physical and software constraints are ranges.
  Range          :: ConstraintReason -> RealInterval Expr Expr -> Constraint  -- TODO: parameterize the Expr Expr?
  EnumeratedReal :: ConstraintReason -> [Double]               -> Constraint  -- TODO: I think this can be merged with the below when we get typed expressions, `Expr a` instead of Double/String
  EnumeratedStr  :: ConstraintReason -> [String]               -> Constraint

-- | Smart constructor for range of 'Physical' constraints between two given expressions.
physc :: RealInterval Expr Expr -> Constraint
physc = Range Physical
-- | Smart constructor for range of 'Software' constraints between two given expressions.
sfwrc :: RealInterval Expr Expr -> Constraint
sfwrc = Range Software

-- but also for enumeration of values; right now, always physical
-- | Smart constructor for enumeration of values, default reason for constraint is 'Physical'.
enumc :: [Double] -> Constraint
enumc = EnumeratedReal Physical

isPhysC, isSfwrC :: Constraint -> Bool
-- | Helpful for filtering for Physical constraints. True if constraint is 'Physical'.
isPhysC (Range Physical _) = True
isPhysC (EnumeratedReal Physical _) = True
isPhysC (EnumeratedStr Physical _) = True
isPhysC _ = False
-- | Helpful for filtering for Software constraints. True if constraint is 'Software'.
isSfwrC (Range Software _) = True
isSfwrC (EnumeratedReal Software _) = True
isSfwrC (EnumeratedStr Software _) = True
isSfwrC _ = False
