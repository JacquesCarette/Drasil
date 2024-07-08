-- | Defines common constraints for use in Drasil.
module Data.Drasil.Constraints where

import Language.Drasil

gtZeroConstr, probConstr :: ConstraintE
gtZeroConstr = physRange $ UpFrom  (Exc, exactDbl 0)
probConstr   = physRange $ Bounded (Inc, exactDbl 0) (Inc, exactDbl 1)
