-- | Defines common constraints for use in Drasil.
module Data.Drasil.Constraints where

import Language.Drasil

gtZeroConstr, probConstr :: ConstraintE
gtZeroConstr = physc $ UpFrom  (Exc, exactDbl 0)
probConstr   = physc $ Bounded (Inc, exactDbl 0) (Inc, exactDbl 1)
