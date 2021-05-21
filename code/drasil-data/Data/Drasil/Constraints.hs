module Data.Drasil.Constraints where

import Language.Drasil

gtZeroConstr, probConstr :: Constraint
gtZeroConstr = physc $ UpFrom  (Exc, exactDbl 0)
probConstr   = physc $ Bounded (Inc, exactDbl 0) (Inc, exactDbl 1)
