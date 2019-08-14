module Data.Drasil.Constraints where

import Language.Drasil

gtZeroConstr, probConstr :: Constraint
gtZeroConstr = physc $ UpFrom  (Exc, 0)
probConstr   = physc $ Bounded (Inc, 0) (Inc, 1)
