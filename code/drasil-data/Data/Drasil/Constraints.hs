module Data.Drasil.Constraints where

import Language.Drasil

gtZeroConstr, probConstr :: Constraint
gtZeroConstr = physc $ UpFrom  (Exc, dbl 0)
probConstr   = physc $ Bounded (Inc, dbl 0) (Inc, dbl 1)
