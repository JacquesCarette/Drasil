module Data.Drasil.Constraints where

import Language.Drasil

gtZeroConstr, probConstr :: Constraint
gtZeroConstr = physc $ UpFrom  (Exc, Int 0)
probConstr   = physc $ Bounded (Inc, Int 0) (Inc, Int 1)
