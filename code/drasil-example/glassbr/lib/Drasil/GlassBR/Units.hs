module Drasil.GlassBR.Units (sFlawPU) where

import Language.Drasil (UnitDefn, newUnit, (^$), (^:))
import Data.Drasil.SI_Units (metre, newton)

--N^(-7)*m^12--
sFlawPU :: UnitDefn
sFlawPU = newUnit "surface flaw parameter" $ m12 ^$ n7
  where
    m12 = metre ^: 12
    n7  = newton ^: (-7)
