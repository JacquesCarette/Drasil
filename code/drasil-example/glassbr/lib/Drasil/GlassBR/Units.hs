module Drasil.GlassBR.Units (units, sFlawPU) where

import Language.Drasil (UnitDefn, compoundUnit, cn', (^$), (^:))
import Data.Drasil.SI_Units (metre, newton)

units :: [UnitDefn]
units = [sFlawPU]

--N^(-7)*m^12--
sFlawPU :: UnitDefn
sFlawPU = compoundUnit "surface flaw parameter" (cn' "surface flaw parameter") "surface flaw parameter" $ m12 ^$ n7
  where
    m12 = metre ^: 12
    n7  = newton ^: (-7)
