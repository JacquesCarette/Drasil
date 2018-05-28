module Drasil.GlassBR.Units(sFlawPU) where

import Language.Drasil
import Data.Drasil.SI_Units

--N^(-7)*m^12--
sFlawPU :: UnitDefn
sFlawPU = new_unit "surface flaw parameter" $ US (m12 ++ n7)
  where
    US m12 = metre  ^: 12
    US n7  = newton ^: (-7)
