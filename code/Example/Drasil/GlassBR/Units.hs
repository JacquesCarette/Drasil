module Drasil.GlassBR.Units where --whole file is used

import Language.Drasil
import Data.Drasil.SI_Units

--N^(-7)*m^12--
sFlawPU :: DerUChunk
sFlawPU = new_unit "surface flaw parameter" $ US (m12 ++ n7)
  where
    US m12 = metre  ^: 12
    US n7  = newton ^: (-7)
