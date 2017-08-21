module Drasil.GlassBR.Units where --whole file is used

import Language.Drasil
import Data.Drasil.SI_Units

--N^(-7)*m^12--
sFlawPU :: DerUChunk
sFlawPU = new_unit "surface flaw parameter" $ 
  UDiv (metre ^: 12) (newton ^: (7))