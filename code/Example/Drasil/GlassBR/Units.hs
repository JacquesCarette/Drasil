module Drasil.GlassBR.Units (sFlawPU) where

import Language.Drasil
import Data.Drasil.SI_Units

--N^(-7)*m^12--
sFlawPU :: DerUChunk
sFlawPU = new_unit "surface flaw parameter" $ UProd [newton ^: (-7), metre ^: 12]
