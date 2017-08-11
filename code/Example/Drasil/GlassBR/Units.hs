module Drasil.GlassBR.Units (sFlawPU) where

import Language.Drasil (DerUChunk, USymb (UDiv), new_unit, (^:))
import Data.Drasil.SI_Units (metre, newton)

--N^(-7)*m^12--
sFlawPU :: DerUChunk
sFlawPU = new_unit "surface flaw parameter" $ 
  UDiv (metre ^: 12) (newton ^: (7))