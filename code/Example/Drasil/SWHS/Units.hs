module Drasil.SWHS.Units where

import Language.Drasil
import Data.Drasil.SI_Units

--W/m^3--
volHtGenU :: DerUChunk
volHtGenU = makeDerU (dcc "volHtGenU" "volumetric heat generation" 
  "the rate of heat energy generation per unit volume") $ USynonym (watt /: m_3)
