module Data.Drasil.Quantities.Thermodynamics where

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Concepts.Thermodynamics as CT

heat_cap_spec :: ConVar

heat_cap_spec = cvR CT.heat_cap_spec cC

temperature :: UnitalChunk

temperature = uc CT.temperature cT centigrade