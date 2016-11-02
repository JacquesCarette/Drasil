module Data.Drasil.Quantities.Thermodynamics where

import Language.Drasil
import Data.Drasil.Concepts.Thermodynamics as CT

heat_cap_spec :: VarChunk

heat_cap_spec = vcFromCC CT.heat_cap_spec cC