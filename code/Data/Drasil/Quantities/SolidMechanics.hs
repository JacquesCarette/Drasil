module Data.Drasil.Quantities.SolidMechanics where

import Language.Drasil
import Data.Drasil.Concepts.Physics as CP
import Data.Drasil.Concepts.SolidMechanics as CSM
import Data.Drasil.SI_Units

elastMod :: UnitalChunk

elastMod = uc CSM.elastMod cE pascal