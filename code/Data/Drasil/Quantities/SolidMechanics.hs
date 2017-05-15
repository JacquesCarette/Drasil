module Data.Drasil.Quantities.SolidMechanics where

import Language.Drasil
import Data.Drasil.Concepts.SolidMechanics as CSM
import Data.Drasil.SI_Units

elastMod, shearRes :: UnitalChunk

elastMod = uc CSM.elastMod cE pascal
shearRes = uc CSM.shearRes cP newton