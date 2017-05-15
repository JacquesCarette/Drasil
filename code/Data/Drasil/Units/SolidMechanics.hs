module Data.Drasil.Units.SolidMechanics where

import Language.Drasil.Unit (new_unit)
import Language.Drasil
import Data.Drasil.SI_Units


stiffnessU :: DerUChunk

stiffnessU   = new_unit "stiffness"            $ newton /: metre