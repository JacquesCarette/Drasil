module Data.Drasil.Units.SolidMechanics where

import Language.Drasil
import Data.Drasil.SI_Units

stiffnessU, stiffness3D :: DerUChunk
stiffnessU  = new_unit "stiffness"    $ newton /: metre
stiffness3D = new_unit "3D stiffness" $ pascal /: metre
