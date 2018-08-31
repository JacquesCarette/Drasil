module Data.Drasil.Units.SolidMechanics where

import Data.Drasil.SI_Units (metre, newton, pascal)
import Language.Drasil (UnitDefn, new_unit, (/:))

stiffnessU, stiffness3D :: UnitDefn
stiffnessU  = new_unit "stiffness"    $ newton /: metre
stiffness3D = new_unit "3D stiffness" $ pascal /: metre
