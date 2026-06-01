-- | Units related to the field of solid mechanics.
module Data.Drasil.Units.SolidMechanics where

import Data.Drasil.SI_Units (metre, newton, pascal)
import Language.Drasil (UnitDefn, newUnit, (/:))

stiffnessU, stiffness3D :: UnitDefn
stiffnessU  = newUnit "stiffness"    $ newton /: metre
stiffness3D = newUnit "3D stiffness" $ pascal /: metre
