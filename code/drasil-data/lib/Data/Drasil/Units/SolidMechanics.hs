-- | Units related to the field of solid mechanics.
module Data.Drasil.Units.SolidMechanics where

import Data.Drasil.SI_Units (metre, newton, pascal)
import Language.Drasil (UnitDefn, compoundUnit, cn', (/:))

stiffnessU, stiffness3D :: UnitDefn
stiffnessU  = compoundUnit "stiffness" (cn' "stiffness") "stiffness" $ newton /: metre
stiffness3D = compoundUnit "3D stiffness" (cn' "3D stiffness") "3D stiffness" $ pascal /: metre
