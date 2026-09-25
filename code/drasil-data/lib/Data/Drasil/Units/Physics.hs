-- | Units related to the field of (classical) physics. Includes kinematics, forces, etc.
module Data.Drasil.Units.Physics where

import Data.Drasil.SI_Units (metre, radian, s_2, second, newton, kilogram,
  m_2, m_3, newton)
import Language.Drasil (cn, compoundUnit', UnitDefn, (/:), (/$), (*:), compoundUnit, cncpt''', Sentence(..))
import Drasil.Database (mkUid)

accelU, angVelU, angAccelU, forcePerMeterU, momtInertU, momentOfForceU,
 impulseU, springConstU, torqueU, velU :: UnitDefn

accelU          = compoundUnit' "acceleration"         $ metre /: s_2
angVelU         = compoundUnit' "angular velocity"     $ radian /: second
angAccelU       = compoundUnit' "angular acceleration" $ radian /: s_2
forcePerMeterU  = compoundUnit' "force per meter"      $ newton /: metre
impulseU        = compoundUnit' "impulse"              $ newton *: second
momtInertU      = compoundUnit' "moment of inertia"    $ kilogram *: m_2
momentOfForceU  = compoundUnit' "moment of force"      $ newton *: metre
springConstU    = compoundUnit' "spring constant"      $ newton /: metre
torqueU         = compoundUnit' "torque"               $ newton *: metre
velU            = compoundUnit' "velocity"             $ metre /: second

gravConstU :: UnitDefn

gravConstU = compoundUnit (cncpt''' (mkUid "gravConstU") (cn "gravitational constant")
  (S "universal gravitational constant")) (m_3 /$ (kilogram *: s_2))
