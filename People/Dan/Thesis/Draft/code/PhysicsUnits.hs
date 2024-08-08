-- | Units related to the field of (classical) physics. Includes kinematics, forces, etc.
module Data.Drasil.Units.Physics where

import Data.Drasil.SI_Units (metre, radian, s_2, second, newton, kilogram,
  m_2, m_3, newton)
import Language.Drasil (cn, dcc, newUnit, UnitDefn, (/:), (/$), (*:), makeDerU)

accelU, angVelU, angAccelU, forcePerMeterU, momtInertU, momentOfForceU, 
 impulseU, springConstU, torqueU, velU :: UnitDefn

accelU          = newUnit "acceleration"         $ metre /: s_2
angVelU         = newUnit "angular velocity"     $ radian /: second
angAccelU       = newUnit "angular acceleration" $ radian /: s_2
forcePerMeterU  = newUnit "force per meter"      $ newton /: metre
impulseU        = newUnit "impulse"              $ newton *: second
momtInertU      = newUnit "moment of inertia"    $ kilogram *: m_2
momentOfForceU  = newUnit "moment of force"      $ newton *: metre
springConstU    = newUnit "spring constant"      $ newton /: metre
torqueU         = newUnit "torque"               $ newton *: metre
velU            = newUnit "velocity"             $ metre /: second

gravConstU :: UnitDefn

gravConstU = makeDerU (dcc "gravConstU" (cn "gravitational constant")
  "universal gravitational constant") (m_3 /$ (kilogram *: s_2))
