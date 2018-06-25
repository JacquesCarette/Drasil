module Data.Drasil.Units.Physics where

import Data.Drasil.SI_Units (metre, radian, s_2, second, newton, kilogram,
  m_2, m_3, newton)
import Language.Drasil (new_unit, UnitDefn, (/:), (/$), (*:), cn, UDefn(..)
 , dcc, makeDerU')

accelU, angVelU, angAccelU, momtInertU, momentOfForceU, impulseU, springConstU, torqueU, velU :: UnitDefn

accelU          = new_unit "acceleration"         $ metre /: s_2
angVelU         = new_unit "angular velocity"     $ radian /: second
angAccelU       = new_unit "angular acceleration" $ radian /: s_2
impulseU        = new_unit "impulse"              $ newton *: second
momtInertU      = new_unit "moment of inertia"    $ kilogram *: m_2
momentOfForceU  = new_unit "moment of force"      $ newton *: metre
springConstU    = new_unit "spring constant"      $ newton /: metre
torqueU         = new_unit "torque"               $ newton *: metre
velU            = new_unit "velocity"             $ metre /: second

gravConstU :: UnitDefn

gravConstU = makeDerU' (dcc "gravConstU" (cn "gravitational constant")
  "universal gravitational constant") (m_3 /$ (kilogram *: s_2))
