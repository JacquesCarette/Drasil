module Drasil.GamePhysics.Units where

import Language.Drasil
import Data.Drasil.SI_Units

import Control.Lens ((^.))

----- Table of Units -----

cpSIUnits :: [UnitDefn]
cpSIUnits = map UU [metre, kilogram, second] ++ map UU [newton, radians]

----- Derived Units -----

s_2 :: DerUChunk
s_2 = new_unit "seconds squared" $ second ^: 2

m_2, m_3 :: DerUChunk
m_2 = new_unit "square metres"   $ metre ^: 2
m_3 = new_unit "cubic metres"    $ metre ^: 3

velU, accelU, angVelU, angAccelU, momtInertU, densityU :: DerUChunk
velU   = new_unit "velocity"     $ metre /: second
accelU = new_unit "acceleration" $ metre /: s_2

angVelU      = new_unit "angular velocity"     $ radians /: second
angAccelU    = new_unit "angular acceleration" $ radians /: s_2
momtInertU   = new_unit "moment of inertia"    $ kilogram *: s_2
densityU     = new_unit "density"              $ kilogram /: m_3

impulseU, springConstU, torqueU :: DerUChunk
impulseU     = new_unit "impulse"              $ newton *: second
springConstU = new_unit "spring constant"      $ newton /: metre
torqueU      = new_unit "torque"               $ newton *: metre

--m^3/kgs^2--
gravConstU :: DerUChunk
gravConstU = makeDerU (unitCon "gravitational constant") gravConst_eqn

gravConst_eqn :: UDefn
gravConst_eqn = USynonym (UDiv (m_3 ^. unit)
                               (UProd [kilogram ^. unit, s_2 ^. unit]))

