{-# OPTIONS -Wall #-}
module ChipUnits where

import SI_Units
import Unit (Unit(..), UDefn(..), DerUChunk(..),
  makeDerU, unitCon)
import Chunk (ConceptChunk(..))
import Control.Lens ((^.))
import Spec (USymb(..), Sentence(..))

--s^2--
s_2 :: DerUChunk
s_2 = makeDerU (unitCon "seconds squared") s_2eqn

s_2eqn :: UDefn
s_2eqn = USynonym (UPow (second ^. unit) (2))

--m^3--
m_3 :: DerUChunk
m_3 = makeDerU (unitCon "cubic metres") m_3eqn

m_3eqn :: UDefn
m_3eqn = USynonym (UPow (metre ^. unit) (3))

--m/s--
velU :: DerUChunk
velU = makeDerU (unitCon "velocity") velU_eqn

velU_eqn :: UDefn
velU_eqn = USynonym (UDiv (metre ^. unit) (second ^. unit))

--m/s^2--
accelU :: DerUChunk
accelU = makeDerU (unitCon "acceleration") accelU_eqn

accelU_eqn :: UDefn
accelU_eqn = USynonym (UDiv (metre ^. unit) (s_2 ^. unit))

--rad/s--
angVelU :: DerUChunk
angVelU = makeDerU (unitCon "angular velocity") angVelU_eqn

angVelU_eqn :: UDefn
angVelU_eqn = USynonym (UDiv (radians ^. unit) (second ^. unit))

--rad/s^2--
angAccelU :: DerUChunk
angAccelU = makeDerU (unitCon "angular acceleration") angAccelU_eqn

angAccelU_eqn :: UDefn
angAccelU_eqn = USynonym (UDiv (radians ^. unit) (s_2 ^. unit))

--m^3/kgs^2--
gravConstU :: DerUChunk
gravConstU = makeDerU (unitCon "gravitational constant") gravConst_eqn

gravConst_eqn :: UDefn
gravConst_eqn = USynonym (UDiv (m_3 ^. unit)
                               (UProd [kilogram ^. unit, s_2 ^. unit]))

--m^2--
m_2 :: DerUChunk
m_2 = makeDerU (unitCon "square metres") m_2eqn

m_2eqn :: UDefn
m_2eqn = USynonym (UPow (metre ^. unit) (2))

--kgm^2--
momtInertU :: DerUChunk
momtInertU = makeDerU (unitCon "moment of inertia") momtInert_eqn

momtInert_eqn :: UDefn
momtInert_eqn = USynonym (UProd [kilogram ^. unit, m_2 ^. unit])

--kg/m^3--
densityU :: DerUChunk
densityU = makeDerU (unitCon "density") densityU_eqn

densityU_eqn :: UDefn
densityU_eqn = USynonym (UDiv (kilogram ^. unit) (m_3 ^. unit))

--Ns--

impulseU :: DerUChunk
impulseU = makeDerU (unitCon "impulse") impulseU_eqn

impulseU_eqn :: UDefn
impulseU_eqn = USynonym (UProd [newton ^. unit, second ^. unit])

--N/m--

springConstU :: DerUChunk
springConstU = makeDerU (unitCon "spring constant") springConstU_eqn

springConstU_eqn :: UDefn
springConstU_eqn = USynonym (UDiv (newton ^. unit) (metre ^. unit))

--Nm--

torqueU :: DerUChunk
torqueU = makeDerU (unitCon "torque") torqueU_eqn

torqueU_eqn :: UDefn
torqueU_eqn = USynonym (UProd [newton ^. unit, metre ^. unit])
