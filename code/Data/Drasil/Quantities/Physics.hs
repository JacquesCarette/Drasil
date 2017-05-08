module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Data.Drasil.Concepts.Physics as CP
import Control.Lens((^.)) --need for parametrization hack

surface, acceleration, angularAccel, restitutionCoef, force, momentOfInertia,
  impulseS, impulseV, gravitationalAccel, gravitationalConst, displacement,
  angularVelocity, position, distance, angularDisplacement,time, torque, 
  linearDisplacement, velocity, linearVelocity, linearAccel :: ConVar

surface = cvR CP.surface cS --Maybe should be physical property?
restitutionCoef = cvR CP.restitutionCoef (sub cC cR)

force = cvR CP.force (vec cF)
momentOfInertia = cvR CP.momentOfInertia (vec cI)
impulseV = cvR CP.impulseV (vec cJ)
impulseS = cvR CP.impulseS lJ
position = cvR CP.position (vec lP)
distance = cvR CP.distance lR
displacement = cvR CP.displacement (vec lR)
velocity = cvR CP.velocity (vec lV)
acceleration = cvR CP.acceleration (vec lA)
--FIXME: COMBINATION HACK
angularDisplacement = cvR angDisp (Greek Theta_L)
  where angDisp = dcc "angularDisplacement" (compoundPhrase' (CP.angular ^. term) (CP.displacement ^. term)) "fixme"
--FIXME: COMBINATION HACK
angularVelocity = cvR angVelo (Greek Omega_L)
  where angVelo = dcc "angularVelocity" (compoundPhrase' (CP.angular ^. term) (CP.velocity ^. term)) "fixme"
--FIXME: COMBINATION HACK
angularAccel = cvR angAccel (Greek Alpha_L)
  where angAccel = dcc "angularAcceleration" (compoundPhrase' (CP.angular ^. term) (CP.acceleration ^. term)) "fixme"
--FIXME: COMBINATION HACK
linearDisplacement = cvR linDisp (Concat [(vec lR), Atomic "(",lT, Atomic ")"])
  where linDisp = dcc "linearDisplacement" (compoundPhrase' (CP.linear ^. term) (CP.displacement ^. term)) "fixme"
--FIXME: COMBINATION HACK
linearVelocity = cvR linVelo (Concat [(vec lV), Atomic "(", lT, Atomic ")"])
  where linVelo = dcc "linearVelocity" (compoundPhrase' (CP.linear ^. term) (CP.velocity ^. term)) "fixme"
--FIXME: COMBINATION HACK
linearAccel = cvR linAccel (Concat [(vec lA), Atomic "(", lT, Atomic ")"])
  where linAccel = dcc "linearAcceleration" (compoundPhrase' (CP.linear ^. term) (CP.acceleration ^. term)) "fixme"

time = cvR CP.time lT
torque = cvR CP.torque (Greek Tau_L)



gravitationalAccel = cvR CP.gravitationalAccel lG
gravitationalConst = cvR CP.gravitationalConst cG
