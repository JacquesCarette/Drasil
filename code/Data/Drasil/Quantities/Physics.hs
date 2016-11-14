module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Data.Drasil.Concepts.Physics as CP

surface, acceleration, angularAccel, restitutionCoef, force, momentOfInertia,
  impulseS, impulseV, gravitationalAccel, gravitationalConst, 
  angularV :: VarChunk

surface = vcFromCC CP.surface cS --Maybe should be physical property?
restitutionCoef = vcFromCC CP.restitutionCoef (sub cC cR)
acceleration = vcFromCC CP.acceleration (vec lA)
angularAccel = vcFromCC CP.angularAccel (Greek Alpha_L)
angularV = vcFromCC CP.angularV (Greek Omega_L)
force = vcFromCC CP.force (vec cF)
momentOfInertia = vcFromCC CP.momentOfInertia (vec cI)
impulseV = vcFromCC CP.impulseV (vec cJ)
impulseS = vcFromCC CP.impulseS lJ

gravitationalAccel = vcFromCC CP.gravitationalAccel lG
gravitationalConst = vcFromCC CP.gravitationalConst cG
