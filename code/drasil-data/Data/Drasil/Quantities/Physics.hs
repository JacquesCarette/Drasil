module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Language.Drasil.ShortHands
import qualified Data.Drasil.Concepts.Physics as CP (acceleration, angAccel,
  angDisp, angVelo, chgInVelocity, constAccel, constAccelV, displacement,
  distance, energy, fSpeed, fVel, force, gravitationalAccel, gravitationalConst,
  height, iPos, iSpeed, iVel, impulseS, impulseV, ixPos, ixVel, iyPos, iyVel,
  kEnergy, linAccel, linDisp, linVelo, momentOfInertia, position, pressure,
  restitutionCoef, scalarAccel, scalarPos, speed, time, torque, velocity,
  weight, xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist, 
  yPos, yVel)
import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, second)
import Data.Drasil.Units.Physics (accelU, angAccelU, angVelU, gravConstU, 
    impulseU, momtInertU, torqueU, velU)

restitutionCoef :: DefinedQuantityDict
restitutionCoef = dqd' CP.restitutionCoef (const $ sub cC cR) Real Nothing

physicscon :: [UnitalChunk]
physicscon = [acceleration, angularAccel, angularDisplacement, angularVelocity,
  chgInVelocity, constAccel, constAccelV, displacement, distance, energy,
  fSpeed, fVel, force, gravitationalAccel, gravitationalConst, height, iPos,
  iSpeed, iVel, impulseS, impulseV, ixPos, ixVel, iyPos, iyVel, kEnergy,
  linearAccel, linearDisplacement, linearVelocity, momentOfInertia, position,
  pressure, scalarAccel, scalarPos, speed, time, torque, velocity, weight,
  xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist, yPos, yVel]

acceleration, angularAccel, angularDisplacement, angularVelocity, chgInVelocity,
  constAccel, constAccelV, displacement, distance, energy, fSpeed, fVel, force,
  gravitationalAccel, gravitationalConst, height, iPos, iSpeed, iVel, impulseS,
  impulseV, ixPos, ixVel, iyPos, iyVel, kEnergy, linearAccel, linearDisplacement,
  linearVelocity, momentOfInertia, position, pressure, scalarAccel, scalarPos,
  speed, time, torque, velocity, weight, xAccel, xConstAccel, xDist, xPos, xVel,
  yAccel, yConstAccel, yDist, yPos, yVel :: UnitalChunk

acceleration         = uc CP.acceleration (vec lA) accelU
angularAccel         = uc CP.angAccel lAlpha angAccelU
angularDisplacement  = uc CP.angDisp lTheta radian
angularVelocity      = uc CP.angVelo lOmega angVelU
chgInVelocity        = uc CP.chgInVelocity (Concat [cDelta, vec lV]) velU
constAccel           = uc CP.constAccel (sup lA lC) accelU
displacement         = uc CP.displacement (vec lR) metre
distance             = uc CP.distance lR metre
energy               = uc CP.energy cE joule
force                = uc CP.force (vec cF) newton
gravitationalAccel   = uc CP.gravitationalAccel lG accelU
gravitationalConst   = uc CP.gravitationalConst cG gravConstU
height               = uc CP.height lH metre
impulseS             = uc CP.impulseS lJ impulseU
impulseV             = uc CP.impulseV (vec cJ) impulseU
kEnergy              = uc CP.kEnergy (Concat [cK, cE]) joule
linearAccel          = uc CP.linAccel (Concat [vec lA, Atomic "(", lT, Atomic ")"]) accelU
linearDisplacement   = uc CP.linDisp (Concat [vec lR, Atomic "(",lT, Atomic ")"]) metre
linearVelocity       = uc CP.linVelo (Concat [vec lV, Atomic "(", lT, Atomic ")"]) velU
momentOfInertia      = uc CP.momentOfInertia (vec cI) momtInertU
position             = uc CP.position (vec lP) metre
pressure             = uc CP.pressure lP pascal
speed                = uc CP.speed lV velU
scalarAccel          = uc CP.scalarAccel lA accelU
scalarPos            = uc CP.scalarPos lP metre
time                 = uc CP.time lT second
torque               = uc CP.torque lTau torqueU
velocity             = uc CP.velocity (vec lV) velU
weight               = uc CP.weight cW newton

-- Variants of distance, speed, and scalar acceleration
-- FIXME: Add variants of vector forms?
-- FIXME: Pull out commonalities?

xDist = uc CP.xDist (sub lR lX) metre
yDist = uc CP.yDist (sub lR lY) metre

iPos = uc CP.iPos (sup lP lI) metre
xPos = uc CP.xPos (sub lP lX) metre
yPos = uc CP.yPos (sub lP lY) metre

ixPos = uc CP.ixPos (sup (sub lP lX) lI) velU
iyPos = uc CP.iyPos (sup (sub lP lY) lI) velU

fSpeed = uc CP.fSpeed (sup lV lF) velU
iSpeed = uc CP.iSpeed (sup lV lI) velU

fVel = uc CP.fVel (sup (vec lV) lF) velU
iVel = uc CP.iVel (sup (vec lV) lI) velU
xVel = uc CP.xVel (sub      lV  lX) velU
yVel = uc CP.yVel (sub      lV  lY) velU

ixVel = uc CP.ixVel (sup (sub lV lX) lI) velU
iyVel = uc CP.iyVel (sup (sub lV lY) lI) velU

xAccel = uc CP.xAccel (sub lA lX) accelU
yAccel = uc CP.yAccel (sub lA lY) accelU

constAccelV = uc CP.constAccelV (sup (vec lA)    lC) accelU
xConstAccel = uc CP.xConstAccel (sup (sub lA lX) lC) accelU
yConstAccel = uc CP.yConstAccel (sup (sub lA lY) lC) accelU
