-- | Assigns symbols and units (quantities) to physical concepts.
module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Language.Drasil.Display
import Language.Drasil.ShortHands
import qualified Data.Drasil.Concepts.Physics as CP (acceleration, angAccel,
  angDisp, angVelo, chgInVelocity, constAccel, constAccelV, displacement,
  distance, energy, fSpeed, fVel, force, frequency, gravitationalAccel, gravitationalConst,
  gravitationalMagnitude, height, iPos, iSpeed, ixSpeed, iySpeed, iVel, impulseS, impulseV,
  ixPos, ixVel, iyPos, iyVel, kEnergy, linAccel, linDisp, linVelo, momentOfInertia, position,
  potEnergy, pressure, restitutionCoef, scalarAccel, scalarPos, speed, time, torque,
  velocity, weight, xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist,
  yPos, yVel, momentum, moment, moment2D, fOfGravity, positionVec, tension, angFreq, period,
  frequency, chgMomentum)

import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, second, hertz)
import Data.Drasil.Units.Physics (accelU, angAccelU, angVelU, gravConstU,
    impulseU, momtInertU, torqueU, velU)

restitutionCoef :: DefinedQuantityDict
restitutionCoef = dqdNoUnit CP.restitutionCoef (sub cC (label "R")) Real

-- | Collects all physical quantities defined in this file for easy use in Drasil.
physicscon :: [DefinedQuantityDict]
physicscon = [acceleration, angularAccel, angularDisplacement, angularVelocity,
  chgInVelocity, constAccel, constAccelV, displacement, distance, energy, frequency,
  fSpeed, fVel, force, gravitationalAccel, gravitationalConst, gravitationalMagnitude,
  height, iPos, iSpeed, ixSpeed, iySpeed, iVel, impulseS, impulseV, ixPos, ixVel, iyPos,
  iyVel, kEnergy, linearAccel, linearDisplacement, linearVelocity, momentOfInertia,
  position, potEnergy, pressure, scalarAccel, scalarPos, speed, time, torque, velocity,
  weight, xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist,
  yPos, yVel,momentum, moment, moment2D, fOfGravity, positionVec, tension,
  angularFrequency, period, chgMomentum]

-- * Physical Quantities (With Units)

acceleration, angularAccel, angularDisplacement, angularVelocity, chgInVelocity,
  constAccel, constAccelV, displacement, distance, energy, fSpeed, fVel, force,
  gravitationalAccel, gravitationalConst, gravitationalMagnitude, height, iPos,
  iSpeed, ixSpeed, iySpeed, iVel, impulseS, impulseV, ixPos, ixVel, iyPos, iyVel,
  kEnergy, linearAccel, linearDisplacement, linearVelocity, momentOfInertia, position,
  potEnergy, pressure, scalarAccel, scalarPos, speed, time, torque, velocity, weight,
  xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist, yPos,
  yVel, momentum, moment, moment2D, fOfGravity, positionVec, tension, angularFrequency,
  period, frequency, chgMomentum :: DefinedQuantityDict

acceleration           = dqd CP.acceleration           (Concat [vec lA, label "(", lT, label ")"]) (Vect Real) accelU
angularAccel           = dqd CP.angAccel               lAlpha                                      Real        angAccelU
angularDisplacement    = dqd CP.angDisp                lTheta                                      Real        radian
angularFrequency       = dqd CP.angFreq                cOmega                                      Real        second
angularVelocity        = dqd CP.angVelo                lOmega                                      Real        angVelU
chgInVelocity          = dqd CP.chgInVelocity          (Atop Delta $ vec lV)                       Real        velU
constAccel             = dqd CP.constAccel             (sup lA lC)                                 Real        accelU
displacement           = dqd CP.displacement           (vec lU)                                    Real        metre
distance               = dqd CP.distance               lD                                          Real        metre
energy                 = dqd CP.energy                 cE                                          Real        joule
force                  = dqd CP.force                  (vec cF)                                    (Vect Real) newton
frequency              = dqd CP.frequency              lF                                          Real        hertz
gravitationalAccel     = dqd CP.gravitationalAccel     (vec lG)                                    Real        accelU
gravitationalConst     = dqd CP.gravitationalConst     cG                                          Real        gravConstU
gravitationalMagnitude = dqd CP.gravitationalMagnitude lG                                          Real        accelU
height                 = dqd CP.height                 lH                                          Real        metre
impulseS               = dqd CP.impulseS               lJ                                          Real        impulseU
impulseV               = dqd CP.impulseV               (vec cJ)                                    Real        impulseU
kEnergy                = dqd CP.kEnergy                (Concat [cK, cE])                           Real        joule
linearAccel            = dqd CP.linAccel               (Concat [lA, label "(", lT, label ")"])     Real        accelU
linearDisplacement     = dqd CP.linDisp                (Concat [lU, label "(", lT, label ")"])     Real        metre
linearVelocity         = dqd CP.linVelo                (Concat [lV, label "(", lT, label ")"])     Real        velU
momentOfInertia        = dqd CP.momentOfInertia        (vec cI)                                    Real        momtInertU
chgMomentum            = dqd CP.chgMomentum            (Concat [cDelta,vec cP])                    Real        impulseU
momentum               = dqd CP.momentum               (vec cP)                                    Real        impulseU
moment                 = dqd CP.moment                 (vec cM)                                    Real        torqueU
moment2D               = dqd CP.moment2D               cM                                          Real        torqueU
-- FIXME: moment2D should eventually be a specialization of moment, not separately defined
period                 = dqd CP.period                 cT                                          Real        second
position               = dqd CP.position               (Concat [vec lP, label "(", lT, label ")"]) Real        metre
positionVec            = dqd CP.positionVec            (vec lR)                                    Real        metre
potEnergy              = dqd CP.potEnergy              (Concat [cP, cE])                           Real        joule
pressure               = dqd CP.pressure               lP                                          Real        pascal
speed                  = dqd CP.speed                  lV                                          Real        velU
scalarAccel            = dqd CP.scalarAccel            lA                                          Real        accelU
scalarPos              = dqd CP.scalarPos              lP                                          Real        metre
tension                = dqd CP.tension                (vec cT)                                    Real        newton
time                   = dqd CP.time                   lT                                          Real        second
torque                 = dqd CP.torque                 (vec lTau)                                  Real        torqueU
velocity               = dqd CP.velocity               (Concat [vec lV, label "(", lT, label ")"]) (Vect Real) velU
weight                 = dqd CP.weight                 cW                                          Real        newton
fOfGravity             = dqd CP.fOfGravity             (sub (vec cF) (vec lG))                     Real        newton

-- Variants of distance, speed, and scalar acceleration
-- FIXME: Add variants of vector forms?
-- FIXME: Pull out commonalities?

xDist = dqd CP.xDist (subX lR) Real metre
yDist = dqd CP.yDist (subY lR) Real metre

iPos = dqd CP.iPos (sup lP initial) Real metre
xPos = dqd CP.xPos (subX lP)        Real metre
yPos = dqd CP.yPos (subY lP)        Real metre

ixPos = dqd CP.ixPos (sup (subX lP) initial) Real metre
iyPos = dqd CP.iyPos (sup (subY lP) initial) Real metre

fSpeed = dqd CP.fSpeed (sup lV final)   Real velU
iSpeed = dqd CP.iSpeed (sup lV initial) Real velU

ixSpeed = dqd CP.ixSpeed (sub lU initial) Real velU -- temporary use for avoiding having too many superscripts
iySpeed = dqd CP.iySpeed (sub lW initial) Real velU -- temporary use for avoiding having too many superscripts

fVel = dqd CP.fVel (sup (vec lV) final)   Real velU
iVel = dqd CP.iVel (sup (vec lV) initial) Real velU
xVel = dqd CP.xVel (subX lV)              Real velU
yVel = dqd CP.yVel (subY lV)              Real velU

ixVel = dqd CP.ixVel (sup (subX lV) initial) Real velU
iyVel = dqd CP.iyVel (sup (subY lV) initial) Real velU

xAccel = dqd CP.xAccel (subX lA) Real accelU
yAccel = dqd CP.yAccel (subY lA) Real accelU

constAccelV = dqd CP.constAccelV (sup (vec  lA) constant) Real accelU
xConstAccel = dqd CP.xConstAccel (sup (subX lA) constant) Real accelU
yConstAccel = dqd CP.yConstAccel (sup (subY lA) constant) Real accelU

constant, final, initial :: Symbol
constant = label "c"
final    = label "f"
initial  = label "i"

-- * Helpers

-- Helpers for common modifiers
subMax, subMin, subX, subY, subZ, supMax, supMin :: Symbol -> Symbol
subMax s = sub s (label "max")
subMin s = sub s (label "min")
subX   s = sub s (label "x")
subY   s = sub s (label "y")
subZ   s = sub s (label "z")
supMax s = sup s (label "max")
supMin s = sup s (label "min")

-- * Constants

gravitationalConstValue :: ConstQDef
gravitationalConstValue = mkQuantDef gravitationalConst (dbl 6.6743E-11)
--(dbl 6.673 * 10E-11)
--(dbl 0.00000000006673)
gravitationalAccelConst :: ConstQDef
gravitationalAccelConst = mkQuantDef gravitationalMagnitude (dbl 9.8)
