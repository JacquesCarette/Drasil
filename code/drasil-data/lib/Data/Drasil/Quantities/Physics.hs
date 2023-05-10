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
  yPos, yVel, momentum, moment, fOfGravity, positionVec, tension, angFreq, period, frequency, chgMomentum)

import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, second, hertz)
import Data.Drasil.Units.Physics (accelU, angAccelU, angVelU, gravConstU, 
    impulseU, momtInertU, torqueU, velU)

restitutionCoef :: DefinedQuantityDict
restitutionCoef = dqdNoUnit CP.restitutionCoef (sub cC (label "R")) Real

-- | Collects all physical quantities defined in this file for easy use in Drasil.
physicscon :: [UnitalChunk]
physicscon = [acceleration, angularAccel, angularDisplacement, angularVelocity,
  chgInVelocity, constAccel, constAccelV, displacement, distance, energy, frequency,
  fSpeed, fVel, force, gravitationalAccel, gravitationalConst, gravitationalMagnitude,
  height, iPos, iSpeed, ixSpeed, iySpeed, iVel, impulseS, impulseV, ixPos, ixVel, iyPos, 
  iyVel, kEnergy, linearAccel, linearDisplacement, linearVelocity, momentOfInertia, 
  position, potEnergy, pressure, scalarAccel, scalarPos, speed, time, torque, velocity,
  weight, xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist,
  yPos, yVel,momentum, moment, moment2D, fOfGravity, positionVec, tension,
  angularFrequency, period, frequency, chgMomentum]

-- * Physical Quantities (With Units)

acceleration, angularAccel, angularDisplacement, angularVelocity, chgInVelocity,
  constAccel, constAccelV, displacement, distance, energy, fSpeed, fVel, force,
  gravitationalAccel, gravitationalConst, gravitationalMagnitude, height, iPos, 
  iSpeed, ixSpeed, iySpeed, iVel, impulseS, impulseV, ixPos, ixVel, iyPos, iyVel, 
  kEnergy, linearAccel, linearDisplacement, linearVelocity, momentOfInertia, position, 
  potEnergy, pressure, scalarAccel, scalarPos, speed, time, torque, velocity, weight, 
  xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist, yPos, 
  yVel, momentum, moment, moment2D, fOfGravity, positionVec, tension, angularFrequency, 
  period, frequency, chgMomentum :: UnitalChunk

acceleration           = uc CP.acceleration           (vec lA)                                    (Vect Real) accelU
angularAccel           = uc CP.angAccel               lAlpha                                      Real        angAccelU
angularDisplacement    = uc CP.angDisp                lTheta                                      Real        radian
angularFrequency       = uc CP.angFreq                cOmega                                      Real        second
angularVelocity        = uc CP.angVelo                lOmega                                      Real        angVelU
chgInVelocity          = uc CP.chgInVelocity          (Atop Delta $ vec lV)                       Real        velU
constAccel             = uc CP.constAccel             (sup lA lC)                                 Real        accelU
displacement           = uc CP.displacement           (vec lU)                                    Real        metre
distance               = uc CP.distance               lD                                          Real        metre
energy                 = uc CP.energy                 cE                                          Real        joule
force                  = uc CP.force                  (vec cF)                                    Real        newton
frequency              = uc CP.frequency              lF                                          Real        hertz
gravitationalAccel     = uc CP.gravitationalAccel     (vec lG)                                    Real        accelU
gravitationalConst     = uc CP.gravitationalConst     cG                                          Real        gravConstU
gravitationalMagnitude = uc CP.gravitationalMagnitude lG                                          Real        accelU
height                 = uc CP.height                 lH                                          Real        metre
impulseS               = uc CP.impulseS               lJ                                          Real        impulseU
impulseV               = uc CP.impulseV               (vec cJ)                                    Real        impulseU
kEnergy                = uc CP.kEnergy                (Concat [cK, cE])                           Real        joule
linearAccel            = uc CP.linAccel               (Concat [vec lA, label "(", lT, label ")"]) Real        accelU
linearDisplacement     = uc CP.linDisp                (Concat [vec lU, label "(", lT, label ")"]) Real        metre
linearVelocity         = uc CP.linVelo                (Concat [vec lV, label "(", lT, label ")"]) Real        velU
momentOfInertia        = uc CP.momentOfInertia        (vec cI)                                    Real        momtInertU
chgMomentum            = uc CP.chgMomentum            (Concat [cDelta,vec cP])                    Real        impulseU
momentum               = uc CP.momentum               (vec cP)                                    Real        impulseU
moment                 = uc CP.moment                 (vec cM)                                    Real        torqueU
moment2D               = uc CP.moment                 cM                                          Real        torqueU
-- FIXME: moment2D should eventually be a specialization of moment, not separately defined
period                 = uc CP.period             cT                                              Real        second
position               = uc CP.position           (vec lP)                                        Real        metre
positionVec            = uc CP.positionVec        (vec lR)                                        Real        metre
potEnergy              = uc CP.potEnergy          (Concat [cP, cE])                               Real        joule
pressure               = uc CP.pressure           lP                                              Real        pascal
speed                  = uc CP.speed              lV                                              Real        velU
scalarAccel            = uc CP.scalarAccel        lA                                              Real        accelU
scalarPos              = uc CP.scalarPos          lP                                              Real        metre
tension                = uc CP.tension            (vec cT)                                        Real        newton
time                   = uc CP.time               lT                                              Real        second
torque                 = uc CP.torque             (vec lTau)                                      Real        torqueU
velocity               = uc CP.velocity           (vec lV)                                        (Vect Real) velU
weight                 = uc CP.weight             cW                                              Real        newton
fOfGravity             = uc CP.fOfGravity         (sub (vec cF) (vec lG))                         Real        newton

-- Variants of distance, speed, and scalar acceleration
-- FIXME: Add variants of vector forms?
-- FIXME: Pull out commonalities?

xDist = uc CP.xDist (subX lR) Real metre
yDist = uc CP.yDist (subY lR) Real metre

iPos = uc CP.iPos (sup lP initial) Real metre
xPos = uc CP.xPos (subX lP)        Real metre
yPos = uc CP.yPos (subY lP)        Real metre

ixPos = uc CP.ixPos (sup (subX lP) initial) Real metre
iyPos = uc CP.iyPos (sup (subY lP) initial) Real metre

fSpeed = uc CP.fSpeed (sup lV final)   Real velU
iSpeed = uc CP.iSpeed (sup lV initial) Real velU

ixSpeed = uc CP.ixSpeed (sub lU initial) Real velU -- temporary use for avoiding having too many superscripts
iySpeed = uc CP.iySpeed (sub lW initial) Real velU -- temporary use for avoiding having too many superscripts

fVel = uc CP.fVel (sup (vec lV) final)   Real velU
iVel = uc CP.iVel (sup (vec lV) initial) Real velU
xVel = uc CP.xVel (subX lV)              Real velU
yVel = uc CP.yVel (subY lV)              Real velU

ixVel = uc CP.ixVel (sup (subX lV) initial) Real velU
iyVel = uc CP.iyVel (sup (subY lV) initial) Real velU

xAccel = uc CP.xAccel (subX lA) Real accelU
yAccel = uc CP.yAccel (subY lA) Real accelU

constAccelV = uc CP.constAccelV (sup (vec  lA) constant) Real accelU
xConstAccel = uc CP.xConstAccel (sup (subX lA) constant) Real accelU
yConstAccel = uc CP.yConstAccel (sup (subY lA) constant) Real accelU

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
