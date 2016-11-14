module Drasil.GamePhysics.Unitals where

import Language.Drasil
import Data.Drasil.SI_Units
import qualified Data.Drasil.Quantities.Physics as QP

import qualified Data.Drasil.Quantities.PhysicalProperties as QPP

import Control.Lens((^.))

----- Table of Symbols -----

cpSymbols :: [MUChunk]
cpSymbols = (map Has cpUnits) ++ (map HasNot cpUnitless)

-- Chunks with units --
accel, angAccel, force, gravAccel, gravConst, momtInert, impulseVec,
    impulseScl, len, mass, iVect, jVect, normalVect, angVel, position,
    orientation, dist, disp, time, torque, angDisp, vel :: UnitalChunk

cpUnits :: [UnitalChunk]
cpUnits = [accel, angAccel, force, gravAccel, gravConst, momtInert, impulseVec,
    impulseScl, len, mass, iVect, jVect, normalVect, angVel, position,
    orientation, dist, disp, time, torque, angDisp, vel]

accel       = ucFromVC QP.acceleration accelU
angAccel    = ucFromVC QP.angularAccel angAccelU
force       = ucFromVC QP.force newton
gravAccel   = ucFromVC QP.gravitationalAccel accelU
-- What would be the best way to represent universal constants
-- like gravitational constant, and display their constant value?
gravConst   = ucFromVC QP.gravitationalConst gravConstU
momtInert   = ucFromVC QP.momentOfInertia momtInertU
impulseVec  = ucFromVC QP.impulseV impulseU
impulseScl  = ucFromVC QP.impulseS impulseU
len         = ucFromVC QPP.length metre
mass        = ucFromVC QPP.mass kilogram
iVect       = makeUC "i" "horizontal unit vector" (vec (hat lI)) metre
jVect       = makeUC "j" "vertical unit vector" (vec (hat lJ)) metre
normalVect  = makeUC "n" "collision normal vector" (vec lN) metre
angVel      = ucFromVC QP.angularV angVelU
position    = makeUC "p" "position" (vec lP) metre
orientation = makeUC "phi" "orientation" (Greek Phi_L) radians
dist        = makeUC "r" "distance" lR metre
disp        = makeUC "r" "displacement" (vec lR) metre
time        = makeUC "t" "time" lT second
torque      = makeUC "tau" "torque" (Greek Tau_L) torqueU
angDisp     = makeUC "theta" "angular displacement" (Greek Theta_L) radians
vel         = makeUC "v" "velocity" (vec lV) velU

-- Chunks without units --
cpUnitless :: [VarChunk]
cpUnitless = [QP.restitutionCoef, numParticles]

numParticles :: VarChunk
numParticles = makeVC "n" "number of particles in a rigid body" lN

----- Specific unitals needed for certain models and definitions -----

-- T2 --

force_1, force_2 :: UnitalChunk

force_1 = makeUC "F_1" "force exerted by the first body (on another body)"
    (sub (force ^. symbol) (Atomic "1")) newton
force_2 = makeUC "F_2" "force exerted by the second body (on another body)"
    (sub (force ^. symbol) (Atomic "2")) newton

-- T3 --

mass_1, mass_2, dispUnit, dispNorm, sqrDist :: UnitalChunk

mass_1  = makeUC "m_1" "mass of the first body" (sub (mass ^. symbol)
    (Atomic "1")) kilogram
mass_2  = makeUC "m_2" "mass of the second body" (sub (mass ^. symbol)
    (Atomic "2")) kilogram
dispUnit = makeUC "rhat" "unit displacement vector" (vec (hat lR)) metre
-- Improvised norm symbols --
dispNorm    = makeUC "||r||" "Euclidean norm of the displacement"
    (Concat [Atomic "||", (disp ^. symbol), Atomic "||"]) metre
sqrDist = makeUC "||r||^2" "squared distance" (sup (dispNorm ^. symbol)
    (Atomic "2")) m_2

-- T4 --

vel_B, vel_O, r_OB :: UnitalChunk

vel_B   = makeUC "v_B" "velocity at point B" (sub (vel ^. symbol) cB) velU
vel_O   = makeUC "v_O" "velocity at the origin" (sub (vel ^. symbol) cO) velU
r_OB    = makeUC "r_OB" "displacement vector between the origin and point B"
    (sub (disp ^. symbol) (Concat [cO, cB])) metre

-- DD1 --

pos_CM, mass_i, pos_i, mTot :: UnitalChunk

pos_CM = makeUC "p_CM" ("the mass-weighted average position of a rigid " ++
    "body's particles") (sub (position ^. symbol) (Atomic "CM")) metre
mass_i = makeUC "m_i" "mass of the i-th particle" (sub (mass ^. symbol) lI)
    kilogram
pos_i = makeUC "p_i" "position vector of the i-th particle"
    (sub (position ^. symbol) lI) metre
mTot = makeUC "M" "total mass of the rigid body" cM kilogram

-- DD8 --

initRelVel, mass_A, mass_B, normalLen, contDisp_A, contDisp_B, perpLen_A,
    momtInert_A, perpLen_B, momtInert_B :: UnitalChunk

initRelVel = makeUC "v_i^AB" "relative velocity between rigid bodies A and B"
    (sup (sub (vel ^. symbol) lI) (Concat [cA, cB])) velU
mass_A = makeUC "m_A" "mass of rigid body A" (sub (mass ^. symbol) cA) kilogram
mass_B = makeUC "m_B" "mass of rigid body B" (sub (mass ^. symbol) cB) kilogram
normalLen = makeUC "||n||" "length of the normal vector" (Concat [Atomic "||",
    (normalVect ^. symbol), Atomic "||"]) metre
contDisp_A = makeUC "r_AP" ("displacement vector between the centre of " ++
    "mass of rigid body A and contact point P") (sub (disp ^. symbol)
    (Concat [cA, cP])) metre
contDisp_B = makeUC "r_BP" ("displacement vector between the centre of " ++
    "mass of rigid body B and contact point P") (sub (disp ^. symbol)
    (Concat [cB, cP])) metre
perpLen_A = makeUC "||r_AP x n||" ("length of the vector perpendicular " ++
    "to the contact displacement vector of rigid body A")
    (Concat [Atomic "||", (contDisp_A ^. symbol), Atomic "*",
    (normalVect ^. symbol), Atomic "||"]) metre
perpLen_B = makeUC "||r_BP x n||" ("length of the vector perpendicular " ++
    "to the contact displacement vector of rigid body B")
    (Concat [Atomic "||", (contDisp_B ^. symbol), Atomic "*",
    (normalVect ^. symbol), Atomic "||"]) metre
momtInert_A = makeUC "I_A" "moment of inertia of rigid body A"
    (sub (momtInert ^. symbol) cA) momtInertU
momtInert_B = makeUC "I_B" "moment of inertia of rigid body B"
    (sub (momtInert ^. symbol) cB) momtInertU
