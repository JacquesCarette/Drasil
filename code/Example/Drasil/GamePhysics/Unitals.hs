module Drasil.GamePhysics.Unitals where

import Language.Drasil
import Data.Drasil.SI_Units
import qualified Data.Drasil.Quantities.Physics as QP
import qualified Data.Drasil.Quantities.Math as QM
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP

import Control.Lens((^.))

----- Table of Symbols -----

cpSymbols :: [QSWrapper]
cpSymbols = (map qs cpUnits) ++ [qs QP.restitutionCoef] ++ 
  (map qs cpUnitless)

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
--jVect       = unFromVC jvec metre
--            where jvec = cvR 
iVect       = ucFromVC ivec metre
            where ivec = cvR (dcc "unit_vect" (compoundPhrase' (cn "horizontal") (QM.unit_vect ^. term)) "unit vector" ) (QM.unit_vect ^. symbol)
            --FIXME: Better way to parametrize a ConVar?
--iVect       = uc' "i" (nounPhraseSP "horizontal unit vector")
--  "FIXME: Define this or remove the need for definitions" (vec (hat lI)) metre

jVect       = ucFromVC ivec metre
            where ivec = cvR (dcc "unit_vect" (compoundPhrase' (cn "vertical") (QM.unit_vect ^. term)) "unit vector" ) (vec $ hat lJ)
            --FIXME: Better way to parametrize a ConVar?
--jVect       = uc' "j" (nounPhraseSP "vertical unit vector")
--  "FIXME: Define this or remove the need for definitions" (vec (hat lJ)) metre

normalVect  = uc' "n" (nounPhraseSP "collision normal vector")
  "FIXME: Define this or remove the need for definitions"(vec lN) metre
angVel      = ucFromVC QP.angularV angVelU
position    = ucFromVC QP.position metre
orientation = ucFromVC QM.orientation radians
  --{uc' "phi" (cn' "orientation")
  --"FIXME: Define this or remove the need for definitions" (Greek Phi_L) radians
dist        = ucFromVC QP.distance metre
disp        = ucFromVC QP.displacement metre
time        = ucFromVC QP.time second
torque      = ucFromVC QP.torque torqueU
angDisp     = ucFromVC QP.angularDisplacement radians
vel         = ucFromVC QP.velocity velU

-- Chunks without units --
cpUnitless :: [VarChunk]
cpUnitless = [numParticles]

numParticles :: VarChunk
numParticles = makeVC "n" (nounPhraseSP "number of particles in a rigid body") lN

----- Specific unitals needed for certain models and definitions -----

-- TODO: The following should all end up parameterized

-- T2 --

force_1, force_2 :: UnitalChunk

force_1 = uc' "F_1" 
  (nounPhraseSP "force exerted by the first body (on another body)")
  "FIXME: Define this or remove the need for definitions" 
  (sub (force ^. symbol) (Atomic "1")) newton
force_2 = uc' "F_2" 
  (nounPhraseSP "force exerted by the second body (on another body)")
  "FIXME: Define this or remove the need for definitions" 
  (sub (force ^. symbol) (Atomic "2")) newton

-- T3 --

mass_1, mass_2, dispUnit, dispNorm, sqrDist :: UnitalChunk

mass_1  = uc' "m_1" (nounPhraseSP "mass of the first body")
  "FIXME: Define this or remove the need for definitions"
  (sub (mass ^. symbol) (Atomic "1")) kilogram
mass_2  = uc' "m_2" (nounPhraseSP "mass of the second body")
  "FIXME: Define this or remove the need for definitions" 
  (sub (mass ^. symbol) (Atomic "2")) kilogram
dispUnit = uc' "rhat" (nounPhraseSP "unit displacement vector")
  "FIXME: Define this or remove the need for definitions" (vec (hat lR)) metre
-- Improvised norm symbols --
--FIXME: Properly implement Norm
dispNorm    = uc' "||r||" (nounPhraseSP "Euclidean norm of the displacement")
  "FIXME: Define this or remove the need for definitions" 
  (Concat [Atomic "||", (disp ^. symbol), Atomic "||"]) metre

--sqrDist = ucFromVC MP.EuclideanNorm m_2
sqrDist = uc' "||r||^2" (nounPhraseSP "squared distance")
  "FIXME: Define this or remove the need for definitions" 
  (sup (dispNorm ^. symbol) (Atomic "2")) m_2

-- T4 --

vel_B, vel_O, r_OB :: UnitalChunk

vel_B   = uc' "v_B" (nounPhraseSP "velocity at point B")
  "FIXME: Define this or remove the need for definitions" (sub (vel ^. symbol) cB) velU
vel_O   = uc' "v_O" (nounPhraseSP "velocity at the origin")
  "FIXME: Define this or remove the need for definitions" (sub (vel ^. symbol) cO) velU
r_OB    = uc' "r_OB" 
  (nounPhraseSP "displacement vector between the origin and point B")
  "FIXME: Define this or remove the need for definitions" 
  (sub (disp ^. symbol) (Concat [cO, cB])) metre

-- DD1 --

pos_CM, mass_i, pos_i, mTot :: UnitalChunk

pos_CM = uc' "p_CM" (nounPhraseSP $ 
  "the mass-weighted average position of a rigid " ++
  "body's particles") 
  "FIXME: Define this or remove the need for definitions" 
  (sub (position ^. symbol) (Atomic "CM")) metre
mass_i = uc' "m_i" (nounPhraseSP "mass of the i-th particle")
  "FIXME: Define this or remove the need for definitions" 
  (sub (mass ^. symbol) lI) kilogram
pos_i = uc' "p_i" (nounPhraseSP "position vector of the i-th particle")
  "FIXME: Define this or remove the need for definitions" 
  (sub (position ^. symbol) lI) metre
mTot = uc' "M" (nounPhraseSP "total mass of the rigid body")
  "FIXME: Define this or remove the need for definitions" cM kilogram

-- DD8 --

initRelVel, mass_A, mass_B, normalLen, contDisp_A, contDisp_B, perpLen_A,
  momtInert_A, perpLen_B, momtInert_B :: UnitalChunk

initRelVel = uc' "v_i^AB" 
  (nounPhraseSP "relative velocity between rigid bodies A and B")
  "FIXME: Define this or remove the need for definitions"
  (sup (sub (vel ^. symbol) lI) (Concat [cA, cB])) velU
mass_A = uc' "m_A" (nounPhraseSP "mass of rigid body A")
  "FIXME: Define this or remove the need for definitions" 
  (sub (mass ^. symbol) cA) kilogram
mass_B = uc' "m_B" (nounPhraseSP "mass of rigid body B")
  "FIXME: Define this or remove the need for definitions" 
  (sub (mass ^. symbol) cB) kilogram
normalLen = uc' "||n||" (nounPhraseSP "length of the normal vector")
  "FIXME: Define this or remove the need for definitions" 
  (Concat [Atomic "||",(normalVect ^. symbol), Atomic "||"]) metre
contDisp_A = uc' "r_AP" (nounPhraseSP $ 
  "displacement vector between the centre of " ++
  "mass of rigid body A and contact point P")
  "FIXME: Define this or remove the need for definitions" 
  (sub (disp ^. symbol) (Concat [cA, cP])) metre
contDisp_B = uc' "r_BP" (nounPhraseSP $ 
  "displacement vector between the centre of " ++
  "mass of rigid body B and contact point P")
  "FIXME: Define this or remove the need for definitions" 
  (sub (disp ^. symbol) (Concat [cB, cP])) metre
perpLen_A = uc' "||r_AP x n||" (nounPhraseSP $ 
  "length of the vector perpendicular " ++
  "to the contact displacement vector of rigid body A")
  "FIXME: Define this or remove the need for definitions" 
  (Concat [Atomic "||", (contDisp_A ^. symbol), Atomic "*",
  (normalVect ^. symbol), Atomic "||"]) metre
perpLen_B = uc' "||r_BP x n||" (nounPhraseSP $ 
  "length of the vector perpendicular " ++
  "to the contact displacement vector of rigid body B")
  "FIXME: Define this or remove the need for definitions" 
  (Concat [Atomic "||", (contDisp_B ^. symbol), Atomic "*",
  (normalVect ^. symbol), Atomic "||"]) metre
momtInert_A = uc' "I_A" (nounPhraseSP "moment of inertia of rigid body A")
  "FIXME: Define this or remove the need for definitions" 
  (sub (momtInert ^. symbol) cA) momtInertU
momtInert_B = uc' "I_B" (nounPhraseSP "moment of inertia of rigid body B")
  "FIXME: Define this or remove the need for definitions" 
  (sub (momtInert ^. symbol) cB) momtInertU
