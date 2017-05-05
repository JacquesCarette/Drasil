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
-- FIXME: parametrized hack
iVect       = ucFromVC ivec metre
  where ivec = cvR (dccWDS "unit_vect" (compoundPhrase' (cn "horizontal") (QM.unit_vect ^. term)) (phrase $ QM.unit_vect ^. term)) (QM.unit_vect ^. symbol)
-- FIXME: parametrized hack
jVect       = ucFromVC ivec metre
  where ivec = cvR (dccWDS "unit_vect" (compoundPhrase' (cn "vertical") (QM.unit_vect ^. term)) (phrase $ QM.unit_vect ^. term) ) (vec $ hat lJ)
-- FIXME: parametrized hack
normalVect  = ucFromVC normVect metre
  where normVect = cvR (dccWDS "norm_vect" (compoundPhrase' (cn "collision") (QM.norm_vect ^. term)) (phrase $ QM.norm_vect ^. term) ) (QM.norm_vect ^. symbol)
angVel      = ucFromVC QP.angularV angVelU
position    = ucFromVC QP.position metre
orientation = ucFromVC QM.orientation radians
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
-- FIXME: parametrized hack
dispUnit = ucFromVC dispVect metre
  where dispVect = cvR (dccWDS "dispUnit" (compoundPhrase' (cn "displacement")  (QM.unit_vect ^. term)) (phrase $ compoundPhrase' (cn "displacement") (QM.unit_vect ^. term))) (vec (hat lR))
--dispUnit = uc' "rhat" (nounPhraseSP "unit displacement vector")
--  "FIXME: Define this or remove the need for definitions" (vec (hat lR)) metre
-- Improvised norm symbols --
--FIXME: Properly implement Norm
--dispNorm    = uc' "||r||" (nounPhraseSP "Euclidean norm of the displacement")
--  "FIXME: Define this or remove the need for definitions" 
--  (Concat [Atomic "||", (disp ^. symbol), Atomic "||"]) metre

-- FIXME: parametrized hack
dispNorm = ucFromVC norm metre
  where norm = cvR (dccWDS "euclideanNorm" (compoundPhrase' (QM.euclid_norm ^. term) (cn "of the displacement")) (phrase $ QM.euclid_norm ^. term) ) (QM.euclid_norm ^. symbol)
-- FIXME: parametrized hack
sqrDist = ucFromVC norm m_2
  where norm = cvR (dccWDS "euclideanNorm" (cn' "squared distance") (phrase $ QM.euclid_norm ^. term) ) (sup (QM.euclid_norm ^. symbol) (Atomic "2"))

-- T4 --

vel_B, vel_O, r_OB :: UnitalChunk

-- FIXME: parametrized hack
vel_B   = ucFromVC velb velU
  where velb = cvR (dccWDS "velocity" (compoundPhrase' (QP.velocity ^. term) (cn "at point B")) (phrase $ QP.velocity ^. term)) (sub (QP.velocity ^. symbol) cB)
-- FIXME: parametrized hack
vel_O   = ucFromVC velo velU
  where velo = cvR (dccWDS "velocity" (compoundPhrase' (QP.velocity ^. term) (cn "at the origin")) (phrase $ QP.velocity ^. term)) (sub (QP.velocity ^. symbol) cO)
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

--FIXME: parametrized hack
mass_i = ucFromVC massi kilogram
  where massi = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term) (cn "of the i-th particle")) (phrase $ QPP.mass ^. term)) (sub (QPP.mass ^. symbol) lI)
--mass_i = uc' "m_i" (nounPhraseSP "mass of the i-th particle")
--  "FIXME: Define this or remove the need for definitions" 
--  (sub (mass ^. symbol) lI) kilogram
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

--FIXME: parametrized hack
mass_A = ucFromVC rigidA kilogram
  where rigidA = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term) (cn "of rigid body A")) (phrase $ QPP.mass ^. term)) (sub (QPP.mass ^. symbol) cA)
--mass_A = uc' "m_A" (nounPhraseSP "mass of rigid body A")
--  "FIXME: Define this or remove the need for definitions" 
--  (sub (mass ^. symbol) cA) kilogram

--FIXME: parametrized hack
mass_B = ucFromVC rigidB kilogram
  where rigidB = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term) (cn "of rigid body B")) (phrase $ QPP.mass ^. term)) (sub (QPP.mass ^. symbol) cB)
--mass_B = uc' "m_B" (nounPhraseSP "mass of rigid body B")
--  "FIXME: Define this or remove the need for definitions" 
--  (sub (mass ^. symbol) cB) kilogram

--FIXME: parametrized hack
normalLen = ucFromVC normLen metre
  where normLen = cvR (dccWDS "length of the normal vector" (compoundPhrase' (cn "length of the") (QM.norm_vect ^. term)) (phrase $ QM.norm_vect ^. term)) (Concat [Atomic "||",(QM.norm_vect ^. symbol), Atomic "||"]) 

--normalLen = uc' "||n||" (nounPhraseSP "length of the normal vector")
--  "FIXME: Define this or remove the need for definitions" 
--  (Concat [Atomic "||",(normalVect ^. symbol), Atomic "||"]) metre
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

-- FIXME: parametrized hack
momtInert_A = ucFromVC momtA momtInertU
  where momtA = cvR (dccWDS "momentOfInertia" (compoundPhrase' (QP.momentOfInertia ^. term) (cn "of rigid body A")) (phrase $ QP.momentOfInertia ^. term)) (sub (momtInert ^. symbol) cA)
-- FIXME: parametrized hack
momtInert_B = ucFromVC momtB momtInertU
  where momtB = cvR (dccWDS "momentOfInertia" (compoundPhrase' (QP.momentOfInertia ^. term) (cn "of rigid body B")) (phrase $ QP.momentOfInertia ^. term)) (sub (momtInert ^. symbol) cB)
