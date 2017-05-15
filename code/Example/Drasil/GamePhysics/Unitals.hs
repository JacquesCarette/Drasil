module Drasil.GamePhysics.Unitals where

import Language.Drasil
import Data.Drasil.SI_Units
import qualified Data.Drasil.Quantities.Physics as QP
import qualified Data.Drasil.Quantities.Math as QM
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP
import Data.Drasil.Units.Physics

import Control.Lens((^.))

----- Table of Symbols -----

cpSymbols :: [QSWrapper]
cpSymbols = (map qs cpUnits) ++ [qs QP.restitutionCoef] ++ 
  (map qs cpUnitless)


cpUnits :: [UnitalChunk]
cpUnits = [accel, angAccel, force, gravAccel, gravConst, momtInert, impulseVec,
    impulseScl, QPP.len, QPP.mass, iVect, jVect, normalVect, angVel, position,
    orientation, dist, disp, QP.time, torque, angDisp, vel]
    
-- Chunks with units --
accel, angAccel, force, gravAccel, gravConst, momtInert, impulseVec,
    impulseScl, iVect, jVect, normalVect, angVel, position,
    orientation, dist, disp, torque, angDisp, vel, linDisp, linVelo
    , linAccel :: UnitalChunk

-- FIXME: A number of chunks are simply renaming values from QP and QPP, may want to consider removal.
    
force       = QP.force
gravAccel   = QP.gravitationalAccel
-- What would be the best way to represent universal constants
-- like gravitational constant, and display their constant value?
gravConst   = QP.gravitationalConst
momtInert   = QP.momentOfInertia
impulseVec  = QP.impulseV
impulseScl  = QP.impulseS
-- FIXME: parametrized hack
iVect       = ucFromVC ivec metre
  where ivec = cvR (dccWDS "unitVect" (compoundPhrase' (cn "horizontal")
               (QM.unitVect ^. term)) (phrase $ QM.unitVect ^. term))
               (QM.unitVect ^. symbol)
-- FIXME: parametrized hack
jVect       = ucFromVC ivec metre
  where ivec = cvR (dccWDS "unitVect" (compoundPhrase' (cn "vertical")
               (QM.unitVect ^. term)) (phrase $ QM.unitVect ^. term) )
               (vec $ hat lJ)
-- FIXME: parametrized hack
normalVect  = ucFromVC normVect metre
  where normVect = cvR (dccWDS "normalVect" (compoundPhrase' (cn "collision")
                   (QM.normalVect ^. term)) (phrase $ QM.normalVect ^. term) )
                   (QM.normalVect ^. symbol)
position    = QP.position
orientation = QM.orientation
dist        = QP.distance
torque      = QP.torque
disp        = QP.displacement
vel         = QP.velocity
accel       = QP.acceleration

angDisp     = QP.angularDisplacement
angVel      = QP.angularVelocity
angAccel    = QP.angularAccel

--FIXME: parametrized hack
linDisp     = ucFromVC ldisp velU
  where ldisp = cvR (dccWDS "linearDisp" (compoundPhrase'
                (QP.linearDisplacement ^. term) (cn ("FIXME: add definition")))
                (phrase $ QP.linearDisplacement ^. term))
                (QP.linearDisplacement ^. symbol)
--FIXME: parametrized hack
linVelo     = ucFromVC lVelo velU
  where lVelo = cvR (dccWDS "linearVelo" (compoundPhrase'
                (QP.linearVelocity ^. term) (cn ("FIXME: add definition")))
                (phrase $ QP.linearVelocity ^. term)) 
                (QP.linearVelocity ^. symbol)
--FIXME: parametrized hack
linAccel     = ucFromVC lAccl accelU
  where lAccl = cvR (dccWDS "linearDisp" (compoundPhrase'
                (QP.linearAccel ^. term) (cn ("FIXME: add definition")))
                (phrase $ QP.linearAccel ^. term))
                (QP.linearAccel ^. symbol)
-- Chunks without units --
cpUnitless :: [VarChunk]
cpUnitless = [numParticles]

numParticles :: VarChunk
numParticles = makeVC "n" (nounPhraseSP "number of particles in a rigid body") lN

----- Specific unitals needed for certain models and definitions -----

-- TODO: The following should all end up parameterized

-- T2 --

force_1, force_2 :: UnitalChunk
--FIXME: parametrized hack
force_1 = ucFromVC force1 newton
  where force1 = cvR (dccWDS "force" (QP.force ^. term)
                 (phrase $ compoundPhrase' (QP.force ^. term)
                 (cn "exerted by the first body (on another body)")))
                 (sub (QP.force ^. symbol) (Atomic "1"))
--FIXME: parametrized hack
force_2 = ucFromVC force1 newton
  where force1 = cvR (dccWDS "force" (QP.force ^. term)
                 (phrase $ compoundPhrase' (QP.force ^. term)
                 (cn "exerted by the second body (on another body)")))
                 (sub (QP.force ^. symbol) (Atomic "2"))
-- T3 --
mass_1, mass_2, dispUnit, dispNorm, sqrDist :: UnitalChunk

-- FIXME: parametrized hack
mass_1 = ucFromVC mass1 kilogram
  where mass1 = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the first body")) (phrase $ QPP.mass ^. term))
                (sub (QPP.mass ^. symbol) (Atomic "1"))
-- FIXME: parametrized hack
mass_2 = ucFromVC mass2 kilogram
  where mass2 = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the second body")) (phrase $ QPP.mass ^. term))
                (sub (QPP.mass ^. symbol) (Atomic "2"))
-- FIXME: parametrized hack
dispUnit = ucFromVC dispVect metre
  where dispVect = cvR (dccWDS "dispUnit" (compoundPhrase' (cn "displacement")
                   (QM.unitVect ^. term)) (phrase $ compoundPhrase'
                   (cn "displacement") (QM.unitVect ^. term))) (vec (hat lR))
-- FIXME: parametrized hack
dispNorm = ucFromVC norm metre
  where norm = cvR (dccWDS "euclideanNorm" (compoundPhrase'
               (QM.euclidNorm ^. term) (cn "of the displacement"))
               (phrase $ QM.euclidNorm ^. term) ) (QM.euclidNorm ^. symbol)
-- FIXME: parametrized hack
sqrDist = ucFromVC norm m_2
  where norm = cvR (dccWDS "euclideanNorm" (cn' "squared distance")
               (phrase $ QM.euclidNorm ^. term) ) (sup (QM.euclidNorm ^. symbol) 
               (Atomic "2"))
-- T4 --
vel_B, vel_O, r_OB :: UnitalChunk

-- FIXME: parametrized hack
vel_B   = ucFromVC velb velU
  where velb = cvR (dccWDS "velocity" (compoundPhrase' (QP.velocity ^. term)
               (cn "at point B")) (phrase $ QP.velocity ^. term))
               (sub (QP.velocity ^. symbol) cB)
-- FIXME: parametrized hack
vel_O   = ucFromVC velo velU
  where velo = cvR (dccWDS "velocity" (compoundPhrase' (QP.velocity ^. term)
               (cn "at the origin")) (phrase $ QP.velocity ^. term))
               (sub (QP.velocity ^. symbol) cO)
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
  where massi = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the i-th particle")) (phrase $ QPP.mass ^. term))
                (sub (QPP.mass ^. symbol) lI)
pos_i = uc' "p_i" (nounPhraseSP "position vector of the i-th particle")
  "FIXME: Define this or remove the need for definitions" 
  (sub (position ^. symbol) lI) metre
mTot = uc' "M" (nounPhraseSP "total mass of the rigid body")
  "FIXME: Define this or remove the need for definitions" cM kilogram

-- DD8 --

initRelVel, mass_A, mass_B, normalLen, contDisp_A, contDisp_B, perpLen_A,
  momtInert_A, perpLen_B, momtInert_B :: UnitalChunk

--FIXME: parametrized hack
initRelVel = ucFromVC relVel velU
  where relVel = cvR (dccWDS "v_i^AB" (compoundPhrase'
                 (compoundPhrase' (cn "relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B"))
                 (phrase $ QP.velocity ^. term))
                 (sup (sub (QP.velocity ^. symbol) lI) (Concat [cA, cB]))
--FIXME: parametrized hack
mass_A = ucFromVC rigidA kilogram
  where rigidA = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term)
                 (cn "of rigid body A")) (phrase $ QPP.mass ^. term))
                 (sub (QPP.mass ^. symbol) cA)
--FIXME: parametrized hack
mass_B = ucFromVC rigidB kilogram
  where rigidB = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term)
                 (cn "of rigid body B")) (phrase $ QPP.mass ^. term))
                 (sub (QPP.mass ^. symbol) cB)
--FIXME: parametrized hack
normalLen = ucFromVC normLen metre
  where normLen = cvR (dccWDS "length of the normal vector" (compoundPhrase'
                  (cn "length of the") (QM.normalVect ^. term))
                  (phrase $ QM.normalVect ^. term))
                  (Concat [Atomic "||",(QM.normalVect ^. symbol), Atomic "||"])
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

--FIXME: parametrized hack -> needs synonym for normal with perpendicular
perpLen_A = ucFromVC perpA metre
  where perpA = cvR (dccWDS "|| r_AP x n ||" (compoundPhrase' (compoundPhrase
                (cn' "length of the") (QM.perpVect ^. term))
                (cn "to the contact displacement vector of rigid body A"))
                (phrase $ QM.perpVect ^. term))  (Concat [Atomic "||", 
                (contDisp_A ^. symbol), Atomic "*", 
                (QM.perpVect ^. symbol), Atomic "||"])
--FIXME: parametrized hack -> needs synonym for normal with perpendicular
perpLen_B = ucFromVC perpB metre
  where perpB = cvR (dccWDS "|| r_AB x n ||" (compoundPhrase' (compoundPhrase 
                (cn' "length of the") (QM.perpVect ^. term))
                (cn "to the contact displacement vector of rigid body B"))
                (phrase $ QM.perpVect ^. term)) 
                (Concat [Atomic "||", (contDisp_B ^. symbol), Atomic "*", 
                (QM.perpVect ^. symbol), Atomic "||"])

-- FIXME: parametrized hack
momtInert_A = ucFromVC momtA momtInertU
  where momtA = cvR (dccWDS "momentOfInertia" (compoundPhrase'
                (QP.momentOfInertia ^. term) (cn "of rigid body A"))
                (phrase $ QP.momentOfInertia ^. term))
                (sub (momtInert ^. symbol) cA)
-- FIXME: parametrized hack
momtInert_B = ucFromVC momtB momtInertU
  where momtB = cvR (dccWDS "momentOfInertia" (compoundPhrase'
                (QP.momentOfInertia ^. term) (cn "of rigid body B"))
                (phrase $ QP.momentOfInertia ^. term))
                (sub (momtInert ^. symbol) cB)
