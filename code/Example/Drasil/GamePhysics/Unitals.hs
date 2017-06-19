module Drasil.GamePhysics.Unitals where

import Language.Drasil
import Data.Drasil.SI_Units
import qualified Data.Drasil.Concepts.Physics as CP
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
cpUnits = [QP.acceleration, QP.angularAccel, QP.force, QP.gravitationalAccel, 
  QP.gravitationalConst, QP.momentOfInertia, QP.impulseV, QP.impulseS, QPP.len, 
  QPP.mass, iVect, jVect, normalVect, QP.angularVelocity, QP.position, 
  QM.orientation, QP.distance, QP.displacement, QP.time, QP.torque,
  QP.angularDisplacement, QP.velocity, pos_CM, pos_i, mass_i, mTot, acc_i, vel_i,
  QP.linearDisplacement, QP.linearVelocity, QP.linearAccel, initRelVel, normalLen,
  perpLen_A, perpLen_B, force_i, torque_i, time_c, vel_A, vel_B, mass_A, mass_B,
  angVel_A, angVel_B]
    
-- Chunks with units --
iVect, jVect, normalVect :: UnitalChunk


-- FIXME: parametrized hack
iVect       = ucFromCV ivec metre
  where ivec = cvR (dccWDS "unitVect" (compoundPhrase' (cn "horizontal")
               (QM.unitVect ^. term)) (phrase $ QM.unitVect))
               (QM.unitVect ^. symbol)
-- FIXME: parametrized hack
jVect       = ucFromCV ivec metre
  where ivec = cvR (dccWDS "unitVect" (compoundPhrase' (cn "vertical")
               (QM.unitVect ^. term)) (phrase $ QM.unitVect) )
               (vec $ hat lJ)
-- FIXME: parametrized hack
normalVect  = ucFromCV normVect metre
  where normVect = cvR (dccWDS "normalVect" (compoundPhrase' (cn "collision")
                   (QM.normalVect ^. term)) (phrase $ QM.normalVect) )
                   (QM.normalVect ^. symbol)

-- Chunks without units --
cpUnitless :: [VarChunk]
cpUnitless = [numParticles]

numParticles :: VarChunk
numParticles = vc "n" (nounPhraseSP "number of particles in a rigid body") lN Integer

----- Specific unitals needed for certain models and definitions -----

-- TODO: The following should all end up parameterized

-- T2 --

force_1, force_2, force_i :: UnitalChunk
force_1 = ucFromCV (forceParam "1" "first") newton
force_2 = ucFromCV (forceParam "2" "second") newton
force_i = ucFromCV (forceParam "i" "i-th") newton

forceParam, massParam :: String -> String -> ConVar
forceParam n w = cvR (dccWDS "force" (cn $ "force exerted by the" ++ w ++ "body (on another body)") (phrase QP.force)) (sub (QP.force ^. symbol) (Atomic n))

massParam n w = cvR (dccWDS "mass" (cn $ "mass of the" ++ w ++ "body") (phrase QPP.mass))
                (sub (QPP.mass ^. symbol) (Atomic n))
-- T3 --
mass_1, mass_2, dispUnit, dispNorm, sqrDist :: UnitalChunk

mass_1 = ucFromCV (massParam "1" "first") kilogram
mass_2 = ucFromCV (massParam "2" "second") kilogram

-- FIXME: parametrized hack
dispUnit = ucFromCV dispVect metre
  where dispVect = cvR (dccWDS "dispUnit" (cn "displacement unit vector") (S "displacement" +:+ (phrase QM.unitVect))) (vec (hat lR))

-- FIXME: parametrized hack
dispNorm = ucFromCV norm metre
  where norm = cvR (dccWDS "euclideanNorm" (cn "euclidean normal of displacement")
               (phrase $ QM.euclidNorm) ) (QM.euclidNorm ^. symbol)
-- FIXME: parametrized hack
sqrDist = ucFromCV norm m_2
  where norm = cvR (dccWDS "euclideanNorm" (cn' "squared distance")
               (phrase $ QM.euclidNorm) ) (sup (QM.euclidNorm ^. symbol) 
               (Atomic "2"))
-- T4 --
vel_A, vel_B, vel_O, r_OB, angVel_A, angVel_B :: UnitalChunk

velParam :: String -> Symbol -> ConVar
velParam n w = cvR (dccWDS "velocity" (compoundPhrase' (QP.velocity ^. term)
               (cn $ "at point" ++ n)) (phrase $ QP.velocity))
               (sub (QP.velocity ^. symbol) w)

-- FIXME: parametrized hack
vel_A   = ucFromCV (velParam "A" cA) velU
vel_B   = ucFromCV (velParam "B" cB) velU
vel_O   = ucFromCV (velParam "origin" cO) velU
angVel_A = ucFromCV (angParam "A" cA) angVelU
angVel_B = ucFromCV (angParam "B" cB) angVelU

angParam :: String -> Symbol -> ConVar
angParam n w = cvR (dccWDS "angular velocity" (compoundPhrase'
              (cn $ "is the" ++ n ++ "body's") (QP.angularVelocity ^. term))
              (phrase $ QP.angularVelocity))
              (sub (QP.angularVelocity ^. symbol) w)

--s the k-th body’s angular velocity at time 


r_OB    = uc' "r_OB" 
  (nounPhraseSP "displacement vector between the origin and point B")
  "FIXME: Define this or remove the need for definitions" 
  (sub (QP.displacement ^. symbol) (Concat [cO, cB])) metre

-- DD1 --

pos_CM, mass_i, pos_i, acc_i, mTot, vel_i, torque_i, time_c :: UnitalChunk

pos_CM = ucs "p_CM" (nounPhraseSP $ 
  "the mass-weighted average position of a rigid " ++
  "body's particles") 
  "FIXME: Define this or remove the need for definitions" 
  (sub (QP.position ^. symbol) (Atomic "CM")) metre Real

--FIXME: parametrized hack
mass_i = ucFromCV massi kilogram
  where massi = cvR (dccWDS "m_i" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the i-th particle")) (phrase $ QPP.mass))
                (sub (QPP.mass ^. symbol) lI)
pos_i = ucFromCV posi metre
  where posi = cvR (dccWDS "p_i" (compoundPhrase' (QP.position ^. term) 
                (cn "vector of the i-th particle")) (phrase $ QP.position))
                (sub (QP.position ^. symbol) lI)

acc_i = ucFromCV accI accelU
  where accI = cvR (dccWDS "acc_i" (compoundPhrase' (QP.acceleration ^. term) 
                (cn "of the i-th body's acceleration")) (phrase $ QP.acceleration))
                (sub (QP.acceleration ^. symbol) lI)

vel_i = ucFromCV accI velU
  where accI = cvR (dccWDS "vel_i" (compoundPhrase' (QP.velocity ^. term) 
                (cn "of the i-th body's velocity")) (phrase $ QP.velocity))
                (sub (QP.velocity ^. symbol) lI)

torque_i = ucFromCV torI torqueU
  where torI = cvR (dccWDS "torque_i"
                (cn "is the torque applied to the i-th body")
                (phrase $ QP.torque))
                (sub (QP.torque ^. symbol) lI)

mTot = ucFromCV mtotal kilogram
  where mtotal = cvR (dccWDS "M" (compoundPhrase' (cn "total mass of the") 
                (CP.rigidBody ^. term)) (phrase $ QPP.mass)) 
                cM

time_c = ucFromCV timec second
  where timec = cvR (dccWDS "time_c" (cn "denotes the time at collision") 
                (phrase $ QP.time)) 
                (sub (QP.time ^. symbol) lC)
-- DD8 --

initRelVel, mass_A, mass_B, massIRigidBody, normalLen, contDisp_A, contDisp_B, 
  perpLen_A, momtInert_A, perpLen_B, momtInert_B :: UnitalChunk

--FIXME: parametrized hack
initRelVel = ucFromCV relVel velU
  where relVel = cvR (dccWDS "v_i^AB" (compoundPhrase'
                 (compoundPhrase' (cn "relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B"))
                 (phrase $ QP.velocity))
                 (sup (sub (QP.velocity ^. symbol) lI) (Concat [cA, cB]))
--FIXME: parametrized hack

rigidParam :: String -> Symbol -> ConVar
rigidParam n w = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term)
                 (cn $ "of rigid body" ++ n)) (phrase $ QPP.mass))
                 (sub (QPP.mass ^. symbol) w)

mass_A = ucFromCV (rigidParam "A" cA) kilogram
mass_B = ucFromCV (rigidParam "B" cB) kilogram
massIRigidBody = ucFromCV massI kilogram
  where massI = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term) 
                 (cn "of the i-th rigid body")) (phrase $ QPP.mass)) 
                 (sub (QPP.mass ^. symbol) cI)
--FIXME: parametrized hack
normalLen = ucFromCV normLen metre
  where normLen = cvR (dccWDS "length of the normal vector" (compoundPhrase'
                  (cn "length of the") (QM.normalVect ^. term))
                  (phrase $ QM.normalVect))
                  (Concat [Atomic "||",(QM.normalVect ^. symbol), Atomic "||"])

contDisp_A = ucFromCV (contParam "A" cA) metre
contDisp_B = ucFromCV (contParam "B" cB) metre

contParam :: String -> Symbol -> ConVar
contParam n w = cvR (dccWDS ("r_" ++ n ++ "P") (contdispN n) (phrase $ QP.displacement))
                  (sub (QP.displacement ^. symbol) (Concat $ [w, cP]))
contdispN :: String -> NP
contdispN n = cn $ "displacement vector between the centre of mass of rigid body " 
  ++ n ++ " and contact point P"


--FIXME: parametrized hack
perpParam :: String -> Symbol -> ConVar
perpParam n w = cvR (dccWDS ("|| r_A" ++ n ++ " x n ||") 
                (compoundPhrase' (compoundPhrase 
                (cn' "length of the") (QM.perpVect ^. term))
                (cn $ "to the contact displacement vector of rigid body" ++ n))
                (phrase $ QM.perpVect)) 
                (Concat [Atomic "||", w, Atomic "*", 
                (QM.perpVect ^. symbol), Atomic "||"])

perpLen_A = ucFromCV (perpParam "A" (contDisp_A ^. symbol)) metre
perpLen_B = ucFromCV (perpParam "B" (contDisp_B ^. symbol)) metre

-- FIXME: parametrized hack
momtParam :: String -> Symbol -> ConVar
momtParam n w = cvR (dccWDS "momentOfInertia" (compoundPhrase'
                (QP.momentOfInertia ^. term) (cn $ "of rigid body" ++ n))
                (phrase $ QP.momentOfInertia))
                (sub (QP.momentOfInertia ^. symbol) w)

momtInert_A = ucFromCV (momtParam "A" cA) momtInertU
momtInert_B = ucFromCV (momtParam "B" cB) momtInertU

timeT, initTime, collTime, velTime :: UnitalChunk
timeT = ucFromCV timet second
  where timet = cvR (dccWDS "t" (cn "point in time")
                (phrase $ QP.time))
                (QP.time ^. symbol)

initTime = ucFromCV timeN second
  where timeN = cvR (dccWDS "t_0" (cn "denotes the initial time")
                   (phrase $ QP.time))
                   (sub (QP.time ^. symbol) (Atomic "0"))

collTime = ucFromCV collisionT second
  where collisionT = cvR (dccWDS "t_c" (cn "denotes the time at collision")
                     (phrase $ QP.time))
                     (sub (QP.time ^. symbol) (Atomic "c"))

velTime = ucFromCV velatTime second
  where velatTime = cvR (dccWDS "t_c" (cn "i-th body's velocity")
                     (phrase $ QP.time))
                     (sub (QP.time ^. symbol) (Atomic "c"))