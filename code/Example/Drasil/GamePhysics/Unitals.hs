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

force_1, force_2, force_i :: UnitalChunk
force_1 = ucFromVC (forceParam "1" "first") newton
force_2 = ucFromVC (forceParam "2" "second") newton
force_i = ucFromVC (forceParam "i" "i-th") newton

forceParam, massParam :: String -> String -> ConVar
forceParam n w = cvR (dccWDS "force" (QP.force ^. term)
                 (phrase $ compoundPhrase' (QP.force ^. term)
                 (cn $ "exerted by the" ++ w ++ "body (on another body)")))
                 (sub (QP.force ^. symbol) (Atomic n))
massParam n w = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term)
                (cn $ "of the" ++ w ++ "body")) (phrase $ QPP.mass ^. term))
                (sub (QPP.mass ^. symbol) (Atomic n))
-- T3 --
mass_1, mass_2, dispUnit, dispNorm, sqrDist :: UnitalChunk

mass_1 = ucFromVC (massParam "1" "first") kilogram
mass_2 = ucFromVC (massParam "2" "second") kilogram

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

velParam :: String -> Symbol -> ConVar
velParam n w = cvR (dccWDS "velocity" (compoundPhrase' (QP.velocity ^. term)
               (cn $ "at point" ++ n)) (phrase $ QP.velocity ^. term))
               (sub (QP.velocity ^. symbol) w)

-- FIXME: parametrized hack
vel_B   = ucFromVC (velParam "B" cB) velU
vel_O   = ucFromVC (velParam "origin" cO) velU

r_OB    = uc' "r_OB" 
  (nounPhraseSP "displacement vector between the origin and point B")
  "FIXME: Define this or remove the need for definitions" 
  (sub (disp ^. symbol) (Concat [cO, cB])) metre

-- DD1 --

pos_CM, mass_i, pos_i, acc_i, mTot :: UnitalChunk

pos_CM = uc' "p_CM" (nounPhraseSP $ 
  "the mass-weighted average position of a rigid " ++
  "body's particles") 
  "FIXME: Define this or remove the need for definitions" 
  (sub (position ^. symbol) (Atomic "CM")) metre

--FIXME: parametrized hack
mass_i = ucFromVC massi kilogram
  where massi = cvR (dccWDS "m_i" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the i-th particle")) (phrase $ QPP.mass ^. term))
                (sub (QPP.mass ^. symbol) lI)
pos_i = ucFromVC posi metre
  where posi = cvR (dccWDS "p_i" (compoundPhrase' (position ^. term) 
                (cn "vector of the i-th particle")) (phrase $ position ^. term))
                (sub (position ^. symbol) lI)

acc_i = ucFromVC accI accelU
  where accI = cvR (dccWDS "acc_i" (compoundPhrase' (accel ^. term) 
                (cn "of the i-th body's acceleration")) (phrase $ accel ^. term))
                (sub (accel ^. symbol) lI)

mTot = ucFromVC mtotal kilogram
  where mtotal = cvR (dccWDS "M" (compoundPhrase' (cn "total mass of the") 
                (CP.rigidBody ^. term)) (phrase $ QPP.mass ^. term)) 
                cM
-- DD8 --

initRelVel, mass_A, mass_B, massIRigidBody, normalLen, contDisp_A, contDisp_B, 
  perpLen_A, momtInert_A, perpLen_B, momtInert_B :: UnitalChunk

--FIXME: parametrized hack
initRelVel = ucFromVC relVel velU
  where relVel = cvR (dccWDS "v_i^AB" (compoundPhrase'
                 (compoundPhrase' (cn "relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B"))
                 (phrase $ QP.velocity ^. term))
                 (sup (sub (QP.velocity ^. symbol) lI) (Concat [cA, cB]))
--FIXME: parametrized hack

rigidParam :: String -> Symbol -> ConVar
rigidParam n w = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term)
                 (cn $ "of rigid body" ++ n)) (phrase $ QPP.mass ^. term))
                 (sub (QPP.mass ^. symbol) w)

mass_A = ucFromVC (rigidParam "A" cA) kilogram
mass_B = ucFromVC (rigidParam "B" cB) kilogram
massIRigidBody = ucFromVC massI kilogram
  where massI = cvR (dccWDS "mass" (compoundPhrase' (QPP.mass ^. term) 
                 (cn "of the i-th rigid body")) (phrase $ QPP.mass ^. term)) 
                 (sub (QPP.mass ^. symbol) cI)
--FIXME: parametrized hack
normalLen = ucFromVC normLen metre
  where normLen = cvR (dccWDS "length of the normal vector" (compoundPhrase'
                  (cn "length of the") (QM.normalVect ^. term))
                  (phrase $ QM.normalVect ^. term))
                  (Concat [Atomic "||",(QM.normalVect ^. symbol), Atomic "||"])

contDisp_A = ucFromVC (contParam "A" cA) metre
contDisp_B = ucFromVC (contParam "B" cB) metre

contParam :: String -> Symbol -> ConVar
contParam n w = cvR (dccWDS ("r_" ++ n ++ "P") (contdispN n) (phrase $ disp ^. term))
                  (sub (disp ^. symbol) (Concat $ [w, cP]))
contdispN :: String -> NP
contdispN n = cn $ "displacement vector between the centre of mass of rigid body " 
  ++ n ++ " and contact point P"


--FIXME: parametrized hack
perpParam :: String -> Symbol -> ConVar
perpParam n w = cvR (dccWDS ("|| r_A" ++ n ++ " x n ||") 
                (compoundPhrase' (compoundPhrase 
                (cn' "length of the") (QM.perpVect ^. term))
                (cn $ "to the contact displacement vector of rigid body" ++ n))
                (phrase $ QM.perpVect ^. term)) 
                (Concat [Atomic "||", w, Atomic "*", 
                (QM.perpVect ^. symbol), Atomic "||"])

perpLen_A = ucFromVC (perpParam "A" (contDisp_A ^. symbol)) metre
perpLen_B = ucFromVC (perpParam "B" (contDisp_B ^. symbol)) metre

-- FIXME: parametrized hack
momtParam :: String -> Symbol -> ConVar
momtParam n w = cvR (dccWDS "momentOfInertia" (compoundPhrase'
                (QP.momentOfInertia ^. term) (cn $ "of rigid body" ++ n))
                (phrase $ QP.momentOfInertia ^. term))
                (sub (momtInert ^. symbol) w)

momtInert_A = ucFromVC (momtParam "A" cA) momtInertU
momtInert_B = ucFromVC (momtParam "B" cB) momtInertU

timeT, initTime, collTime, velTime :: UnitalChunk
timeT = ucFromVC timet second
  where timet = cvR (dccWDS "t" (cn "point in time")
                (phrase $ QP.time ^. term))
                (QP.time ^. symbol)

initTime = ucFromVC timeN second
  where timeN = cvR (dccWDS "t_0" (cn "denotes the initial time")
                   (phrase $ QP.time ^. term))
                   (sub (QP.time ^. symbol) (Atomic "0"))

collTime = ucFromVC collisionT second
  where collisionT = cvR (dccWDS "t_c" (cn "denotes the time at collision")
                     (phrase $ QP.time ^. term))
                     (sub (QP.time ^. symbol) (Atomic "c"))

velTime = ucFromVC velatTime second
  where velatTime = cvR (dccWDS "t_c" (cn "i-th body's velocity")
                     (phrase $ QP.time ^. term))
                     (sub (QP.time ^. symbol) (Atomic "c"))