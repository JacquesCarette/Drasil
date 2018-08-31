module Drasil.GamePhysics.Unitals where

import Language.Drasil
import Data.Drasil.SI_Units(kilogram, metre, m_2, newton, second)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, 
  angularAccel, angularDisplacement, angularVelocity, displacement, distance, 
  force, gravitationalAccel, gravitationalConst, impulseS, impulseV, 
  linearAccel, linearDisplacement, linearVelocity, momentOfInertia, position, 
  restitutionCoef, time, torque, velocity)
import qualified Data.Drasil.Quantities.Math as QM (euclidNorm, normalVect, 
  orientation, perpVect, pi_, unitVect)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.Units.Physics (accelU, angVelU, impulseU, momtInertU, 
  torqueU, velU)

import Control.Lens((^.))



gamephySymbols :: [DefinedQuantityDict]
gamephySymbols = (map dqdWr gamephyUnitSymbs) ++ (map dqdWr cpInputConstraints) ++
  (map dqdWr cpOutputConstraints)

gamephyUnitSymbs :: [UnitaryConceptDict]
gamephyUnitSymbs = map ucw cpUnits ++ map ucw [iVect, jVect, normalVect,
 force_1, force_2, force_i, mass_1, mass_2, dispUnit, 
  dispNorm, sqrDist, vel_A, vel_B, vel_O, r_OB, angVel_A, angVel_B,
  pos_CM, mass_i, pos_i, acc_i, mTot, vel_i, torque_i, time_c, initRelVel, 
  mass_A, mass_B, massIRigidBody, normalLen, contDisp_A, contDisp_B, 
  perpLen_A, momtInert_A, perpLen_B, momtInert_B, timeT, initTime, 
  momtInert_k, pointOfCollision, contDisp_k, collisionImpulse]

----------------------
-- TABLE OF SYMBOLS --
----------------------

cpSymbols, cpSymbolsAll, inputSymbols, outputSymbols :: [QuantityDict]

-- FIXME: pi hack
cpSymbolsAll = cpSymbols ++ inputSymbols ++ outputSymbols ++ [QM.pi_]

cpSymbols = (map qw cpUnits) ++ 
  (map qw cpUnitless) ++ 
  (map qw cpInputConstraints)

inputSymbols = map qw [QP.position, QP.velocity, QP.force, QM.orientation, 
  QP.angularVelocity, QP.linearVelocity, QP.gravitationalConst, QPP.mass, 
  QPP.len, QP.momentOfInertia, QP.torque] ++ [qw QP.restitutionCoef]

outputSymbols = map qw [QP.position, QP.velocity, QM.orientation, 
  QP.angularVelocity]


cpUnits :: [UnitalChunk]
cpUnits = [QP.acceleration, QP.angularAccel, QP.gravitationalAccel, 
  QP.impulseV, QP.impulseS, iVect, jVect, normalVect, QP.distance, QP.displacement, 
  QP.time, QP.angularDisplacement, pos_CM, pos_i, mass_i, mTot, acc_i, vel_i,
  QP.linearDisplacement, QP.linearVelocity, QP.linearAccel, initRelVel, normalLen,
  perpLen_A, perpLen_B, force_i, torque_i, time_c, vel_A, vel_B, mass_A, mass_B,
  angVel_A, angVel_B, force_1, force_2, mass_1, mass_2, dispUnit, 
  dispNorm, sqrDist, vel_O, r_OB, massIRigidBody, contDisp_A, contDisp_B, 
  momtInert_A, momtInert_B, timeT, initTime,  
  momtInert_k, pointOfCollision, contDisp_k, collisionImpulse]

-----------------------
-- PARAMETRIZED HACK --
-----------------------
--FIXME: parametrized hack
--FIXME: "A" is not being capitalized when it should be.
forceParam, massParam, momtParam, contParam :: String -> String -> DefinedQuantityDict
forceParam n w = dqdEL
 (dccWDS ("force" ++ n) (cn $ "force exerted by the " ++ w ++ 
  " body (on another body)") (phrase QP.force)) 
  (sub (eqSymb QP.force) (Atomic n)) Real newton

massParam n w = dqdEL
 (dccWDS ("mass" ++ n) (cn $ "mass of the " ++ w ++ " body") 
  (phrase QPP.mass)) (sub (eqSymb QPP.mass) (Atomic n)) Real kilogram

momtParam n w = dqdEL
 (dccWDS ("momentOfInertia" ++ n) (compoundPhrase'
  (QP.momentOfInertia ^. term) (cn $ "of rigid body " ++ n))
  (phrase QP.momentOfInertia)) (sub (eqSymb QP.momentOfInertia) (Atomic w)) Real momtInertU

contParam n w = dqdEL
 (dccWDS ("r_" ++ n ++ "P") (contdispN n) 
  (phrase QP.displacement)) (sub (eqSymb QP.displacement)
  (Concat $ [Atomic w, cP])) Real metre

contdispN :: String -> NP
contdispN n = cn $ "displacement vector between the centre of mass of rigid body " 
  ++ n ++ " and contact point P"

perpParam, rigidParam, velParam, 
  angParam :: String -> Symbol -> DefinedQuantityDict

velParam n w = dqdEL
 (dccWDS ("velocity" ++ n) (compoundPhrase' (QP.velocity ^. term)
  (cn $ "at point " ++ n)) (phrase QP.velocity)) (sub (eqSymb QP.velocity) w) Real velU

angParam n w = dqdEL
 (dccWDS ("angular velocity" ++ n) (compoundPhrase'
  (cn $ n ++ " body's") (QP.angularVelocity ^. term))
  (phrase QP.angularVelocity)) (sub (eqSymb QP.angularVelocity) w) Real angVelU

perpParam n w = dqdEL
 (dccWDS ("|| r_A" ++ n ++ " x n ||") 
  (compoundPhrase' (compoundPhrase (cn' "length of the") (QM.perpVect ^. term))
  (cn $ "to the contact displacement vector of rigid body " ++ n)) 
  (phrase QM.perpVect)) (Concat [Atomic "||", w, Atomic "*", --should be x for cross
  (eqSymb QM.perpVect), Atomic "||"]) Real metre

rigidParam n w = dqdEL
 (dccWDS ("rig_mass" ++ n) (compoundPhrase' (QPP.mass ^. term)
  (cn $ "of rigid body " ++ n)) (phrase QPP.mass)) (sub (eqSymb QPP.mass) w) Real kilogram

-----------------------
-- CHUNKS WITH UNITS --
-----------------------

iVect, jVect, normalVect, force_1, force_2, force_i, mass_1, mass_2, dispUnit, 
  dispNorm, sqrDist, vel_A, vel_B, vel_O, r_OB, angVel_A, angVel_B,
  pos_CM, mass_i, pos_i, acc_i, mTot, vel_i, torque_i, time_c, initRelVel, 
  mass_A, mass_B, massIRigidBody, normalLen, contDisp_A, contDisp_B, 
  perpLen_A, momtInert_A, perpLen_B, momtInert_B, timeT, initTime, 
  momtInert_k, pointOfCollision, contDisp_k, collisionImpulse :: UnitalChunk

-- FIXME: parametrized hack
iVect = ucFromDQD ivec
  where ivec = dqdEL (dccWDS "unitVect" (compoundPhrase' (cn "horizontal")
               (QM.unitVect ^. term)) (phrase QM.unitVect)) 
               (eqSymb QM.unitVect) Real metre
-- FIXME: parametrized hack
jVect       = ucFromDQD ivec
  where ivec = dqdEL (dccWDS "unitVectJ" (compoundPhrase' (cn "vertical")
               (QM.unitVect ^. term)) (phrase QM.unitVect)) (vec $ hat lJ) Real metre
-- FIXME: parametrized hack
normalVect  = ucFromDQD normVect
  where normVect = dqdEL (dccWDS "normalVect" (compoundPhrase' (cn "collision")
                   (QM.normalVect ^. term)) (phrase QM.normalVect)) 
                   (eqSymb QM.normalVect) Real metre

dispUnit = ucFromDQD dispVect
  where dispVect = dqdEL (dccWDS "dispUnit" (cn "displacement unit vector") 
                   (S "displacement" +:+ (phrase QM.unitVect))) (vec (hat lR)) Real metre

-- FIXME: parametrized hack
dispNorm = ucFromDQD norm
  where norm = dqdEL (dccWDS "euclideanNormDisp" (cn "Euclidean norm of the displacement")
               (phrase QM.euclidNorm) ) (eqSymb QM.euclidNorm) Real metre

-- FIXME: parametrized hack
sqrDist = ucFromDQD norm
  where norm = dqdEL (dccWDS "euclideanNorm" (cn' "squared distance")
               (phrase QM.euclidNorm) ) (sup (eqSymb QM.euclidNorm) 
               (Atomic "2")) Real m_2

r_OB    = uc' "r_OB" 
  (nounPhraseSP "displacement vector between the origin and point B")
  "FIXME: Define this or remove the need for definitions" 
  (sub (eqSymb QP.displacement) (Concat [cO, cB])) metre

pos_CM = ucs "p_CM" (nounPhraseSP $ 
  "mass-weighted average position of a rigid " ++
  "body's particles") 
  "FIXME: Define this or remove the need for definitions" 
  (sub (eqSymb QP.position) (Atomic "CM")) metre Real

--FIXME: parametrized hack
mass_i = ucFromDQD massi
  where massi = dqdEL (dccWDS "m_j" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the j-th particle")) (phrase QPP.mass)) 
                (sub (eqSymb QPP.mass) lJ) Real kilogram

pos_i = ucFromDQD posi
  where posi = dqdEL (dccWDS "p_j" (compoundPhrase' (QP.position ^. term) 
               (cn "vector of the j-th particle")) (phrase QP.position))
               (sub (eqSymb QP.position) lJ) Real metre

acc_i = ucFromDQD accI
  where accI = dqdEL (dccWDS "acc_i" (compoundPhrase' (cn "the i-th body's")
               (QP.acceleration ^. term)) (phrase QP.acceleration))
               (sub (eqSymb QP.acceleration) lI) Real accelU

vel_i = ucFromDQD accI
  where accI = dqdEL (dccWDS "vel_i" (compoundPhrase' (QP.velocity ^. term) 
               (cn "of the i-th body's velocity")) (phrase QP.velocity))
               (sub (eqSymb QP.velocity) lI) Real velU

torque_i = ucFromDQD torI
  where torI = dqdEL (dccWDS "torque_i" 
               (cn "torque applied to the i-th body")
               (phrase QP.torque)) (sub (eqSymb QP.torque) lI) Real torqueU

mTot = ucFromDQD mtotal
  where mtotal = dqdEL (dccWDS "M" (compoundPhrase' (cn "total mass of the") 
                 (CP.rigidBody ^. term)) (phrase QPP.mass)) cM Real kilogram

time_c = ucFromDQD timec
  where timec = dqdEL (dccWDS "time_c" (cn "denotes the time at collision") 
                (phrase QP.time)) (sub (eqSymb QP.time) lC) Real second

--FIXME: parametrized hack
initRelVel = ucFromDQD relVel
  where relVel = dqdEL (dccWDS "v_i^AB" (compoundPhrase'
                 (compoundPhrase' (cn "relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B")) (phrase QP.velocity))
                 (sup (sub (eqSymb QP.velocity) lI) (Concat [cA, cB])) Real velU

--FIXME: parametrized hack
massIRigidBody = ucFromDQD massI
  where massI = dqdEL (dccWDS "massI" (compoundPhrase' (QPP.mass ^. term) 
                (cn "of the i-th rigid body")) (phrase QPP.mass)) 
                (sub (eqSymb QPP.mass) lI) Real kilogram
--FIXME: parametrized hack
normalLen = ucFromDQD normLen
  where normLen = dqdEL (dccWDS "length of the normal vector" (compoundPhrase'
                  (cn "length of the") (QM.normalVect ^. term)) 
                  (phrase QM.normalVect))
                  (Concat [Atomic "||",(eqSymb QM.normalVect), Atomic "||"]) Real metre

timeT = ucFromDQD timet
  where timet = dqdEL (dccWDS "t" (cn "point in time") (phrase QP.time))
                (eqSymb QP.time) Real second

initTime = ucFromDQD timeN
  where timeN = dqdEL (dccWDS "t_0" (cn "denotes the initial time") 
                (phrase QP.time)) (sub (eqSymb QP.time) (Atomic "0")) Real second

momtInert_k = ucFromDQD momtK
 where momtK = dqdEL (dccWDS "momentOfInertiaK" (compoundPhrase'
               (QP.momentOfInertia ^. term) 
               (cn $ "of the k-th rigid body"))
               (phrase QP.momentOfInertia)) 
               (sub (eqSymb QP.momentOfInertia) lK) Real momtInertU

pointOfCollision = ucFromDQD pointC
  where pointC = dqdEL (dccWDS "point_c" (cn "point of collision") 
                 (S "point")) cP Real metre

collisionImpulse = ucFromDQD impul
  where impul = dqdEL (dccWDS "collisionImp" (compoundPhrase' 
                (cn $ "collision") (QP.impulseS ^. term)) (phrase QP.impulseS)) 
                (eqSymb QP.impulseS) Real impulseU


force_i = ucFromDQD theforce
  where 
    theforce = dqdEL (dccWDS "force_i" (compoundPhrase' 
      (QP.force ^. term) (cn "applied to the i-th body at time t")) 
      (phrase QP.force)) (sub (eqSymb QP.force) lI) Real newton

force_1     = ucFromDQD (forceParam "1" "first")
force_2     = ucFromDQD (forceParam "2" "second")
mass_1      = ucFromDQD (massParam "1" "first")
mass_2      = ucFromDQD (massParam "2" "second")
vel_A       = ucFromDQD (velParam "A" cA)
vel_B       = ucFromDQD (velParam "B" cB)
vel_O       = ucFromDQD (velParam "origin" cO)
angVel_A    = ucFromDQD (angParam "A" cA)
angVel_B    = ucFromDQD (angParam "B" cB)
perpLen_A   = ucFromDQD (perpParam "A" $ eqSymb contDisp_A)
perpLen_B   = ucFromDQD (perpParam "B" $ eqSymb contDisp_B)
momtInert_A = ucFromDQD (momtParam "A" "A")
momtInert_B = ucFromDQD (momtParam "B" "B")
contDisp_A  = ucFromDQD (contParam "A" "A")
contDisp_B  = ucFromDQD (contParam "B" "B")
contDisp_k  = ucFromDQD (contParam "k" "k")
mass_A      = ucFromDQD (rigidParam "A" cA)
mass_B      = ucFromDQD (rigidParam "B" cB)

--------------------------
-- CHUNKS WITHOUT UNITS --
--------------------------

cpUnitless :: [VarChunk]
cpUnitless = [numParticles]

numParticles :: VarChunk
numParticles = vc "n" (nounPhraseSP "number of particles in a rigid body") lN Integer

-----------------------
-- CONSTRAINT CHUNKS --
-----------------------

lengthCons, massCons, mmntOfInCons, gravAccelCons, posCons, orientCons,
  angVeloCons, forceCons, torqueCons, veloCons, restCoefCons :: ConstrConcept

cpInputConstraints :: [UncertQ]
cpInputConstraints = map (\x -> uq x (0.1 :: Double))
  [lengthCons, massCons, mmntOfInCons, gravAccelCons, posCons, orientCons,
  veloCons, angVeloCons, forceCons, torqueCons, restCoefCons]

cpOutputConstraints :: [UncertQ]
cpOutputConstraints = map (\x -> uq x (0.1 :: Double)) 
  [posCons, veloCons, orientCons, angVeloCons]

nonNegativeConstraint :: Constraint -- should be pulled out and put somewhere for generic constraints
nonNegativeConstraint = physc $ UpFrom (Inc,0)

lengthCons     = constrained' QPP.len               [nonNegativeConstraint] (dbl 44.2)
massCons       = constrained' QPP.mass              [nonNegativeConstraint] (dbl 56.2)
mmntOfInCons   = constrained' QP.momentOfInertia    [nonNegativeConstraint] (dbl 74.5)
gravAccelCons  = constrained' QP.gravitationalConst [] (dbl 9.8)
posCons        = constrained' QP.position           [] (dbl 0.412) --FIXME: should be (0.412, 0.502) vector
veloCons       = constrained' QP.velocity           [] (dbl 2.51)
orientCons     = constrained' QM.orientation        [] (sy QM.pi_ / 2) -- physical constraint not needed space is radians
angVeloCons    = constrained' QP.angularVelocity    [] (dbl 2.1)
forceCons      = constrained' QP.force              [] (dbl 98.1)
torqueCons     = constrained' QP.torque             [] (dbl 200)
restCoefCons   = constrained' QP.restitutionCoef    [physc $ Bounded (Inc,0) (Inc,1)] (dbl 0.8)

---------------------
-- INSTANCE MODELS --
---------------------

transMotLegTerms, rotMotLegTerms, col2DLegTerms :: [UnitalChunk]
transMotLegTerms = [massIRigidBody, QP.gravitationalAccel, timeT, initTime, pos_CM,
  QP.acceleration, QP.velocity, force_i]

rotMotLegTerms = [massIRigidBody, QP.gravitationalAccel, timeT, initTime,
  QM.orientation, QP.angularVelocity, QP.angularAccel, torque_i, momtInert_k]

col2DLegTerms = [massIRigidBody, momtInert_k, timeT, initTime, time_c, pos_CM,
  QP.velocity, QM.orientation, QP.angularVelocity, normalVect, -- +:+. S "Its signed direction is determined by (A4)",
  collisionImpulse, pointOfCollision, contDisp_k]

---------------------
-- GOAL STATEMENTS --
---------------------

