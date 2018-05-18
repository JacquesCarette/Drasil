module Drasil.GamePhysics.Unitals where

import Language.Drasil
import Data.Drasil.SI_Units
import qualified Data.Drasil.Concepts.Physics as CP
import qualified Data.Drasil.Quantities.Physics as QP
import qualified Data.Drasil.Quantities.Math as QM
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP
import Data.Drasil.Units.Physics

import Control.Lens((^.))

----------------------
-- TABLE OF SYMBOLS --
----------------------

cpSymbols, cpSymbolsAll, inputSymbols, outputSymbols :: [QuantityDict]

-- FIXME: pi hack
cpSymbolsAll = cpSymbols ++ inputSymbols ++ outputSymbols ++ [pi_]

cpSymbols = (map qw cpUnits) ++ 
  (map qw cpUnitless) ++ (map qw cpInputConstraints)

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
forceParam n w = cqsEL
 (dccWDS ("force" ++ n) (cn $ "force exerted by the " ++ w ++ 
  " body (on another body)") (phrase QP.force)) 
  (sub (eqSymb QP.force) (Atomic n)) Real

massParam n w = cqsEL
 (dccWDS ("mass" ++ n) (cn $ "mass of the " ++ w ++ " body") 
  (phrase QPP.mass)) (sub (eqSymb QPP.mass) (Atomic n)) Real

momtParam n w = cqsEL
 (dccWDS ("momentOfInertia" ++ n) (compoundPhrase'
  (QP.momentOfInertia ^. term) (cn $ "of rigid body " ++ n))
  (phrase QP.momentOfInertia)) (sub (eqSymb QP.momentOfInertia) (Atomic w)) Real

contParam n w = cqsEL
 (dccWDS ("r_" ++ n ++ "P") (contdispN n) 
  (phrase QP.displacement)) (sub (eqSymb QP.displacement)
  (Concat $ [Atomic w, cP])) Real

contdispN :: String -> NP
contdispN n = cn $ "displacement vector between the centre of mass of rigid body " 
  ++ n ++ " and contact point P"

perpParam, rigidParam, velParam, 
  angParam :: String -> Symbol -> DefinedQuantityDict

velParam n w = cqsEL
 (dccWDS ("velocity" ++ n) (compoundPhrase' (QP.velocity ^. term)
  (cn $ "at point " ++ n)) (phrase QP.velocity)) (sub (eqSymb QP.velocity) w) Real

angParam n w = cqsEL
 (dccWDS ("angular velocity" ++ n) (compoundPhrase'
  (cn $ "is the " ++ n ++ " body's") (QP.angularVelocity ^. term))
  (phrase QP.angularVelocity)) (sub (eqSymb QP.angularVelocity) w) Real

perpParam n w = cqsEL
 (dccWDS ("|| r_A" ++ n ++ " x n ||") 
  (compoundPhrase' (compoundPhrase (cn' "length of the") (QM.perpVect ^. term))
  (cn $ "to the contact displacement vector of rigid body " ++ n)) 
  (phrase QM.perpVect)) (Concat [Atomic "||", w, Atomic "*", --should be x for cross
  (eqSymb QM.perpVect), Atomic "||"]) Real

rigidParam n w = cqsEL
 (dccWDS ("rig_mass" ++ n) (compoundPhrase' (QPP.mass ^. term)
  (cn $ "of rigid body " ++ n)) (phrase QPP.mass)) (sub (eqSymb QPP.mass) w) Real

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
iVect = ucFromDQD ivec metre
  where ivec = cqsEL (dccWDS "unitVect" (compoundPhrase' (cn "horizontal")
               (QM.unitVect ^. term)) (phrase QM.unitVect)) 
               (eqSymb QM.unitVect) Real
-- FIXME: parametrized hack
jVect       = ucFromDQD ivec metre
  where ivec = cqsEL (dccWDS "unitVectJ" (compoundPhrase' (cn "vertical")
               (QM.unitVect ^. term)) (phrase QM.unitVect)) (vec $ hat lJ) Real
-- FIXME: parametrized hack
normalVect  = ucFromDQD normVect metre
  where normVect = cqsEL (dccWDS "normalVect" (compoundPhrase' (cn "collision")
                   (QM.normalVect ^. term)) (phrase QM.normalVect)) 
                   (eqSymb QM.normalVect) Real

dispUnit = ucFromDQD dispVect metre
  where dispVect = cqsEL (dccWDS "dispUnit" (cn "displacement unit vector") 
                   (S "displacement" +:+ (phrase QM.unitVect))) (vec (hat lR)) Real

-- FIXME: parametrized hack
dispNorm = ucFromDQD norm metre
  where norm = cqsEL (dccWDS "euclideanNormDisp" (cn "Euclidean norm of the displacement")
               (phrase QM.euclidNorm) ) (eqSymb QM.euclidNorm) Real

-- FIXME: parametrized hack
sqrDist = ucFromDQD norm m_2
  where norm = cqsEL (dccWDS "euclideanNorm" (cn' "squared distance")
               (phrase QM.euclidNorm) ) (sup (eqSymb QM.euclidNorm) 
               (Atomic "2")) Real

r_OB    = uc' "r_OB" 
  (nounPhraseSP "displacement vector between the origin and point B")
  "FIXME: Define this or remove the need for definitions" 
  (sub (eqSymb QP.displacement) (Concat [cO, cB])) metre

pos_CM = ucs "p_CM" (nounPhraseSP $ 
  "the mass-weighted average position of a rigid " ++
  "body's particles") 
  "FIXME: Define this or remove the need for definitions" 
  (sub (eqSymb QP.position) (Atomic "CM")) metre Real

--FIXME: parametrized hack
mass_i = ucFromDQD massi kilogram
  where massi = cqsEL (dccWDS "m_j" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the j-th particle")) (phrase QPP.mass)) 
                (sub (eqSymb QPP.mass) lJ) Real

pos_i = ucFromDQD posi metre
  where posi = cqsEL (dccWDS "p_j" (compoundPhrase' (QP.position ^. term) 
               (cn "vector of the j-th particle")) (phrase QP.position))
               (sub (eqSymb QP.position) lJ) Real

acc_i = ucFromDQD accI accelU
  where accI = cqsEL (dccWDS "acc_i" (compoundPhrase' (cn "the i-th body's")
               (QP.acceleration ^. term)) (phrase QP.acceleration))
               (sub (eqSymb QP.acceleration) lI) Real

vel_i = ucFromDQD accI velU
  where accI = cqsEL (dccWDS "vel_i" (compoundPhrase' (QP.velocity ^. term) 
               (cn "of the i-th body's velocity")) (phrase QP.velocity))
               (sub (eqSymb QP.velocity) lI) Real

torque_i = ucFromDQD torI torqueU
  where torI = cqsEL (dccWDS "torque_i" 
               (cn "is the torque applied to the i-th body")
               (phrase QP.torque)) (sub (eqSymb QP.torque) lI) Real

mTot = ucFromDQD mtotal kilogram
  where mtotal = cqsEL (dccWDS "M" (compoundPhrase' (cn "total mass of the") 
                 (CP.rigidBody ^. term)) (phrase QPP.mass)) cM Real

time_c = ucFromDQD timec second
  where timec = cqsEL (dccWDS "time_c" (cn "denotes the time at collision") 
                (phrase QP.time)) (sub (eqSymb QP.time) lC) Real

--FIXME: parametrized hack
initRelVel = ucFromDQD relVel velU
  where relVel = cqsEL (dccWDS "v_i^AB" (compoundPhrase'
                 (compoundPhrase' (cn "relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B")) (phrase QP.velocity))
                 (sup (sub (eqSymb QP.velocity) lI) (Concat [cA, cB])) Real

--FIXME: parametrized hack
massIRigidBody = ucFromDQD massI kilogram
  where massI = cqsEL (dccWDS "massI" (compoundPhrase' (QPP.mass ^. term) 
                (cn "of the i-th rigid body")) (phrase QPP.mass)) 
                (sub (eqSymb QPP.mass) lI) Real
--FIXME: parametrized hack
normalLen = ucFromDQD normLen metre
  where normLen = cqsEL (dccWDS "length of the normal vector" (compoundPhrase'
                  (cn "length of the") (QM.normalVect ^. term)) 
                  (phrase QM.normalVect))
                  (Concat [Atomic "||",(eqSymb QM.normalVect), Atomic "||"]) Real

timeT = ucFromDQD timet second
  where timet = cqsEL (dccWDS "t" (cn "point in time") (phrase QP.time))
                (eqSymb QP.time) Real

initTime = ucFromDQD timeN second
  where timeN = cqsEL (dccWDS "t_0" (cn "denotes the initial time") 
                (phrase QP.time)) (sub (eqSymb QP.time) (Atomic "0")) Real

momtInert_k = ucFromDQD momtK momtInertU
 where momtK = cqsEL (dccWDS "momentOfInertiaK" (compoundPhrase'
               (QP.momentOfInertia ^. term) 
               (cn $ "of the k-th rigid body"))
               (phrase QP.momentOfInertia)) 
               (sub (eqSymb QP.momentOfInertia) lK) Real

pointOfCollision = ucFromDQD pointC metre
  where pointC = cqsEL (dccWDS "point_c" (cn "point of collision") 
                 (S "point")) cP Real

collisionImpulse = ucFromDQD impul impulseU
  where impul = cqsEL (dccWDS "collisionImp" (compoundPhrase' 
                (cn $ "collision") (QP.impulseS ^. term)) (phrase QP.impulseS)) 
                (eqSymb QP.impulseS) Real


force_i = ucFromDQD theforce newton
  where theforce = cqsEL (dccWDS "force_i" (compoundPhrase' 
                (QP.force ^. term) (cn "applied to the i-th body at time t")) 
                (phrase QP.force)) (sub (eqSymb QP.force) lI) Real

force_1     = ucFromDQD (forceParam "1" "first")               newton
force_2     = ucFromDQD (forceParam "2" "second")              newton
mass_1      = ucFromDQD (massParam "1" "first")                kilogram
mass_2      = ucFromDQD (massParam "2" "second")               kilogram
vel_A       = ucFromDQD (velParam "A" cA)                      velU
vel_B       = ucFromDQD (velParam "B" cB)                      velU
vel_O       = ucFromDQD (velParam "origin" cO)                 velU
angVel_A    = ucFromDQD (angParam "A" cA)                      angVelU
angVel_B    = ucFromDQD (angParam "B" cB)                      angVelU
perpLen_A   = ucFromDQD (perpParam "A" $ eqSymb contDisp_A)    metre
perpLen_B   = ucFromDQD (perpParam "B" $ eqSymb contDisp_B)    metre
momtInert_A = ucFromDQD (momtParam "A" "A")                    momtInertU
momtInert_B = ucFromDQD (momtParam "B" "B")                    momtInertU
contDisp_A  = ucFromDQD (contParam "A" "A")                    metre
contDisp_B  = ucFromDQD (contParam "B" "B")                    metre
contDisp_k  = ucFromDQD (contParam "k" "k")                    metre
mass_A      = ucFromDQD (rigidParam "A" cA)                    kilogram
mass_B      = ucFromDQD (rigidParam "B" cB)                    kilogram

--------------------------
-- CHUNKS WITHOUT UNITS --
--------------------------

cpUnitless :: [VarChunk]
cpUnitless = [numParticles]

numParticles :: VarChunk
numParticles = vc "n" (nounPhraseSP "number of particles in a rigid body") lN Integer []


-----------------------
-- CONSTRAINT CHUNKS --
-----------------------

lengthCons, massCons, mmntOfInCons, gravAccelCons, posCons, orientCons,
  angVeloCons, forceCons, torqueCons, veloCons, restCoefCons :: ConstrConcept

cpInputConstraints :: [Maybe UnitDefn -> UncertQ]
cpInputConstraints = map (\x -> uq x (0.1 :: Double))
  [lengthCons, massCons, mmntOfInCons, gravAccelCons, posCons, orientCons,
  veloCons, angVeloCons, forceCons, torqueCons, restCoefCons]

cpOutputConstraints :: [Maybe UnitDefn -> UncertQ]
cpOutputConstraints = map (\x -> uq x (0.1 :: Double)) 
  [posCons, veloCons, orientCons, angVeloCons]

nonNegativeConstraint :: Constraint -- should be pulled out and put somewhere for generic constraints
nonNegativeConstraint = physc $ UpFrom (Inc,0)

-- FIXME
pi_ :: QuantityDict
pi_ = mkQuant "pi" (nounPhraseSP "pi") (Greek Pi_L) Real Nothing Nothing []

lengthCons     = constrained' QPP.len               [nonNegativeConstraint] (dbl 44.2) Nothing
massCons       = constrained' QPP.mass              [nonNegativeConstraint] (dbl 56.2) Nothing
mmntOfInCons   = constrained' QP.momentOfInertia    [nonNegativeConstraint] (dbl 74.5) Nothing
gravAccelCons  = constrained' QP.gravitationalConst [] (dbl 9.8) Nothing
posCons        = constrained' QP.position           [] (dbl 0.412) Nothing --FIXME: should be (0.412, 0.502) vector
veloCons       = constrained' QP.velocity           [] (dbl 2.51) Nothing
orientCons     = constrained' QM.orientation        [] (sy pi_ / 2) Nothing -- physical constraint not needed space is radians
angVeloCons    = constrained' QP.angularVelocity    [] (dbl 2.1) Nothing
forceCons      = constrained' QP.force              [] (dbl 98.1) Nothing
torqueCons     = constrained' QP.torque             [] (dbl 200) Nothing
restCoefCons   = constrained' QP.restitutionCoef    [physc $ Bounded (Inc,0) (Inc,1)] (dbl 0.8) Nothing

---------------------
-- INSTANCE MODELS --
---------------------

im1legTerms, im2legTerms, im3legTerms :: [UnitalChunk]
im1legTerms = [massIRigidBody, QP.gravitationalAccel, timeT, initTime, pos_CM, 
  QP.acceleration, QP.velocity, force_i]

im2legTerms = [massIRigidBody, QP.gravitationalAccel, timeT, initTime, 
  QM.orientation, QP.angularVelocity, QP.angularAccel, torque_i, momtInert_k]

im3legTerms = [massIRigidBody, momtInert_k, timeT, initTime, time_c, pos_CM,
  QP.velocity, QM.orientation, QP.angularVelocity, normalVect, -- +:+. S "Its signed direction is determined by (A4)",
  collisionImpulse, pointOfCollision, contDisp_k]

---------------------
-- GOAL STATEMENTS --
---------------------

