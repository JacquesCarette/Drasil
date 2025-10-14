module Drasil.GamePhysics.Unitals where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import Language.Drasil.Display (Symbol(..), Decoration(Magnitude))
import Language.Drasil.ShortHands

import Data.Drasil.SI_Units(kilogram, metre, m_2, newton, second)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, angularAccel,
  angularDisplacement, angularVelocity, chgInVelocity, chgMomentum, displacement, distance,
  final, force, gravitationalAccel, gravitationalConst, gravitationalConstValue,
  height, impulseS, impulseV, initial, kEnergy, linearAccel, linearDisplacement,
  linearVelocity, momentOfInertia, position, potEnergy, restitutionCoef, time,
  torque, velocity, fOfGravity, positionVec)

import qualified Data.Drasil.Quantities.Math as QM (euclidNorm, normalVect,
  orientation, perpVect, pi_, unitVect)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.Units.Physics (accelU, angVelU, impulseU, momtInertU,
  torqueU, velU, angAccelU)

import Control.Lens((^.))
import Data.Drasil.Constraints (gtZeroConstr)

defSymbols :: [DefinedQuantityDict]
defSymbols = map dqdWr unitSymbs ++ map dqdWr inputConstraints

unitSymbs :: [UnitalChunk]
unitSymbs = map ucw [iVect, jVect, normalVect,
  force_1, force_2, forcej, mass_1, mass_2,
  dispNorm, sqrDist, velA, velB, velO, rOB, angVelA, angVelB,
  posCM, massj, posj, accj, angAccj, mTot, velj, torquej, timeC, initRelVel,
  massA, massB, massIRigidBody, normalLen, contDispA, contDispB,
  perpLenA, momtInertA, perpLenB, momtInertB, timeT, inittime,
  momtInertK, pointOfCollision, contDispK, collisionImpulse, velAP,
  velBP, time_1, time_2, velo_1, velo_2, rRot, mLarger, distMass, dVect]

----------------------
-- TABLE OF SYMBOLS --
----------------------

symbolsAll, inputSymbols, outputSymbols :: [DefinedQuantityDict]

symbolsAll = unitless ++ map dqdWr unitalSymbols ++ [QP.restitutionCoef]

inputSymbols = map dqdWr [QP.position, QP.velocity, QP.force, QM.orientation,
  QP.angularVelocity, QP.linearVelocity, QP.gravitationalConst, QPP.mass,
  QPP.len, QP.momentOfInertia, QP.torque, QP.kEnergy, QP.chgInVelocity, QP.potEnergy, QP.fOfGravity, QP.positionVec] ++
  [QP.restitutionCoef]

outputSymbols = map dqdWr [QP.position, QP.velocity, QM.orientation,
  QP.angularVelocity, QP.chgMomentum, QP.chgInVelocity]


unitalSymbols :: [UnitalChunk]
unitalSymbols = [QP.acceleration, QP.angularAccel, QP.gravitationalAccel,
  QP.force, QP.impulseV, QP.impulseS, QP.distance, QP.displacement,
  QP.time, QP.angularDisplacement, QP.linearDisplacement, QP.linearVelocity,
  QP.linearAccel, QP.kEnergy, QP.chgInVelocity, QP.potEnergy, QP.height,
  QP.fOfGravity, QP.positionVec, QP.chgMomentum, QP.gravitationalConst,
  QP.momentOfInertia, QPP.len, QPP.mass, QP.position, QP.velocity, QP.torque,
  QP.angularVelocity, QM.orientation] ++ unitalTerms

unitalTerms :: [UnitalChunk]
unitalTerms = [iVect, jVect, normalVect, posCM, posj, massj, mTot, accj, angAccj, velj,
  initRelVel, normalLen,
  perpLenA, perpLenB, forcej, torquej, timeC, velA, velB, massA, massB,
  angVelA, angVelB, force_1, force_2, mass_1, mass_2,
  dispNorm, sqrDist, velO, rOB, massIRigidBody, contDispA, contDispB,
  momtInertA, momtInertB, timeT, inittime, momtInertK, pointOfCollision,
  contDispK, collisionImpulse, finRelVel, velAP, velBP, time_1, time_2, velo_1, velo_2,
  rRot, mLarger, distMass,
  dVect]

-----------------------
-- PARAMETRIZED HACK --
-----------------------
--FIXME: parametrized hack
--FIXME: "A" is not being capitalized when it should be.
forceParam, massParam, timeParam :: String -> String -> Symbol -> UnitalChunk
forceParam n w s = uc
 (dccWDS ("force" ++ n) (cn $ "force exerted by the " ++ w ++
  " body (on another body)") (phrase QP.force))
  (sub (eqSymb QP.force) s) Real newton

massParam n w s = uc
 (dccWDS ("mass" ++ n) (cn $ "mass of the " ++ w ++ " body")
  (phrase QPP.mass)) (sub (eqSymb QPP.mass) s) Real kilogram

timeParam n w s = uc
 (dccWDS ("time" ++ n) (cn $ "time at a point in " ++ w ++ " body ")
  (phrase QP.time)) (sub (eqSymb QP.time) s) Real second

contParam :: String -> String -> Symbol -> Symbol -> UnitalChunk
contParam n m w s = uc
 (dccWDS ("r_" ++ n ++ m) contdispN (phrase QP.displacement))
  (sub (eqSymb QP.displacement) (Concat [w, s])) Real metre
  where contdispN = cn $ "displacement vector between the centre of mass of rigid body " ++
                         n ++ " and contact point " ++ m

angParam, momtParam, perpParam, rigidParam, velBodyParam, velParam :: String -> Symbol -> UnitalChunk

angParam n w = uc
 (dccWDS ("angular velocity" ++ n) (compoundPhrase'
  (cn $ n ++ " body's") (QP.angularVelocity ^. term))
  (phrase QP.angularVelocity)) (sub (eqSymb QP.angularVelocity) w) Real angVelU

momtParam n w = uc
 (dccWDS ("momentOfInertia" ++ n) (compoundPhrase'
  (QP.momentOfInertia ^. term) (cn $ "of rigid body " ++ n))
  (phrase QP.momentOfInertia)) (sub (eqSymb QP.momentOfInertia) w) Real momtInertU

perpParam n w = uc
 (dccWDS ("|| r_A" ++ n ++ " x n ||")
  (compoundPhrase' (QPP.len `ofThe` QM.perpVect)
  (cn $ "to the contact displacement vector of rigid body " ++ n))
  (phrase QM.perpVect)) (Atop Magnitude $ Concat [w, label "*", --should be x for cross
  eqSymb QM.perpVect]) Real metre

rigidParam n w = uc
 (dccWDS ("rig_mass" ++ n) (compoundPhrase' (QPP.mass ^. term)
  (cn $ "of rigid body " ++ n)) (phrase QPP.mass)) (sub (eqSymb QPP.mass) w) Real kilogram

velBodyParam n w = uc
 (dccWDS ("velocity" ++ n) (compoundPhrase' (QP.velocity ^. term)
  (cn $ "of the  " ++ n ++ " body")) (phrase QP.velocity)) (sub (eqSymb QP.velocity) w) Real velU

velParam n w = uc
 (dccWDS ("velocity" ++ n) (compoundPhrase' (QP.velocity ^. term)
  (cn $ "at point " ++ n)) (phrase QP.velocity)) (sub (eqSymb QP.velocity) w) Real velU

-----------------------
-- CHUNKS WITH UNITS --
-----------------------

iVect, jVect, normalVect, force_1, force_2, forcej, mass_1, mass_2,
  dispNorm, sqrDist, velA, velB, velO, rOB, angVelA, angVelB,
  posCM, massj, posj, accj, angAccj, mTot, velj, torquej, timeC, initRelVel,
  massA, massB, massIRigidBody, normalLen, contDispA, contDispB,
  perpLenA, momtInertA, perpLenB, momtInertB, timeT, inittime,
  momtInertK, pointOfCollision, contDispK, collisionImpulse, finRelVel,
  velAP, velBP, time_1, time_2, velo_1, velo_2, rRot, mLarger, distMass, dVect :: UnitalChunk

iVect = uc (dccWDS "unitVectI" (compoundPhrase' (cn "horizontal")
               (QM.unitVect ^. term)) (phrase QM.unitVect))
               (eqSymb QM.unitVect) Real metre
jVect       = uc (dccWDS "unitVectJ" (compoundPhrase' (cn "vertical")
               (QM.unitVect ^. term)) (phrase QM.unitVect)) (vec $ hat lJ) Real metre
normalVect  = uc (dccWDS "normalVect" (compoundPhrase (cn "collision") (QM.normalVect ^. term))
                 (phrase QM.normalVect))
                   (eqSymb QM.normalVect) (Vect Real) metre

dVect = uc (dccWDS "unitVectD"
          (cn "unit vector directed from the center of the large mass to the center of the smaller mass")
                   (phrase QM.unitVect)) (vec (hat lD)) Real metre

dispNorm = uc (dccWDS "euclideanNormDisp" (cn "Euclidean norm of the distance between the center of mass of two bodies")
               (phrase QM.euclidNorm) ) (eqSymb QM.euclidNorm) Real metre

distMass = uc (dccWDS "distMass" (cn "distance between the center of mass of the rigid bodies")
                 (phrase QP.distance)) (vec lD) Real metre

sqrDist = uc (dccWDS "euclideanNorm" (cn' "squared distance")
               (phrase QM.euclidNorm)) (sup (eqSymb QM.euclidNorm)
               label2) Real m_2

rOB    = uc' "rOB"
  (nounPhraseSP "displacement vector between the origin and point B")
  (S "FIXME: Define this or remove the need for definitions")
  (sub (eqSymb QP.displacement) (Concat [lOrigin, lBodyB])) Real metre

posCM = uc' "p_CM" (nounPhraseSP "Center of Mass")
 --"mass-weighted average position of a rigid " ++
 -- "body's particles")
  (S "FIXME: Define this or remove the need for definitions")
  (sub (eqSymb QP.position) lCMass) Real metre

massj = uc (dccWDS "m_j" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the j-th particle")) (phrase QPP.mass))
                (sub (eqSymb QPP.mass) lJ) Real kilogram

posj = uc (dccWDS "p_j" (compoundPhrase' (QP.position ^. term)
               (cn "vector of the j-th particle")) (phrase QP.position))
               (sub (eqSymb QP.position) lJ) Real metre

accj = uc (dccWDS "accj" (compoundPhrase' (cn "j-th body's")
               (QP.acceleration ^. term)) (phrase QP.acceleration))
               (sub (eqSymb QP.acceleration) lJ) Real accelU

-- FIXME: Using the titleized version in the same style as 'accj' above does not render properly.
--        Oddly, stable breaks differently when trying to use 'nounPhraseSent' or 'combineNPNI'. See #2650.
angAccj = uc (dccWDS "angAccj" (nounPhrase'' n n CapWords CapWords) (phrase QP.angularAccel))
               (sub (eqSymb QP.angularAccel) lJ) Real angAccelU
  where
    n :: D.NPStruct
    n = D.S "j-th body's" D.:+: (phraseNP $ QP.angularAccel ^. term)

velj = uc (dccWDS "velj" (compoundPhrase' (QP.velocity ^. term)
               (cn "of the j-th body")) (phrase QP.velocity))
               (sub (eqSymb QP.velocity) lJ) Real velU

torquej = uc (dccWDS "torquej"
               (cn "torque applied to the j-th body")
               (phrase QP.torque)) (sub (eqSymb QP.torque) lJ) Real torqueU

mTot = uc (dccWDS "M_T" (compoundPhrase' (cn "total mass of the")
                 (CP.rigidBody ^. term)) (phrase QPP.mass))
                 (sub (eqSymb QPP.mass) cT) Real kilogram

mLarger = uc (dccWDS "mLarger" (compoundPhrase' (cn "mass of the larger")
                 (CP.rigidBody ^. term)) (phrase QPP.mass)) cM Real kilogram

timeC = uc (dccWDS "timeC" (cn "denotes the time at collision")
                (phrase QP.time)) (sub (eqSymb QP.time) lColl) Real second

initRelVel = uc (dccWDS "v_i^AB" (compoundPhrase'
                 (compoundPhrase' (cn "initial relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B")) (phrase QP.velocity))
                 (sup (sub (eqSymb QP.velocity) QP.initial) (Concat [lBodyA, lBodyB])) (Vect Real) velU

finRelVel = uc (dccWDS "v_f^AB" (compoundPhrase'
                 (compoundPhrase' (cn "final relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B")) (phrase QP.velocity))
                 (sup (sub (eqSymb QP.velocity) QP.final) (Concat [lBodyA, lBodyB])) (Vect Real) velU

massIRigidBody = uc (dccWDS "massj" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the j-th rigid body")) (phrase QPP.mass))
                (sub (eqSymb QPP.mass) lJ) Real kilogram
normalLen = uc (dccWDS "length of the normal vector" (
                  QPP.len `ofThe` QM.normalVect)
                  (phrase QM.normalVect))
                  (Atop Magnitude $ eqSymb QM.normalVect) Real metre

rRot = uc (dccWDS "r_j" (compoundPhrase' (QP.distance ^. term)
                (cn "between the j-th particle and the axis of rotation")) (phrase QP.distance))
                (sub (eqSymb QP.distance) lJ) Real metre

timeT = uc (dccWDS "t" (cn "point in time") (phrase QP.time))
                (eqSymb QP.time) Real second

inittime = uc (dccWDS "t_0" (cn "denotes the initial time")
                (phrase QP.time)) (sub (eqSymb QP.time) label0) Real second

pointOfCollision = uc (dccWDS "point_c" (cn "point of collision")
                 (S "point")) cP Real metre

collisionImpulse = uc (dccWDS "collisionImp" (compoundPhrase'
                (cn "collision") (QP.impulseS ^. term)) (phrase QP.impulseS))
                (eqSymb QP.impulseS) Real impulseU

forcej = uc (dccWDS "forcej" (compoundPhrase'
      (QP.force ^. term) (cn "applied to the j-th body at time t"))
      (phrase QP.force)) (sub (eqSymb QP.force) lJ) Real newton

velAP = uc (dccWDS "v^AP" (compoundPhrase' (QP.velocity ^. term)
              (cn "of the point of collision P in body A"))
              (phrase QP.velocity)) (sup (eqSymb QP.velocity)(Concat [lBodyA, lPoint]))
              (Vect Real) velU
velBP = uc (dccWDS "v^BP" (compoundPhrase' (QP.velocity ^. term)
              (cn "of the point of collision P in body B"))
              (phrase QP.velocity)) (sup (eqSymb QP.velocity)(Concat [lBodyB, lPoint]))
              (Vect Real) velU

force_1    = forceParam "1" "first"  label1
force_2    = forceParam "2" "second" label2
mass_1     = massParam  "1" "first"  label1
mass_2     = massParam  "2" "second" label2
velA       = velParam   "A" lBodyA
velB       = velParam   "B" lBodyB
velO       = velParam   "origin" lOrigin
angVelA    = angParam   "A" lBodyA
angVelB    = angParam   "B" lBodyB
perpLenA   = perpParam  "A" $ eqSymb contDispA
perpLenB   = perpParam  "B" $ eqSymb contDispB
momtInertA = momtParam  "A" lBodyA
momtInertB = momtParam  "B" lBodyB
momtInertK = momtParam  "k" lK
contDispA  = contParam  "A" "P" lBodyA lPoint
contDispB  = contParam  "B" "P" lBodyB lPoint
contDispK  = contParam  "k" "P" lK     lPoint
massA      = rigidParam "A" lBodyA
massB      = rigidParam "B" lBodyB
velo_1     = velBodyParam  "first"  label1
velo_2     = velBodyParam  "second" label2
time_1     = timeParam "1" "first"  label1
time_2     = timeParam "2" "second" label2

label0, label1, label2, lBodyA, lBodyB, lCMass, lColl, lOrigin, lPoint :: Symbol
label0  = Integ 0
label1  = Integ 1
label2  = Integ 2
lBodyA  = label "A"
lBodyB  = label "B"
lCMass  = label "CM"
lColl   = label "c"
lOrigin = label "O"
lPoint  = label "P"

--------------------------
-- CHUNKS WITHOUT UNITS --
--------------------------

unitless :: [DefinedQuantityDict]
unitless = QM.pi_ : [numParticles]

numParticles :: DefinedQuantityDict
numParticles = dqdNoUnit (dcc "n" (nounPhraseSP "number of particles in a rigid body")
  "the number of particles in a rigidbody") lN Integer

-----------------------
-- CONSTRAINT CHUNKS --
-----------------------

lengthCons, massCons, mmntOfInCons, gravAccelCons, posCons, orientCons,
  angVeloCons, forceCons, torqueCons, veloCons, restCoefCons, veloOutCons,
  angVeloOutCons, orientOutCons, posOutCons :: ConstrConcept

inputConstraints :: [UncertQ]
inputConstraints = map (`uq` defaultUncrt)
  [lengthCons, massCons, mmntOfInCons, gravAccelCons, orientCons,
  veloCons, angVeloCons, forceCons, torqueCons, restCoefCons, posCons]

outputConstraints :: [UncertQ]
outputConstraints = map (`uq` defaultUncrt)
  [posOutCons, veloOutCons, orientOutCons, angVeloOutCons]

lengthCons     = constrained' QPP.len               [gtZeroConstr] (dbl 44.2)
massCons       = constrained' QPP.mass              [gtZeroConstr] (dbl 56.2)
mmntOfInCons   = constrained' QP.momentOfInertia    [gtZeroConstr] (dbl 74.5)
gravAccelCons  = constrained' QP.gravitationalConst [] (lit $ QP.gravitationalConstValue ^. defnExpr)
posCons        = constrained' QP.position           [] (dbl 0.412) --FIXME: should be (0.412, 0.502) vector
veloCons       = constrained' QP.velocity           [] (dbl 2.51)
orientCons     = constrained' QM.orientation        [sfwrRange $ Bounded (Inc, exactDbl 0) (Inc, exactDbl 2 $* sy QM.pi_)] (half $ sy QM.pi_) -- physical constraint not needed space is radians
angVeloCons    = constrained' QP.angularVelocity    [] (dbl 2.1)
forceCons      = constrained' QP.force              [] (dbl 98.1)
torqueCons     = constrained' QP.torque             [] (exactDbl 200)
restCoefCons   = constrained' QP.restitutionCoef    [physRange $ Bounded (Inc, exactDbl 0) (Inc, exactDbl 1)] (dbl 0.8)

posOutCons        = constrained' QP.position           [] (exactDbl 0)
veloOutCons       = constrained' QP.velocity           [] (exactDbl 0)
orientOutCons     = constrained' QM.orientation        [] (exactDbl 0)
angVeloOutCons    = constrained' QP.angularVelocity    [] (exactDbl 0)

---------------------
-- INSTANCE MODELS --
---------------------

---------------------
-- GOAL STATEMENTS --
---------------------

