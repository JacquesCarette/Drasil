module Drasil.GamePhysics.IMods (iMods, instModIntro) where

import Language.Drasil
import Language.Drasil.ShortHands (lI)
import Theory.Drasil (InstanceModel, imNoDerivNoRefs)
import Utils.Drasil

import Drasil.GamePhysics.Assumptions (assumpOT, assumpOD, assumpAD, assumpCT, assumpDI,
  assumpCAJI)
import Drasil.GamePhysics.DataDefs (ctrOfMassDD, linDispDD, linVelDD, linAccDD,
  angDispDD, angVelDD, angAccelDD, impulseDD, rigidTwoDAssump)
import Drasil.GamePhysics.GenDefs (accelGravityGD)
import Drasil.GamePhysics.Goals (linearGS, angularGS)
import Drasil.GamePhysics.TMods (newtonSL)
import Drasil.GamePhysics.Unitals (accI, forceI, massA, massI, normalVect,
  timeC, torqueI, velA, velI)

import Data.Drasil.Concepts.Documentation (goal)
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Concepts.Physics (rigidBody)
import Data.Drasil.Quantities.Physics (acceleration, angularAccel, angularVelocity,
  force, gravitationalAccel, impulseS, momentOfInertia, time)

iMods :: [InstanceModel]
iMods = [transMot, rotMot, col2D]

{-- Force on the translational motion  --}
transMot :: InstanceModel
transMot = imNoDerivNoRefs transMotRC [qw velI, qw time, qw gravitationalAccel, qw forceI, qw massI] 
  [sy velI $> 0, sy time $> 0, sy gravitationalAccel $> 0, sy forceI $> 0, sy massI $> 0 ]
  (qw accI) [] "transMot" [transMotDesc, rigidTwoDAssump, transMotAssumps]

transMotRC :: RelationConcept
transMotRC = makeRC "transMotRC" transMotNP EmptyS transMotRel

transMotNP :: NP
transMotNP = nounPhraseSP "Force on the translational motion of a set of 2d rigid bodies"

transMotRel :: Relation -- FIXME: add proper equation
transMotRel = sy accI $= deriv (apply1 velI time) time
  $= sy gravitationalAccel + (apply1 forceI time / sy massI)

--fixme: need referencing
transMotDesc, transMotAssumps :: Sentence
transMotDesc = foldlSent [S "The above", phrase equation, S "expresses",
  (S "total" +:+ phrase acceleration) `ofThe` phrase rigidBody, P lI,
  S "as the sum" `sOf` phrase gravitationalAccel, fromSource accelGravityGD `sAnd`
  phrase acceleration, S "due to applied", phrase force, E (apply1 forceI time) +:+.
  fromSource newtonSL, S "The resultant outputs are then obtained from this",
  phrase equation, S "using" +:+. (foldlList Comma List $ map makeRef2S [linDispDD,
   linVelDD, linAccDD]), makeRef2S ctrOfMassDD]
transMotAssumps = foldlSent [S "It is currently assumed that there is no damping",
  S "occurs during the simulation", fromSource assumpDI `sAnd` S "that no", 
  S "constraints are involved", fromSource assumpCAJI]

{-- Rotational Motion --}

rotMot :: InstanceModel
rotMot = imNoDerivNoRefs rotMotRC [qw angularVelocity, qw time, qw torqueI, qw momentOfInertia]
  [sy angularVelocity $> 0, sy time $> 0, sy torqueI $> 0, sy momentOfInertia $> 0] 
    (qw angularAccel) [sy angularAccel $> 0] "rotMot"
  [rotMotDesc]

rotMotRC :: RelationConcept
rotMotRC = makeRC "rotMotRC" rotMotNP EmptyS rotMotRel

rotMotNP :: NP
rotMotNP =  nounPhraseSP "Force on the rotational motion of a set of 2D rigid body"

rotMotRel :: Relation
rotMotRel = sy angularAccel $= deriv
  (apply1 angularVelocity time) time $= 
     (apply1 torqueI time / sy momentOfInertia)

--fixme: need referencing
rotMotDesc :: Sentence
rotMotDesc = foldlSent_ [S "The above equation for the total angular acceleration",
  S "of the rigid body", makeRef2S assumpOT, makeRef2S assumpOD,
  S "i is derived from T5, and the resultant outputs",
  S "are then obtained from this equation using", makeRef2S angDispDD,
  makeRef2S angVelDD +:+. makeRef2S angAccelDD, S "It is",
  S "currently assumed that there is no damping", makeRef2S assumpDI,
  S "or constraints", makeRef2S assumpCAJI +:+. S "involved", makeRef2S assumpAD]

{-- 2D Collision --}

col2D :: InstanceModel
col2D = imNoDerivNoRefs col2DRC [qw time, qw impulseS, qw massA, qw normalVect] 
  [sy time $> 0, sy impulseS $> 0, sy massA $> 0, sy normalVect $> 0]
  (qw timeC) [sy velA $> 0, sy timeC $> 0] "col2D"
  [col2DDesc]

col2DRC :: RelationConcept
col2DRC = makeRC "col2DRC" col2DNP EmptyS col2DRel

col2DNP :: NP
col2DNP =  nounPhraseSP "Collisions on 2D rigid bodies"

col2DRel {-, im3Rel2, im3Rel3, im3Rel4 -} :: Relation -- FIXME: add proper equation
col2DRel = apply1 velA timeC $= apply1 velA time +
  (sy impulseS / sy massA) * sy normalVect

--im3Rel2 = (apply1 velB timeC) $= (apply1 velB time) -
--  ((sy impulseS) / (sy massB)) * (sy normalVect)


--fixme: these two need to use cross product and parametrized dispUnit symbol
--im3Rel3 = (apply1 angVelA timeC) $= (apply1 angVelA time) +
--  ((sy dispUnit) * ((sy impulseS) * (sy normalVect))) / (sy momentOfInertia)

--im3Rel4 = (apply1 angVelB timeC) $= (apply1 angVelB time) -
--  ((sy dispUnit) * ((sy impulseS) * (sy normalVect))) / (sy momentOfInertia)

--fixme: need referencing
col2DDesc :: Sentence
col2DDesc = foldlSent_ [S "This instance model is based on our assumptions",
  S "regarding rigid body", makeRef2S assumpOT, makeRef2S assumpOD,
  S "collisions", makeRef2S assumpCT, S "Again, this does not take",
  S "damping", makeRef2S assumpDI, S "or constraints",
  makeRef2S assumpCAJI +:+. S "into account" +:+. makeRef2S assumpAD,
  makeRef2S ctrOfMassDD, makeRef2S impulseDD]


{--S "Ik is the moment of inertia of the k-th rigid body (kg m2)",
  S "t is a point in time, t0 denotes the initial time" `sC` 
  S "and tc denotes the time at collision (s)",
  S "P is the point of collision (m)"
--}

{--displaceVectBtw  = cvR (ddcWDS "dispBtwVect" (compoundPhrase' 
  (displacement ^. term) (cn "vector between the centre of mass of the k-th
  body and point P"))) (sub (displacement ^. symbol) ) 
--}

{- Intro -}

instModIntro :: Sentence
instModIntro = foldlSent [S "The", phrase goal, makeRef2S linearGS, 
  S "is met by" +:+. (makeRef2S transMot `sAnd` makeRef2S col2D),
  S "The", phrase goal, makeRef2S angularGS, S "is met by",
  makeRef2S rotMot `sAnd` makeRef2S col2D]
