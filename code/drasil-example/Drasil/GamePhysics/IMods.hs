module Drasil.GamePhysics.IMods (iMods, instModIntro) where

import Language.Drasil
import Language.Drasil.ShortHands (lI)
import Theory.Drasil (InstanceModel, imNoDerivNoRefs, qwC)
import Utils.Drasil

import Drasil.GamePhysics.Assumptions (assumpDI, assumpCAJI)
import Drasil.GamePhysics.Concepts (centreMass)
import Drasil.GamePhysics.DataDefs (ctrOfMassDD, linDispDD, linVelDD, linAccDD,
  angDispDD, angVelDD, angAccelDD, collisionAssump, rightHandAssump,
  rigidTwoDAssump)
import Drasil.GamePhysics.GenDefs (accelGravityGD, impulseGD)
import Drasil.GamePhysics.Goals (linearGS, angularGS)
import Drasil.GamePhysics.TMods (newtonSL, newtonSLR)
import Drasil.GamePhysics.Unitals (accI, forceI, massA, massI, normalVect,
  timeC, torqueI, velA, velI)

import Data.Drasil.IdeaDicts (inModel)

import Data.Drasil.Concepts.Documentation (condition, goal, output_)
import Data.Drasil.Concepts.Math (equation, ode)
import Data.Drasil.Concepts.Physics (rigidBody)
import Data.Drasil.Quantities.Math (orientation)
import Data.Drasil.Quantities.Physics (acceleration, angularAccel, angularVelocity,
  force, gravitationalAccel, impulseS, momentOfInertia, position, time, velocity)

iMods :: [InstanceModel]
iMods = [transMot, rotMot, col2D]

{-- Force on the translational motion  --}
transMot :: InstanceModel
transMot = imNoDerivNoRefs transMotRC 
  [qwC velI $ UpFrom (Exc, 0)
  ,qwC time $ UpFrom (Exc, 0)
  ,qwC gravitationalAccel $ UpFrom (Exc, 0)
  ,qwC forceI $ UpFrom (Exc, 0)
  ,qwC massI $ UpFrom (Exc, 0)
  ]
  (qw accI) [] "transMot" [transMotDesc, transMotOutputs, rigidTwoDAssump, noDampConsAssumps]

transMotRC :: RelationConcept
transMotRC = makeRC "transMotRC" transMotNP EmptyS transMotRel

transMotNP :: NP
transMotNP = nounPhraseSP "Force on the translational motion of a set of 2D rigid bodies"

transMotRel :: Relation -- FIXME: add proper equation
transMotRel = sy accI $= deriv (apply1 velI time) time
  $= sy gravitationalAccel + (apply1 forceI time / sy massI)

transMotDesc, transMotOutputs :: Sentence
transMotDesc = foldlSent [S "The above", phrase equation, S "expresses",
  (S "total" +:+ phrase acceleration) `ofThe` phrase rigidBody, P lI,
  S "as the sum" `sOf` phrase gravitationalAccel, fromSource accelGravityGD `sAnd`
  phrase acceleration, S "due to applied", phrase force, E (apply1 forceI time) +:+.
  fromSource newtonSL, S "The resultant", plural output_ `sAre`
  S "then obtained from this", phrase equation, S "using",
  foldlList Comma List (map makeRef2S [linDispDD, linVelDD, linAccDD])]
transMotOutputs = foldlSent [phrase output_ `ofThe'` phrase inModel,
 S "will be the functions" `sOf` phrase position `sAnd` phrase velocity,
 S "over time that satisfy the", getAcc ode, S "for the", phrase acceleration `sC`
 S "with the given initial", plural condition, S "for" +:+. (phrase position `sAnd`
 phrase velocity), S "The motion is translational" `sC` S "so the",
 phrase position `sAnd` phrase velocity, S "functions are for the",
 phrase centreMass, fromSource ctrOfMassDD]

{-- Rotational Motion --}

rotMot :: InstanceModel
rotMot = imNoDerivNoRefs rotMotRC 
  [qwC angularVelocity $ UpFrom (Exc, 0)
  ,qwC time $ UpFrom (Exc, 0)
  ,qwC torqueI $ UpFrom (Exc, 0)
  ,qwC momentOfInertia $ UpFrom (Exc, 0)
  ]
    (qw angularAccel) [UpFrom (Exc, 0)] "rotMot"
  [rotMotDesc, rigidTwoDAssump, rightHandAssump]

rotMotRC :: RelationConcept
rotMotRC = makeRC "rotMotRC" rotMotNP EmptyS rotMotRel

rotMotNP :: NP
rotMotNP =  nounPhraseSP "Force on the rotational motion of a set of 2D rigid body"

rotMotRel :: Relation
rotMotRel = sy angularAccel $= deriv
  (apply1 angularVelocity time) time $= 
     (apply1 torqueI time / sy momentOfInertia)

rotMotDesc :: Sentence
rotMotDesc = foldlSent [S "The above", phrase equation, S "for",
  (S "total" +:+ phrase angularAccel) `ofThe` phrase rigidBody, P lI `sIs`
  S "derived from", makeRef2S newtonSLR `sC` EmptyS `andThe` S "resultant",
  plural output_ `sAre` S "then obtained from this", phrase equation, S "using",
  foldlList Comma List (map makeRef2S [angDispDD, angVelDD, angAccelDD])]

{-- 2D Collision --}

col2D :: InstanceModel
col2D = imNoDerivNoRefs col2DRC
  [qwC time $ UpFrom (Exc, 0)
  ,qwC impulseS $ UpFrom (Exc, 0)
  ,qwC massA $ UpFrom (Exc, 0)
  ,qwC normalVect $ UpFrom (Exc, 0)
  ]
  -- why a constraint on velA if velA is not an output?
  -- (qw timeC) [sy velA $> 0, sy timeC $> 0] "col2D"
  (qw timeC) [UpFrom (Exc, 0)] "col2D"
  [col2DOutputs, rigidTwoDAssump, rightHandAssump, collisionAssump,
    noDampConsAssumps, impulseNote]

col2DRC :: RelationConcept
col2DRC = makeRC "col2DRC" col2DNP EmptyS col2DRel

col2DNP :: NP
col2DNP =  nounPhraseSP "Collisions on 2D rigid bodies"

col2DRel {-, im3Rel2, im3Rel3, im3Rel4 -} :: Relation -- FIXME: add proper equation
col2DRel = apply1 velA timeC $= apply1 velA time +
  (sy impulseS / sy massA) * sy normalVect


col2DOutputs, impulseNote :: Sentence
col2DOutputs = foldlSent [phrase output_ `ofThe'` phrase inModel,
  S "will be the functions" `sOf` vals,  S "over time that satisfy the",
  plural equation, S "for the", phrase velocity `sAnd` phrase angularAccel `sC`
  S "with the given initial", plural condition, S "for" +:+. vals, S
  "The motion is translational" `sC` S "so the", vals, S "functions are for the",
  phrase centreMass, fromSource ctrOfMassDD]
    where vals = foldlList Comma List (map phrase [position, velocity,
                                                   orientation, angularAccel])
impulseNote = ch impulseS `sIs` definedIn'' impulseGD

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

{- Notes -}
noDampConsAssumps :: Sentence
noDampConsAssumps = foldlSent [S "It is currently assumed that no damping",
  S "occurs during the simulation", fromSource assumpDI `sAnd` S "that no", 
  S "constraints are involved", fromSource assumpCAJI]
