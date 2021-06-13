{-# LANGUAGE PostfixOperators #-}
module Drasil.GamePhysics.IMods (iMods, instModIntro) where

import Language.Drasil
import Language.Drasil.ShortHands (lJ)
import Theory.Drasil (InstanceModel, imNoDerivNoRefs, qwC, ModelKinds (OthModel))
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Drasil.GamePhysics.Assumptions (assumpDI, assumpCAJI)
import Drasil.GamePhysics.Concepts (centreMass)
import Drasil.GamePhysics.DataDefs (ctrOfMassDD, linDispDD, linVelDD, linAccDD,
  angDispDD, angVelDD, angAccelDD, collisionAssump, rightHandAssump,
  rigidTwoDAssump)
import Drasil.GamePhysics.GenDefs (accelGravityGD, impulseGD)
import Drasil.GamePhysics.Goals (linearGS, angularGS)
import Drasil.GamePhysics.TMods (newtonSL, newtonSLR)
import Drasil.GamePhysics.Unitals (accj, forcej, massA, massj, normalVect,
  timeC, torquej, velA, velj)

import Data.Drasil.TheoryConcepts (inModel)

import Data.Drasil.Concepts.Documentation (condition, goal, output_)
import Data.Drasil.Concepts.Math (equation, ode)
import Data.Drasil.Concepts.Physics (rigidBody, motion)
import Data.Drasil.Quantities.Math (orientation)
import Data.Drasil.Quantities.Physics (acceleration, angularAccel, angularVelocity,
  force, gravitationalAccel, impulseS, momentOfInertia, position, time, velocity)

iMods :: [InstanceModel]
iMods = [transMot, rotMot, col2D]

{-- Force on the translational motion  --}
transMot :: InstanceModel
transMot = imNoDerivNoRefs (OthModel transMotRC) 
  [qwC velj $ UpFrom (Exc, exactDbl 0)
  ,qwC time $ UpFrom (Exc, exactDbl 0)
  ,qwC gravitationalAccel $ UpFrom (Exc, exactDbl 0)
  ,qwC forcej $ UpFrom (Exc, exactDbl 0)
  ,qwC massj $ UpFrom (Exc, exactDbl 0)
  ]
  (qw accj) [] "transMot" [transMotDesc, transMotOutputs, rigidTwoDAssump, noDampConsAssumps]

transMotRC :: RelationConcept
transMotRC = makeRC "transMotRC" transMotNP EmptyS transMotRel

transMotNP :: NP
transMotNP = nounPhraseSP "Force on the translational motion of a set of 2D rigid bodies"

transMotRel :: Relation -- FIXME: add proper equation
transMotRel = sy accj $= deriv (apply1 velj time) time
  $= sy gravitationalAccel `addRe` (apply1 forcej time $/ sy massj)

transMotDesc, transMotOutputs :: Sentence
transMotDesc = foldlSent [S "The above", phrase equation, S "expresses the total",
  phraseNP (acceleration `ofThe` rigidBody), P lJ,
  S "as the sum" `S.of_` phrase gravitationalAccel, fromSource accelGravityGD `S.and_`
  phrase acceleration, S "due to applied", phrase force, eS (apply1 forcej time) +:+.
  fromSource newtonSL, S "The resultant", plural output_ `S.are`
  S "then obtained from this", phrase equation, S "using",
  foldlList Comma List (map makeRef2S [linDispDD, linVelDD, linAccDD])]
transMotOutputs = foldlSent [atStartNP (output_ `the_ofThe` inModel),
 S "will be the functions" `S.of_` phraseNP (position `and_` velocity),
 S "over time that satisfy the", getAcc ode `S.for` phraseNP (the acceleration) `sC`
 S "with the given initial", (plural condition `S.for` phraseNP (position `and_`
 velocity) !.), atStartNP (the motion), S "is translational" `sC` S "so the",
 phraseNP (position `and_` velocity), S "functions are for the",
 phrase centreMass, fromSource ctrOfMassDD]

{-- Rotational Motion --}

rotMot :: InstanceModel
rotMot = imNoDerivNoRefs (OthModel rotMotRC) 
  [qwC angularVelocity $ UpFrom (Exc, exactDbl 0)
  ,qwC time $ UpFrom (Exc, exactDbl 0)
  ,qwC torquej $ UpFrom (Exc, exactDbl 0)
  ,qwC momentOfInertia $ UpFrom (Exc, exactDbl 0)
  ]
    (qw angularAccel) [UpFrom (Exc, exactDbl 0)] "rotMot"
  [rotMotDesc, rigidTwoDAssump, rightHandAssump]

rotMotRC :: RelationConcept
rotMotRC = makeRC "rotMotRC" rotMotNP EmptyS rotMotRel

rotMotNP :: NP
rotMotNP =  nounPhraseSP "Force on the rotational motion of a set of 2D rigid body"

rotMotRel :: Relation
rotMotRel = sy angularAccel $= deriv
  (apply1 angularVelocity time) time $= 
     (apply1 torquej time $/ sy momentOfInertia)

rotMotDesc :: Sentence
rotMotDesc = foldlSent [S "The above", phrase equation, S "for the total",
  phraseNP (angularAccel `ofThe` rigidBody), P lJ `S.is`
  S "derived from", makeRef2S newtonSLR `sC` EmptyS `S.andThe` S "resultant",
  plural output_ `S.are` S "then obtained from this", phrase equation, S "using",
  foldlList Comma List (map makeRef2S [angDispDD, angVelDD, angAccelDD])]

{-- 2D Collision --}

col2D :: InstanceModel
col2D = imNoDerivNoRefs (OthModel col2DRC)
  [qwC time $ UpFrom (Exc, exactDbl 0)
  ,qwC impulseS $ UpFrom (Exc, exactDbl 0)
  ,qwC massA $ UpFrom (Exc, exactDbl 0)
  ,qwC normalVect $ UpFrom (Exc, exactDbl 0)
  ]
  -- why a constraint on velA if velA is not an output?
  -- (qw timeC) [sy velA $> 0, sy timeC $> 0] "col2D"
  (qw timeC) [UpFrom (Exc, exactDbl 0)] "col2D"
  [col2DOutputs, rigidTwoDAssump, rightHandAssump, collisionAssump,
    noDampConsAssumps, impulseNote]

col2DRC :: RelationConcept
col2DRC = makeRC "col2DRC" col2DNP EmptyS col2DRel

col2DNP :: NP
col2DNP =  nounPhraseSP "Collisions on 2D rigid bodies"

col2DRel {-, im3Rel2, im3Rel3, im3Rel4 -} :: Relation -- FIXME: add proper equation
col2DRel = apply1 velA timeC $= apply1 velA time `addRe`
  ((sy impulseS $/ sy massA) `mulRe` sy normalVect)


col2DOutputs, impulseNote :: Sentence
col2DOutputs = foldlSent [atStartNP (output_ `the_ofThe` inModel),
  S "will be the functions" `S.of_` vals,  S "over time that satisfy the",
  plural equation, S "for the", phraseNP (velocity `and_` angularAccel) `sC`
  S "with the given initial", plural condition, S "for" +:+. vals, atStartNP (the motion),
  S "is translational" `sC` S "so the", vals, S "functions are for the",
  phrase centreMass, fromSource ctrOfMassDD]
    where vals = foldlList Comma List (map phrase [position, velocity,
                                                   orientation, angularAccel])
impulseNote = ch impulseS `S.is` definedIn'' impulseGD

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
instModIntro = foldlSent [atStartNP (the goal), makeRef2S linearGS, 
  S "is met by" +:+. (makeRef2S transMot `S.and_` makeRef2S col2D),
  atStartNP (the goal), makeRef2S angularGS, S "is met by",
  makeRef2S rotMot `S.and_` makeRef2S col2D]

{- Notes -}
noDampConsAssumps :: Sentence
noDampConsAssumps = foldlSent [S "It is currently assumed that no damping",
  S "occurs during the simulation", fromSource assumpDI `S.and_` S "that no", 
  S "constraints are involved", fromSource assumpCAJI]
