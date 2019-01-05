module Drasil.GamePhysics.IMods (iModels_new, im1_new, im2_new, im3_new) where

import Language.Drasil
import Drasil.GamePhysics.Assumptions (newA1, newA2, newA4, newA5, newA6,
  newA7)
import Drasil.GamePhysics.Unitals(acc_i, force_i, transMotLegTerms, rotMotLegTerms,
  col2DLegTerms, mass_A, mass_j, normalVect, time_c, torque_i, vel_A, vel_i)
import Drasil.GamePhysics.DataDefs(ctrOfMassDD, linDispDD, linVelDD, linAccDD,
  angDispDD, angVelDD, angAccelDD, impulseDD)

import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration,
  angularAccel, force, gravitationalAccel, momentOfInertia, angularVelocity, 
  time, impulseS)
import Data.Drasil.SentenceStructures (foldlSent, foldlSent_)
import Data.Drasil.Utils (fmtU, foldle1)

iModels_new :: [InstanceModel]
iModels_new = [im1_new, im2_new, im3_new]

{-- Force on the translational motion  --}
im1_new :: InstanceModel
im1_new = eqModel transMot
  [qw vel_i, qw QP.time, qw QP.gravitationalAccel, qw force_i, qw mass_j] 
  [sy vel_i $> 0, sy QP.time $> 0, sy QP.gravitationalAccel $> 0, 
            sy force_i $> 0, sy mass_j $> 0 ] (qw acc_i) [] [] [] "transMot" [transMotDesc]

transMot :: QDefinition
transMot = fromEqn' "transMot" transMotNP (transMotDesc +:+ transMotLeg) 
  (eqSymb acc_i) transMotEqn

transMotNP :: NP
transMotNP =  nounPhraseSP "Force on the translational motion of a set of 2d rigid bodies"

-- FIXME: the 'equation' used to be a sequence of equations
transMotEqn :: Expr
transMotEqn = (deriv (apply1 vel_i QP.time) QP.time)
  -- $= (sy QP.gravitationalAccel) + ((apply1 force_i QP.time) / (sy mass_j))


-- FIXME: need referencing
transMotDesc, transMotLeg :: Sentence
transMotDesc = foldlSent [S "The above equation expresses the total",
  (phrase QP.acceleration), S "of the", (phrase CP.rigidBody),
  makeRef2S newA1, makeRef2S newA2, S "i as the sum of",
  (phrase QP.gravitationalAccel),
  S "(GD3) and", (phrase QP.acceleration), S "due to applied",
  (phrase QP.force), S "Fi(t) (T1). The resultant outputs are",
  S "then obtained from this equation using", makeRef2S linDispDD,
  makeRef2S linVelDD +:+. makeRef2S linAccDD, S" It is currently",
  S "assumed that there is no damping", makeRef2S newA6,
  S "or constraints", makeRef2S newA7 +:+. S "involved", makeRef2S ctrOfMassDD]

transMotLeg = foldle1 (+:+) (+:+) $ map defList transMotLegTerms

{-- Rotational Motion --}

im2_new :: InstanceModel
im2_new = eqModel rotMot
  [qw QP.angularVelocity, qw QP.time, qw torque_i, qw QP.momentOfInertia]
  [sy QP.angularVelocity $> 0, sy QP.time $> 0, sy torque_i $> 0, sy QP.momentOfInertia $> 0]
  (qw QP.angularAccel) [sy QP.angularAccel $> 0] [] [] "rotMot"
  [rotMotDesc]

rotMot :: QDefinition
rotMot = fromEqn' "rotMot" (rotMotNP) (rotMotDesc +:+ rotMotLeg) 
  (eqSymb QP.angularAccel) rotMotEqn

rotMotNP :: NP
rotMotNP =  nounPhraseSP "Force on the rotational motion of a set of 2D rigid body"

-- FIXME: the 'equation' used to be a sequence of equations
rotMotEqn :: Expr
rotMotEqn = deriv (apply1 QP.angularVelocity QP.time) QP.time 
     -- $= ((apply1 torque_i QP.time) / (sy QP.momentOfInertia))

--fixme: need referencing
rotMotDesc, rotMotLeg :: Sentence
rotMotDesc = foldlSent_ [S "The above equation for the total angular acceleration",
  S "of the rigid body", makeRef2S newA1, makeRef2S newA2,
  S "i is derived from T5, and the resultant outputs",
  S "are then obtained from this equation using", makeRef2S angDispDD,
  makeRef2S angVelDD +:+. makeRef2S angAccelDD, S "It is",
  S "currently assumed that there is no damping", makeRef2S newA6, 
  S "or constraints", makeRef2S newA7 +:+. S "involved", makeRef2S newA4]

rotMotLeg = foldle1 (+:+) (+:+) $ map defList rotMotLegTerms

{-- 2D Collision --}

-- FIXME: othModel because it should be a Functional Model
im3_new :: InstanceModel
im3_new = othModel col2D [qw QP.time, qw QP.impulseS, qw mass_A, qw normalVect] 
  [sy QP.time $> 0, sy QP.impulseS $> 0, sy mass_A $> 0, sy normalVect $> 0]
  (qw time_c) [sy vel_A $> 0, sy time_c $> 0] [] [] "col2D"
  [col2DDesc]

col2D :: RelationConcept
col2D = makeRC "col2D" (col2DNP) (col2DDesc +:+ col2DLeg) col2DRel

col2DNP :: NP
col2DNP =  nounPhraseSP "Collisions on 2D rigid bodies"

col2DRel {-, im3Rel2, im3Rel3, im3Rel4 -} :: Relation -- FIXME: add proper equation
col2DRel = (apply1 vel_A time_c) $= (apply1 vel_A QP.time) +
  ((sy QP.impulseS) / (sy mass_A)) * (sy normalVect)

--im3Rel2 = (apply1 vel_B time_c) $= (apply1 vel_B QP.time) -
--  ((sy QP.impulseS) / (sy mass_B)) * (sy normalVect)


--fixme: these two need to use cross product and parametrized dispUnit symbol
--im3Rel3 = (apply1 angVel_A time_c) $= (apply1 angVel_A QP.time) +
--  ((sy dispUnit) * ((sy QP.impulseS) * (sy normalVect))) / (sy QP.momentOfInertia)

--im3Rel4 = (apply1 angVel_B time_c) $= (apply1 angVel_B QP.time) -
--  ((sy dispUnit) * ((sy QP.impulseS) * (sy normalVect))) / (sy QP.momentOfInertia)

--fixme: need referencing
col2DDesc, col2DLeg :: Sentence
col2DDesc = foldlSent_ [S "This instance model is based on our assumptions",
  S "regarding rigid body", makeRef2S newA1, makeRef2S newA2,
  S "collisions", makeRef2S newA5, S "Again, this does not take",
  S "damping", makeRef2S newA6, S "or constraints",
  makeRef2S newA7 +:+. S "into account" +:+. makeRef2S newA4,
  makeRef2S ctrOfMassDD, makeRef2S impulseDD]


{--S "Ik is the moment of inertia of the k-th rigid body (kg m2)",
  S "t is a point in time, t0 denotes the initial time" `sC` 
  S "and tc denotes the time at collision (s)",
  S "P is the point of collision (m)"
--}

defList :: (Quantity a, MayHaveUnit a) => a -> Sentence
defList thing = foldlSent [(ch thing), S "is the", (phrase thing), sParen (fmtU EmptyS thing)]

col2DLeg = foldle1 (+:+) (+:+) $ map defList col2DLegTerms
  

{--displaceVectBtw  = cvR (ddcWDS "dispBtwVect" (compoundPhrase' 
  (QP.displacement ^. term) (cn "vector between the centre of mass of the k-th
  body and point P"))) (sub (QP.displacement ^. symbol) ) 
--}
