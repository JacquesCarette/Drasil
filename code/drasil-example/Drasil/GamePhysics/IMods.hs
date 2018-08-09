module Drasil.GamePhysics.IMods (iModels, iModels_new, im1_new, im2_new, im3_new) where

import Language.Drasil

import Drasil.GamePhysics.Unitals(acc_i, force_i, transMotLegTerms, rotMotLegTerms,
  col2DLegTerms, mass_A, mass_i, normalVect, time_c, torque_i, vel_A, vel_i)

import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration,
  angularAccel, force, gravitationalAccel, momentOfInertia, angularVelocity, 
  time, impulseS)
import Data.Drasil.SentenceStructures (foldlSent)
import Data.Drasil.Utils (fmtU, foldle1)

-- Labels
l1, l2, l3 :: Label
l1 = mkLabelSame "transMot" (Def Instance)
l2 = mkLabelSame "rotMot" (Def Instance)
l3 = mkLabelSame "col2D" (Def Instance)

iModels :: [RelationConcept]
iModels = [transMot, rotMot, col2D]

iModels_new :: [InstanceModel]
iModels_new = [im1_new, im2_new, im3_new]

{-- Force on the translational motion  --}
im1_new :: InstanceModel
im1_new = im' transMot [qw vel_i, qw QP.time, qw QP.gravitationalAccel, qw force_i, qw mass_i] 
  [ TCon AssumedCon $ sy vel_i $> 0, TCon AssumedCon $ sy QP.time $> 0, TCon AssumedCon $ sy QP.gravitationalAccel $> 0, 
  TCon AssumedCon $ sy force_i $> 0, TCon AssumedCon $ sy mass_i $> 0 ] (qw acc_i) [] l1
  [transMotDesc]

transMot :: RelationConcept
transMot = makeRC "transMot" transMotNP (transMotDesc +:+ transMotLeg) transMotRel l1

transMotNP :: NP
transMotNP =  nounPhraseSP "Force on the translational motion of a set of 2d rigid bodies"

transMotRel :: Relation -- FIXME: add proper equation
transMotRel = (sy acc_i) $= (deriv (apply1 vel_i QP.time) QP.time)
  $= (sy QP.gravitationalAccel) + ((apply1 force_i QP.time) / (sy mass_i))


--fixme: need referencing
transMotDesc, transMotLeg :: Sentence
transMotDesc = foldlSent [S "The above equation expresses the total",
  (phrase QP.acceleration), S "of the", (phrase CP.rigidBody),
  S "(A1, A2) i as the sum of", (phrase QP.gravitationalAccel),
  S "(GD3) and", (phrase QP.acceleration), S "due to applied",
  (phrase QP.force), S "Fi(t) (T1). The resultant outputs are",
  S "then obtained from this equation using DD2, DD3 and DD4. It is currently",
  S "assumed that there is no damping (A6) or constraints (A7) involved"]

transMotLeg = foldle1 (+:+) (+:+) $ map defList transMotLegTerms

{-- Rotational Motion --}

im2_new :: InstanceModel
im2_new = im' rotMot [qw QP.angularVelocity, qw QP.time, qw torque_i, qw QP.momentOfInertia]
  [TCon AssumedCon $ sy QP.angularVelocity $> 0, TCon AssumedCon $ sy QP.time $> 0,
  TCon AssumedCon $ sy torque_i $> 0, TCon AssumedCon $ sy QP.momentOfInertia $> 0] 
  (qw QP.angularAccel) [TCon AssumedCon $ sy QP.angularAccel $> 0] l2
  [rotMotDesc]

rotMot :: RelationConcept
rotMot = makeRC "rotMot" (rotMotNP) (rotMotDesc +:+ rotMotLeg) rotMotRel l2

rotMotNP :: NP
rotMotNP =  nounPhraseSP "Force on the rotational motion of a set of 2D rigid body"

rotMotRel :: Relation
rotMotRel = (sy QP.angularAccel) $= deriv
  (apply1 QP.angularVelocity QP.time) QP.time $= 
     ((apply1 torque_i QP.time) / (sy QP.momentOfInertia))

--fixme: need referencing
rotMotDesc, rotMotLeg :: Sentence
rotMotDesc = foldlSent [S "The above equation for the total angular acceleration",
  S "of the rigid body (A1, A2) i is derived from T5, and the resultant outputs",
  S "are then obtained from this equation using DD5, DD6 and DD7. It is",
  S "currently assumed that there is no damping (A6) or constraints (A7) involved"]

rotMotLeg = foldle1 (+:+) (+:+) $ map defList rotMotLegTerms

{-- 2D Collision --}

im3_new :: InstanceModel
im3_new = im' col2D [qw QP.time, qw QP.impulseS, qw mass_A, qw normalVect] [TCon AssumedCon $ sy QP.time $> 0,
  TCon AssumedCon $ sy QP.impulseS $> 0, TCon AssumedCon $ sy mass_A $> 0, TCon AssumedCon $ sy normalVect $> 0]
  (qw time_c) [TCon AssumedCon $ sy vel_A $> 0, TCon AssumedCon $ sy time_c $> 0] l3
  [col2DDesc]

col2D :: RelationConcept
col2D = makeRC "col2D" (col2DNP) (col2DDesc +:+ col2DLeg) col2DRel l3

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
col2DDesc = foldlSent [S "This instance model is based on our assumptions",
  S "regarding rigid body (A1, A2) collisions (A5). Again, this does not take",
  S "damping (A6) or constraints (A7) into account"]


{--S "Ik is the moment of inertia of the k-th rigid body (kg m2)",
  S "t is a point in time, t0 denotes the initial time" `sC` 
  S "and tc denotes the time at collision (s)",
  S "P is the point of collision (m)"
--}

defList :: (Quantity a) => a -> Sentence
defList thing = foldlSent [(ch thing), S "is the", (phrase thing), sParen (fmtU EmptyS thing)]

col2DLeg = foldle1 (+:+) (+:+) $ map defList col2DLegTerms
  

{--displaceVectBtw  = cvR (ddcWDS "dispBtwVect" (compoundPhrase' 
  (QP.displacement ^. term) (cn "vector between the centre of mass of the k-th
  body and point P"))) (sub (QP.displacement ^. symbol) ) 
--}
