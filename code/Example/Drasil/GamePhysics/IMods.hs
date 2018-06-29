module Drasil.GamePhysics.IMods (iModels) where

import Drasil.GamePhysics.Unitals(acc_i, force_i, transMotLegTerms, rotMotLegTerms,
  col2DLegTerms, mass_A, mass_i, normalVect, time_c, torque_i, vel_A, vel_i)

import Language.Drasil
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration,
  angularAccel, force, gravitationalAccel, momentOfInertia, angularVelocity, 
  time, impulseS)
import Data.Drasil.SentenceStructures (foldlSent)
import Data.Drasil.Utils (fmtU, foldle1, getES)

iModels :: [RelationConcept]
iModels = [transMot, rotMot, col2D]

{-- Force on the translational motion  --}
transMot :: RelationConcept
transMot = makeRC "transMot" (transMotNP) (transMotDesc +:+ transMotLeg) transMotRel

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

rotMot :: RelationConcept
rotMot = makeRC "rotMot" (rotMotNP) (rotMotDesc +:+ rotMotLeg) rotMotRel

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
col2DDesc = foldlSent [S "This instance model is based on our assumptions",
  S "regarding rigid body (A1, A2) collisions (A5). Again, this does not take",
  S "damping (A6) or constraints (A7) into account"]


{--S "Ik is the moment of inertia of the k-th rigid body (kg m2)",
  S "t is a point in time, t0 denotes the initial time" `sC` 
  S "and tc denotes the time at collision (s)",
  S "P is the point of collision (m)"
--}

defList :: (Quantity a) => a -> Sentence
defList thing = foldlSent [(getES thing), S "is the", (phrase thing), sParen (fmtU EmptyS thing)]

col2DLeg = foldle1 (+:+) (+:+) $ map defList col2DLegTerms
  

{--displaceVectBtw  = cvR (ddcWDS "dispBtwVect" (compoundPhrase' 
  (QP.displacement ^. term) (cn "vector between the centre of mass of the k-th
  body and point P"))) (sub (QP.displacement ^. symbol) ) 
--}
