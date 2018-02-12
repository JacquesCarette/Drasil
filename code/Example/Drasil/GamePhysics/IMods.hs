module Drasil.GamePhysics.IMods (iModels) where

import Language.Drasil
import Data.Drasil.Utils (foldle1, fmtU, getES)
import Data.Drasil.SentenceStructures (foldlSent)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration,
  angularAccel, force, gravitationalAccel, momentOfInertia, angularVelocity, 
  time, impulseS)
import Drasil.GamePhysics.Unitals

iModels :: [RelationConcept]
iModels = [im1, im2, im3]

{-- Force on the translational motion  --}
im1 :: RelationConcept
im1 = makeRC "im1" (im1NP) (im1descr +:+ im1leg) im1Rel 

im1NP :: NP
im1NP =  nounPhraseSP "Force on the translational motion of a set of 2d rigid bodies"

im1Rel :: Relation -- FIXME: add proper equation
im1Rel = (C acc_i) $= (Deriv Total (FCall (C vel_i) [C QP.time]) QP.time)
  $= (C QP.gravitationalAccel) + ((FCall (C force_i) [C QP.time]) / (C mass_i))


--fixme: need referencing
im1descr, im1leg :: Sentence
im1descr = foldlSent [S "The above equation expresses the total", 
  (phrase QP.acceleration), S "of the", (phrase CP.rigidBody), 
  S "(A1, A2) i as the sum of", (phrase QP.gravitationalAccel), 
  S "(GD3) and", (phrase QP.acceleration), S "due to applied", 
  (phrase QP.force), S "Fi(t) (T1). The resultant outputs are", 
  S "then obtained from this equation using DD2, DD3 and DD4. It is currently", 
  S "assumed that there is no damping (A6) or constraints (A7) involved"]

im1leg = foldle1 (+:+) (+:+) $ map defList im1legTerms

{-- --}

im2 :: RelationConcept
im2 = makeRC "im2" (im2NP) (im2descr +:+ im2leg) im2Rel 

im2NP :: NP
im2NP =  nounPhraseSP "Force on the rotational motion of a set of 2D rigid body"

im2Rel :: Relation
im2Rel = (C QP.angularAccel) $= Deriv Total
  (FCall (C QP.angularVelocity) [C QP.time]) QP.time $= 
     ((FCall (C torque_i) [C QP.time]) / (C QP.momentOfInertia))

--fixme: need referencing
im2descr, im2leg :: Sentence
im2descr = foldlSent [S "The above equation for the total angular acceleration", 
  S "of the rigid body (A1, A2) i is derived from T5, and the resultant outputs",
  S "are then obtained from this equation using DD5, DD6 and DD7. It is",
  S "currently assumed that there is no damping (A6) or constraints (A7) involved"]

im2leg = foldle1 (+:+) (+:+) $ map defList im2legTerms  

{-- --}

im3 :: RelationConcept
im3 = makeRC "im3" (im3NP) (im3descr +:+ im3leg) im3Rel1

im3NP :: NP
im3NP =  nounPhraseSP "Collisions on 2D rigid bodies"

im3Rel1 {-, im3Rel2, im3Rel3, im3Rel4 -} :: Relation -- FIXME: add proper equation
im3Rel1 = (FCall (C vel_A) [C time_c]) $= (FCall (C vel_A) [C QP.time]) +
  ((C QP.impulseS) / (C mass_A)) * (C normalVect)

--im3Rel2 = (FCall (C vel_B) [C time_c]) $= (FCall (C vel_B) [C QP.time]) -
--  ((C QP.impulseS) / (C mass_B)) * (C normalVect)


--fixme: these two need to use cross product and parametrized dispUnit symbol
--im3Rel3 = (FCall (C angVel_A) [C time_c]) $= (FCall (C angVel_A) [C QP.time]) +
--  ((C dispUnit) * ((C QP.impulseS) * (C normalVect))) / (C QP.momentOfInertia)

--im3Rel4 = (FCall (C angVel_B) [C time_c]) $= (FCall (C angVel_B) [C QP.time]) -
--  ((C dispUnit) * ((C QP.impulseS) * (C normalVect))) / (C QP.momentOfInertia)

--fixme: need referencing
im3descr, im3leg :: Sentence
im3descr = foldlSent [S "This instance model is based on our assumptions",
  S "regarding rigid body (A1, A2) collisions (A5). Again, this does not take",
  S "damping (A6) or constraints (A7) into account"]


{--S "Ik is the moment of inertia of the k-th rigid body (kg m2)",
  S "t is a point in time, t0 denotes the initial time" `sC` 
  S "and tc denotes the time at collision (s)",
  S "P is the point of collision (m)"
--}

defList :: (Quantity a) => a -> Sentence
defList thing = foldlSent [(getES thing), S "is the", (phrase thing), sParen (fmtU EmptyS thing)]

im3leg = foldle1 (+:+) (+:+) $ map defList im3legTerms
  

{--displaceVectBtw  = cvR (ddcWDS "dispBtwVect" (compoundPhrase' 
  (QP.displacement ^. term) (cn "vector between the centre of mass of the k-th
  body and point P"))) (sub (QP.displacement ^. symbol) ) 
--}
