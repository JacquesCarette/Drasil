module Drasil.GamePhysics.IMods where

import Drasil.GamePhysics.Unitals

import Language.Drasil
import Data.Drasil.Utils (foldlSent)
import Data.Drasil.Concepts.Physics (rigidBody)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Prelude hiding (id)
import Control.Lens ((^.))


iModels :: [RelationConcept]
iModels = [fottmoaso2rb]


{-- Force on the translational motion  --}
fottmoaso2rb :: RelationConcept
fottmoaso2rb = makeRC "fottmoaso2rb" (fotmNP) pbdescr pb_rel 

fotmNP :: NP
fotmNP =  nounPhraseSP "Force on the translational motion of a set of 2d rigid bodies"

fotmRel :: Relation
fotmRel = (C prob_br) := 1

fotmdescr :: Sentence
fotmdescr = $ foldlSent [S "The above equation expresses the total" 
  (phrase $ acceleration ^. term), S "of the", (phrase $ rigid body ^. term), 
  S "(A1, A2) i as the sum of", (phrase $ gravAcceleration ^. term), 
  S "(GD3) and", (phrase $ acceleration ^. term), S "due to applied", 
  (phrase $ force ^. term), S "Fi(t) (T1). The resultant outputs are", 
  S "then obtained from this equation using DD2, DD3 and DD4. It is currently", 
  S "assumed that there is no damping (A6) or constraints (A7) involved."]

fotmVarList :: Sentence
fotmVarList = map (ucToSent []) [massIRigidBody, gravAccel, timeT, initTime])
  ++ (map (ucToSent (S "at time time")  [pos_i, acc_i, vel_i, force_i]

ucToSent :: UnitalChunk -> Sentence -> Sentence
ucToSent option ch = foldlSent $ [(phrase $ ch ^. term), S "is the", 
  (phrase $ ch ^. defn )] ++ option ++ [(sParen $ Sy $ unit_symb ch)]



{--  
  foldlSent [S "mi is the mass of the i-th rigid body (kg).",
  S "g is the acceleration due to gravity (ms−2).",
  S "t is a point in time and t0 denotes the initial time (s).",
  S "pi(t) is the i-th body’s position (specifically, the position of its center of mass, pCM(t) (DD1)) at time t (m).",
  S "ai(t) is the i-th body’s acceleration at time t (m).",
  S "v(t) is the i-th body’s velocity at time t (ms−1).",
  S "F(t) is the force applied to the i-th body at time t (N)."]
--}

{-- --}