module Drasil.GamePhysics.Requirements (functionalRequirementsList, 
  functionalRequirementsList', requirements) where

import Language.Drasil hiding (Vector, organization)

import Drasil.DocLang (mkEnumSimpleD, reqF)

import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.Concepts.Documentation as Doc(body, funcReqDom, game,
  input_, nonFuncReqDom, nonfunctionalRequirement, physicalConstraint,
  physicalSim, priority, property)
import Data.Drasil.Concepts.Software (understandability, portability,
  reliability, maintainability, performance, correctness, reliability)
import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), foldlList, 
  foldlSent, foldlSent_, foldlSentCol, foldlSP, foldlSPCol, sAnd, showingCxnBw, 
  sOf, sOr)

import qualified Data.Drasil.Concepts.Physics as CP (collision, elasticity, 
  friction, rigidBody, space)
import qualified Data.Drasil.Concepts.Math as CM (surface)
import qualified Data.Drasil.Quantities.Math as QM (orientation)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import qualified Data.Drasil.Quantities.Physics as QP (angularVelocity, force, 
  position, time, velocity)

import Drasil.GamePhysics.Concepts (twoD)

------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------

requirements :: Section
requirements = reqF [functionalRequirements, nonfunctionalRequirements]

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

functionalRequirements :: Section
functionalRequirements = SRS.funcReq functionalRequirementsList []

functionalRequirementsList :: [Contents]
functionalRequirementsList = mkEnumSimpleD
  functionalRequirementsList'

-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
functionalRequirementsList' :: [ConceptInstance]
functionalRequirementsList' = [reqSS, reqIIC, reqISP, reqVPC, reqCTOT,
  reqCROT, reqDC, reqDCROT]

functional_requirements_req1, functional_requirements_req2, 
  functional_requirements_req3, functional_requirements_req4,
  functional_requirements_req5, functional_requirements_req6,
  functional_requirements_req7, functional_requirements_req8 :: Sentence

  -- | template for requirements
requirementTemplate :: Sentence -> Sentence -> Sentence -> Sentence -> Sentence
requirementTemplate a b x z = foldlSent [S "Determine the", a `sAnd` b, 
  S "over a period of", (phrase QP.time), S "of the", x, z]

  -- | with added constraint
requirementS :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence -> Sentence
requirementS a b = requirementTemplate (plural a) (plural b) ((getAcc twoD)
  +:+ (plural CP.rigidBody))

  -- | without added constraint
requirementS' :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence
requirementS' a b = requirementS a b EmptyS 

-- some requirements look like they could be parametrized
functional_requirements_req1 = foldlSent [S "Create a", (phrase CP.space), S "for all of the",
  (plural CP.rigidBody), S "in the", (phrase physicalSim), 
  S "to interact in"]

functional_requirements_req2 = foldlSent [S "Input the initial", 
  (plural QPP.mass) `sC` (plural QP.velocity) `sC` 
  (plural QM.orientation) `sC` (plural QP.angularVelocity), 
  S "of" `sC` S "and", (plural QP.force), S "applied on", 
  (plural CP.rigidBody)]

functional_requirements_req3 = foldlSent [S "Input the", (phrase CM.surface), 
  (plural property), S "of the", plural body, S "such as",
  (phrase CP.friction) `sOr` (phrase CP.elasticity)]

functional_requirements_req4 = foldlSent [S "Verify that the", plural input_,
  S "satisfy the required", plural physicalConstraint, S "from", 
  (makeRef2S $ SRS.solCharSpec ([]::[Contents]) ([]::[Section]))]

functional_requirements_req5 = requirementS (QP.position) (QP.velocity) 
  (S "acted upon by a" +:+ (phrase QP.force))

functional_requirements_req6 = requirementS' (QM.orientation) (QP.angularVelocity)

functional_requirements_req7 = foldlSent [S "Determine if any of the", 
  (plural CP.rigidBody), S "in the", (phrase CP.space), 
  S "have collided"]

functional_requirements_req8 = requirementS (QP.position) (QP.velocity) 
  (S "that have undergone a" +:+ (phrase CP.collision))

reqSS, reqIIC, reqISP, reqVPC, reqCTOT, reqCROT, reqDC, reqDCROT :: ConceptInstance

reqSS = cic "reqSS" functional_requirements_req1 "Simulation-Space" funcReqDom
reqIIC = cic "reqIIC" functional_requirements_req2 "Input-Initial-Conditions" funcReqDom
reqISP = cic "reqISP" functional_requirements_req3 "Input-Surface-Properties" funcReqDom
reqVPC = cic "reqVPC" functional_requirements_req4 "Verify-Physical_Constraints" funcReqDom
reqCTOT = cic "reqCTOT" functional_requirements_req5 "Calculate-Translation-Over-Time" funcReqDom
reqCROT = cic "reqCROT" functional_requirements_req6 "Calculate-Rotation-Over-Time" funcReqDom
reqDC = cic "reqDC" functional_requirements_req7 "Determine-Collisions" funcReqDom
reqDCROT = cic "reqDCROT" functional_requirements_req8 "Determine-Collision-Response-Over-Time" funcReqDom

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

nonfunctionalRequirements :: Section
nonfunctionalRequirementsIntro :: Contents

nonfunctionalRequirements = SRS.nonfuncReq [nonfunctionalRequirementsIntro] []

chpmnkPriorityNFReqs :: [ConceptChunk]
chpmnkPriorityNFReqs = [correctness, understandability, portability,
  reliability, maintainability]

nonfunctionalRequirementsIntro = foldlSP 
  [(titleize' game), S "are resource intensive, so", phrase performance,
  S "is a high" +:+. phrase priority, S "Other", plural nonfunctionalRequirement,
  S "that are a", phrase priority +: S "are",
  foldlList Comma List (map phrase chpmnkPriorityNFReqs)]
