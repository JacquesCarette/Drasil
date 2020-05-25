module Drasil.GamePhysics.Requirements (funcReqs, nonfuncReqs) where

import Language.Drasil hiding (Vector, organization)
import Utils.Drasil

import qualified Drasil.DocLang.SRS as SRS (propCorSol, solCharSpec)
import Data.Drasil.Concepts.Documentation as Doc (assumption, body, code,
  environment, funcReqDom, input_, likelyChg, mg, mis, module_, nonFuncReqDom,
  output_, physicalConstraint, physicalSim, property, requirement, srs,
  traceyMatrix, unlikelyChg)
import Data.Drasil.IdeaDicts as Doc (dataDefn, genDefn, inModel, thModel)

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

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
funcReqs :: [ConceptInstance]
funcReqs = [simSpace, inputInitialConds, inputSurfaceProps,
  verifyPhysCons, calcTransOverTime, calcRotOverTime, deterColls, deterCollRespOverTime]

simSpaceDesc, inputInitialCondsDesc, 
  inputSurfacePropsDesc, verifyPhysConsDesc,
  calcTransOverTimeDesc, calcRotOverTimeDesc,
  deterCollsDesc, deterCollRespOverTimeDesc :: Sentence

  -- | template for requirements
requirementTemplate :: Sentence -> Sentence -> Sentence -> Sentence -> Sentence
requirementTemplate a b x z = foldlSent [S "Determine the", a `sAnd` b, 
  S "over a period of", phrase QP.time, S "of the", x, z]

  -- | with added constraint
requirementS :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence -> Sentence
requirementS a b = requirementTemplate (plural a) (plural b) (getAcc twoD
  +:+ plural CP.rigidBody)

  -- | without added constraint
requirementS' :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence
requirementS' a b = requirementS a b EmptyS 

-- some requirements look like they could be parametrized
simSpaceDesc = foldlSent [S "Create a", phrase CP.space, S "for all of the",
  plural CP.rigidBody, S "in the", phrase physicalSim, 
  S "to interact in"]

inputInitialCondsDesc = foldlSent [S "Input the initial", foldlList Comma List
  [plural QPP.mass, plural QP.velocity, plural QM.orientation,
  plural QP.angularVelocity `sOf` EmptyS, plural QP.force +:+ S "applied on"],
  plural CP.rigidBody]

inputSurfacePropsDesc = foldlSent [S "Input", (phrase CM.surface +:+
  plural property) `ofThe` plural body, S "such as", phrase CP.friction `sOr`
  phrase CP.elasticity]

verifyPhysConsDesc = foldlSent [S "Verify that the", plural input_,
  S "satisfy the required", plural physicalConstraint, S "from", 
  makeRef2S (SRS.solCharSpec [] [])]

calcTransOverTimeDesc = requirementS QP.position QP.velocity 
  (S "acted upon by a" +:+ phrase QP.force)

calcRotOverTimeDesc = requirementS' QM.orientation QP.angularVelocity

deterCollsDesc = foldlSent [S "Determine if any of the", 
  plural CP.rigidBody, S "in the", phrase CP.space, 
  S "have collided"]

deterCollRespOverTimeDesc = requirementS QP.position QP.velocity 
  (S "that have undergone a" +:+ phrase CP.collision)

simSpace, inputInitialConds, inputSurfaceProps, verifyPhysCons, calcTransOverTime,
  calcRotOverTime, deterColls, deterCollRespOverTime :: ConceptInstance

simSpace              = cic "simSpace"              simSpaceDesc              "Simulation-Space"                       funcReqDom
inputInitialConds     = cic "inputInitialConds"     inputInitialCondsDesc     "Input-Initial-Conditions"               funcReqDom
inputSurfaceProps     = cic "inputSurfaceProps"     inputSurfacePropsDesc     "Input-Surface-Properties"               funcReqDom
verifyPhysCons        = cic "verifyPhysCons"        verifyPhysConsDesc        "Verify-Physical_Constraints"            funcReqDom
calcTransOverTime     = cic "calcTransOverTime"     calcTransOverTimeDesc     "Calculate-Translation-Over-Time"        funcReqDom
calcRotOverTime       = cic "calcRotOverTime"       calcRotOverTimeDesc       "Calculate-Rotation-Over-Time"           funcReqDom
deterColls            = cic "deterColls"            deterCollsDesc            "Determine-Collisions"                   funcReqDom
deterCollRespOverTime = cic "deterCollRespOverTime" deterCollRespOverTimeDesc "Determine-Collision-Response-Over-Time" funcReqDom

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

nonfuncReqs :: [ConceptInstance] 
nonfuncReqs = [highPerformance, correct, usable, understandable, maintainable]

highPerformance :: ConceptInstance
highPerformance = cic "highPerformance" (foldlSent [
  S "The execution time for collision detection and collision resolution shall be", 
  S "comparable to an existing 2D physics library on the market (e.g. Pymunk)."
  ]) "High-Performance" nonFuncReqDom

correct :: ConceptInstance
correct = cic "correct" (foldlSent [S "The", phrase output_ `sOf` S "simulation", 
  S "results shall be compared to an existing implemenation like Pymunk."
  ]) "Correct" nonFuncReqDom
 
usable :: ConceptInstance
usable = cic "usable" (foldlSent [S "Software shall be easy to learn" `sAnd` S "use.",
  S "Usability shall be measured by how long it takes a user to learn how to use", 
  S "the library to create a small program to simulate the movement" `sOf` S "2 bodies", 
  S "over time in space. Creating a program should take no less than 30 to 60 minutes", 
  S "for an intermediate to experienced programmer. Please refer Usability NFR test" `sIn` makeRef2S (SRS.propCorSol [] []) -- wrong reference; placeholder
  S "of System VnV Plan located at", 
  S "https://github.com/smiths/caseStudies/blob/gamephy_finaldoc/CaseStudies/gamephys/docs/VnVPlan/SystVnVPlan/SystVnVPlan.pdf" 
  ]) "Usable" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [S "Users" `sOf` S "Tamias2D shall be", 
  S "able to learn the software with ease. Users shall be able to easily create", 
  S "a small program using the library. Creating a small program to simulate", 
  S "the movement of 2 bodies" `sIn` S "space should take no less that 60 minutes." 
  ]) "Understandable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The development time for any " `ofThe` S "likely changes should not exceed", 
  S "10% percent" `ofThe` S "original development time"]) "Maintainable" nonFuncReqDom
