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
nonfuncReqs = [highPerformance, correct, understandable, portable, reliable, reusable, maintainable]

highPerformance :: ConceptInstance
highPerformance = cic "highPerformance" (foldlSent [
  S "The", phrase code, S "has a short reponse time when performing computation"
  ]) "High-Performance" nonFuncReqDom

correct :: ConceptInstance
correct = cic "correct" (foldlSent [plural output_ `ofThe'` phrase code,
  S "have the", plural property, S "described" `sIn` makeRef2S (SRS.propCorSol [] [])
  ]) "Correct" nonFuncReqDom
 
understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  S "The", phrase code, S "is modularized with complete",
  phrase mg `sAnd` phrase mis]) "Understandable" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  S "The", phrase code, S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom

reliable :: ConceptInstance
reliable = cic "reliable" (foldlSent [
  S "The", phrase code, S "gives consistent", plural output_]) "Reliable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [
  S "The", phrase code, S "is modularized"]) "Reusable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The traceability between", foldlList Comma List [plural requirement,
  plural assumption, plural thModel, plural genDefn, plural dataDefn, plural inModel,
  plural likelyChg, plural unlikelyChg, plural module_], S "is completely recorded in",
  plural traceyMatrix, S "in the", getAcc srs `sAnd` phrase mg]) "Maintainable" nonFuncReqDom
