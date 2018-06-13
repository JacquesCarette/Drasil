module Drasil.GamePhysics.Body where

import Control.Lens ((^.))

import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation (assumption, body, 
  concept, condition, consumer, dataConst, dataDefn, datumConstraint,
  document, endUser, game, genDefn, generalSystemDescription, goalStmt,
  inModel, information, input_, item, library, likelyChg, model, 
  nonfunctionalRequirement, object, organization, physical, physicalConstraint,
  physicalProperty, physicalSim, physics, priority, problemDescription, product_,
  project, property, quantity, realtime, reference, requirement, section_, 
  simulation, solutionCharSpec, srs, systemConstraint, task, template, termAndDef, 
  thModel, traceyMatrix, userCharacteristic)
import Data.Drasil.Concepts.Education (highSchoolCalculus, frstYr,
  highSchoolPhysics)
import Data.Drasil.Concepts.Software (physLib, understandability, portability,
  reliability, maintainability, performance, correctness)

import Data.Drasil.People (alex, luthfi)
import Data.Drasil.Phrase(for')
import Data.Drasil.SI_Units(metre, kilogram, second, newton, radian)

import Drasil.DocumentLanguage (DocDesc, TConvention(..), TSIntro(..), 
  TSIntro(..), Emphasis(..), DocSection(..), IntroSub(..), mkDoc, RefSec(..),
  tsymb, RefTab(..), IntroSec(..), IntroSub(..))
import Drasil.GamePhysics.Concepts (chipmunk, cpAcronyms, twoD)
import Drasil.GamePhysics.DataDefs (cpDDefs, cpQDefs)
import Drasil.GamePhysics.IMods (iModels)
import Drasil.GamePhysics.References (cpCitations)
import Drasil.GamePhysics.TMods (cpTMods)
import Drasil.GamePhysics.Unitals (cpSymbolsAll, cpOutputConstraints,
  inputSymbols, outputSymbols, cpInputConstraints)

import Drasil.Sections.AuxiliaryConstants (valsOfAuxConstantsF)
import Drasil.Sections.Requirements (reqF)
import Drasil.Sections.SolutionCharacterSpec (SubSec, siUQI, siSent, siDDef, 
  sSubSec, siIMod, siUQO, siCon, siTMod, assembler, siSTitl)
import Drasil.Sections.SpecificSystemDescription (specSysDescr)
import Drasil.Sections.TraceabilityMandGs (traceMGF)

import Language.Drasil hiding (organization)

import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (ctrOfMass, 
  dimension)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody, elasticity, 
  cartesian, friction, rightHand, collision, space, joint, damping)
import qualified Data.Drasil.Concepts.Math as CM (equation, surface, ode, 
  constraint, law)

import qualified Data.Drasil.Quantities.Math as QM (orientation)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import qualified Data.Drasil.Quantities.Physics as QP (time, 
  position, force, velocity, angularVelocity, linearVelocity)
import Data.Drasil.SentenceStructures (foldlSent, foldlSent_, foldlList, sOf,
  sAnd, sOr, maybeChanged, maybeExpanded, foldlSentCol, foldlSP, showingCxnBw)
import Data.Drasil.Software.Products (videoGame, openSource, sciCompS)
import Data.Drasil.Utils (makeTMatrix, itemRefToSent, refFromType,
  makeListRef, enumSimple, enumBullet)

import qualified Drasil.SRS as SRS
import qualified Drasil.Sections.ReferenceMaterial as RM

import Drasil.GamePhysics.Unitals (cpSymbolsAll, cpOutputConstraints,
  inputSymbols, outputSymbols, cpInputConstraints)
import Drasil.GamePhysics.Concepts (chipmunk, cpAcronyms, twoD)
import Drasil.GamePhysics.TMods (cpTMods, t1NewtonSL_new, t2NewtonTL_new, 
            t3NewtonLUG_new, t4ChaslesThm_new, t5NewtonSLR_new)
import Drasil.GamePhysics.IMods (iModels, im1_new, im2_new, im3_new)
import Drasil.GamePhysics.DataDefs (cpDDefs, cpQDefs)

import Drasil.DocumentLanguage 
import Drasil.Sections.SpecificSystemDescription (specSysDescr)
import Drasil.Sections.SolutionCharacterSpec
import Drasil.Sections.Requirements (reqF)
import Drasil.Sections.AuxiliaryConstants (valsOfAuxConstantsF)
import Drasil.GamePhysics.References (cpCitations)
import Drasil.DocumentLanguage
import Drasil.DocumentLanguage.Definitions

authors :: People
authors = [alex, luthfi]

auths :: Sentence
auths = S $ manyNames authors

chipmunkSRS' :: Document
chipmunkSRS' = mkDoc mkSRS for' chipmunkSysInfo

mkSRS :: DocDesc 
mkSRS = RefSec (RefProg RM.intro [TUnits, tsymb tableOfSymbols, TAandA]) :
  IntroSec (
    IntroProg para1_introduction_intro (short chipmunk) 
  [IPurpose (para1_purpose_of_document_intro),
   IScope scope_of_requirements_intro_p1 scope_of_requirements_intro_p2, 
   IChar (S "rigid body dynamics") (phrase highSchoolCalculus) (EmptyS), 
   IOrgSec organization_of_documents_intro inModel solution_characteristics_specification EmptyS]) :
   Verbatim general_syatem_description :
   SSDSec 
    (SSDProg [SSDSubVerb problem_description
      , SSDSolChSpec 
        (SCSProg 
          [ Assumptions
		  ,  TMs ([Label] ++ stdFields) [t1NewtonSL_new, t2NewtonTL_new, 
            t3NewtonLUG_new, t4ChaslesThm_new, t5NewtonSLR_new]
          , IMs ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields)
           [im1_new, im2_new, im3_new] ShowDerivation
          
          ]
        )
      ]
    ):
  (map Verbatim [requirements, likely_changes, off_the_shelf_solutions, traceability_matrices_and_graph, values_of_auxiliary_constatnts]) ++ 
  (Bibliography : [])
    where tableOfSymbols = [TSPurpose, TypogConvention[Vector Bold], SymbOrder]
	
stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Source, RefBy]

    --FIXME: Need to be able to print defn for gravitational constant.

chipmunkSysInfo :: SystemInformation
chipmunkSysInfo = SI {
  _sys = chipmunk,
  _kind = srs,
  _authors = authors,
  _units = chipUnits,
  _quants = cpSymbolsAll, 
  _concepts = ([] :: [DefinedQuantityDict]),
  _definitions = (cpDDefs), 
  _inputs = (inputSymbols), 
  _outputs = (outputSymbols), 
  _defSequence = (cpQDefs), 
  _constraints = cpInputConstraints,
  _constants = [],
  _sysinfodb = everything,
  _refdb = cpRefDB
}

cpRefDB :: ReferenceDB
cpRefDB = rdb [] [] newAssumptions [] [] cpCitations -- FIXME: Convert the rest to new chunk types

newAssumptions :: [AssumpChunk]
newAssumptions = [newA1, newA2, newA3, newA4, newA5, newA6, newA7]

newA1, newA2, newA3, newA4, newA5, newA6, newA7 :: AssumpChunk
newA1 = assump "objectTyA" (foldlSent assumptions_assum1) "objectTy"
newA2 = assump "objectDimensionA" (foldlSent assumptions_assum2) "objectDimension"
newA3 = assump "coordinatesystemTyA" (foldlSent assumptions_assum3) "coordinatesystemTy"
newA4 = assump "axesDefinedA" (foldlSent assumptions_assum4) "axesDefined"
newA5 = assump "collisionTypeA" (foldlSent assumptions_assum5) "collisionType"
newA6 = assump "dampingInvolvementA" (foldlSent assumptions_assum6) "dampingInvolvement"
newA7 = assump "constraints_and_jointsInvolvementA" (foldlSent assumptions_assum7) "constraints_and_jointsInvolvement"
--FIXME: All named ideas, not just acronyms.

chipUnits :: [UnitDefn]
chipUnits = map unitWrapper [metre, kilogram, second] ++ map unitWrapper [newton, radian]

everything :: ChunkDB
everything = cdb cpSymbolsAll (map nw cpSymbolsAll ++ map nw cpAcronyms) ([] :: [ConceptChunk]) -- FIXME: Fill in Concepts
  chipUnits

chipCode :: CodeSpec
chipCode = codeSpec chipmunkSysInfo []


--FIXME: The SRS has been partly switched over to the new docLang, so some of
-- the sections below are now redundant. I have not removed them yet, because
-- it makes it easier to test between the two different versions as changes
-- are introduced. Once the SRS is entirely switched to docLang, the redundant
-- sections should be removed.

-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --


------------------------------
-- Section : INTRODUCTION --
------------------------------

para1_introduction_intro :: Sentence
para1_introduction_intro = foldlSent
  [S "Due to the rising cost of developing", (plural videoGame) `sC` 
  S "developers are looking for ways to save time and money for their" +:+.
  (plural project), S "Using an", (phrase openSource), 
  (phrase physLib),
  S "that is reliable and free will cut down development costs and lead",
  S "to better quality", (plural product_)]

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------

detailsAndGoal :: [CI]
detailsAndGoal = [thModel, goalStmt]

para1_purpose_of_document_intro :: Sentence
para1_purpose_of_document_intro = para1_purpose_of_document_param chipmunk document programDescription 
  (plural game) (map plural detailsAndGoal)

programDescription :: Sentence
programDescription = foldlSent_ [(phrase openSource), getAcc twoD, 
  (phrase CP.rigidBody), (phrase physLib)]

para1_purpose_of_document_param :: (Idea a, NamedIdea b) => a -> b -> Sentence -> Sentence ->
  [Sentence] -> Sentence
para1_purpose_of_document_param progName typeOf progDescrip appOf listOf = foldlSent 
  [S "This", (phrase typeOf), S "descibes the modeling of an",
  progDescrip, S "used for" +:+. appOf, S "The", 
  foldlList listOf, S "used in", (short progName), 
  S "are provided. This", (phrase typeOf), 
  S "is intended to be used as a", (phrase reference), 
  S "to provide all necessary", (phrase information), 
  S "to understand and verify the", (phrase model)]

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------
scope_of_requirements_intro_p1, scope_of_requirements_intro_p2 :: Sentence

scope_of_requirements_intro_p1 = foldlSent_
  [S "the", (phrase physicalSim) `sOf` (getAcc twoD), 
  (plural CP.rigidBody), S "acted on by", plural QP.force]
  
scope_of_requirements_intro_p2 = foldlSent_ [S "simulate how these", 
  (plural CP.rigidBody), S "interact with one another"]

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

-------------------------------------
-- 2.3 : Organization of Documents --
-------------------------------------

organization_of_documents_intro :: Sentence

organization_of_documents_intro = foldlSent 
  [S "The", (phrase organization), S "of this", (phrase document), 
  S "follows the", phrase template, S "for an", (getAcc srs), S "for", 
  (phrase sciCompS), S "proposed by", (sSqBrNum 1) `sAnd` (sSqBrNum 2)]

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

general_syatem_description :: Section
general_syatem_description = assembler chipmunk everything generalSystemDescriptionSect
  [userCharacteristicSect, systemConstraintSect]

generalSystemDescriptionSect :: SubSec
generalSystemDescriptionSect = sSubSec generalSystemDescription []

--------------------------------
-- 3.1 : User Characteristics --
--------------------------------

userCharacteristicSect :: SubSec
userCharacteristicSect = sSubSec userCharacteristic [(siCon [user_characteristics_intro])]

user_characteristics_intro :: Contents
user_characteristics_intro = foldlSP
  [S "The", phrase endUser `sOf` short chipmunk,
  S "should have an understanding of", phrase frstYr, S "programming",
  plural concept `sAnd` S "an understanding of", phrase highSchoolPhysics]

-------------------------------
-- 3.2 : System Constraints  --
-------------------------------

systemConstraintSect :: SubSec
systemConstraintSect = sSubSec systemConstraint []

---------------------------------------------
-- SECTION 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

-- NOTE: Section 4 remains incomplete. General definitions and instance models
-- have not been encoded.

specific_system_description :: Section
specific_system_description = specSysDescr physLib [problem_description, solution_characteristics_specification]

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

problem_description :: Section
problem_description_intro :: Sentence

problem_description = assembler chipmunk everything problemDescriptionSect [termAndDefSect, 
  goalStatementSect]

problemDescriptionSect :: SubSec
problemDescriptionSect = sSubSec problemDescription [(siSent [problem_description_intro])]

problem_description_intro = problem_description_intro_param physLib game

problem_description_intro_param :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence
problem_description_intro_param lib app = foldlSent 
  [S "Creating a gaming", (phrase lib), S "is a difficult" +:+. phrase task,
  (titleize' app), S "need",  (plural lib), S "that simulate", plural object,
  S "acting under various", (phrase physical), plural condition `sC` S "while", 
  S "simultaneously being fast and efficient enough to work in soft",
  (phrase realtime), S "during the" +:+. (phrase app), S "Developing a", 
  (phrase lib), S "from scratch takes a long period of", (phrase QP.time) `sAnd`
  S "is very costly" `sC` S "presenting barriers of entry which make it difficult for",
  (phrase app), S "developers to include", (phrase physics), S "in their" +:+. 
  (plural product_), S "There are a few free" `sC` (phrase openSource) `sAnd` S "high quality",
  (plural lib), S "available to be used for", phrase consumer, plural product_ +:+. 
  (sParen $ makeRef off_the_shelf_solutions), S "By creating a simple, lightweight, fast and portable",
  (getAcc twoD), (phrase CP.rigidBody), (phrase lib) `sC` (phrase app),
  S "development will be more accessible to the masses" `sAnd` S "higher quality",
  (plural product_), S "will be produced"]


-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

terminology_and_definitions_bullets :: Contents

termAndDefSect :: SubSec
termAndDefSect = sSubSec termAndDef [(siSTitl), (siCon [terminology_and_definitions_bullets])]

terminology_and_definitions_terms :: [ConceptChunk]
terminology_and_definitions_terms = [CP.rigidBody, CP.elasticity, CPP.ctrOfMass, 
  CP.cartesian, CP.rightHand]

terminology_and_definitions_bullets = enumBullet 
  (map (\x -> (at_start x) +: EmptyS +:+ (x ^. defn)) terminology_and_definitions_terms)


-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

goal_statements_list :: Contents

goalStatementSect :: SubSec
goalStatementSect = sSubSec goalStmt [(siCon [goal_statements_list])]

goalStatementStruct :: (NamedIdea a, NamedIdea b) => Sentence -> [a] -> 
  Sentence -> Sentence -> [a] -> b -> Sentence -> Sentence -> [Sentence]
goalStatementStruct state inputs wrt adjective outputs objct condition1 condition2 = 
  [S "Given the", initial state, (listOfInputs wrt), adjective, S "a set of", 
  (plural objct) `sC` S "determine", condition1, listOfOutputs, 
  S "over a period of", (phrase QP.time), condition2]
  where initial EmptyS      = S "initial"
        initial p           = p `sC` (S "initial")
        listOfInputs EmptyS = (foldlList $ map plural inputs)
        listOfInputs i      = (foldlList $ map plural inputs) `sC` S "and" +:+ i
        listOfOutputs       = (foldlList $ map plural outputs)

goal_statements_G_linear = goalStatementStruct (plural physicalProperty) 
  (take 2 inputSymbols) (plural QP.force) (S "applied on")
  (take 2 outputSymbols) CP.rigidBody 
  (S "their new") EmptyS

goal_statements_G_angular = goalStatementStruct (plural physicalProperty) 
  (drop 3 $ take 5 inputSymbols) (plural QP.force) (S "applied on")
  (drop 3 $ take 5 inputSymbols) CP.rigidBody 
  (S "their new") EmptyS

goal_statements_G_detectCollision = goalStatementStruct EmptyS
  (take 2 inputSymbols) EmptyS (S "of")
  (take 0 inputSymbols) CP.rigidBody
  (S "if any of them will collide with one another") EmptyS

goalStatement4Inputs :: [UnitalChunk]
goalStatement4Inputs = [QP.position, QM.orientation, QP.linearVelocity, 
  QP.angularVelocity]

goal_statements_G_collision = goalStatementStruct (plural physicalProperty)
  (goalStatement4Inputs) --fixme input symbols
  EmptyS (S "of")
  (goalStatement4Inputs) --fixme input symbols
  CP.rigidBody (S "the new") (S "of the" +:+ (plural CP.rigidBody) +:+ 
  S "that have undergone a" +:+ (phrase CP.collision))

goal_statements_G_linear, goal_statements_G_angular, goal_statements_G_detectCollision, goal_statements_G_collision :: [Sentence]

goal_statements_list' :: [Sentence]
goal_statements_list' = map (foldlSent) [goal_statements_G_linear, goal_statements_G_angular, goal_statements_G_detectCollision, 
  goal_statements_G_collision]

goal_statements_list = enumSimple 1 (getAcc goalStmt) goal_statements_list'

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

solution_characteristics_specification :: Section
solution_characteristics_specification = assembler chipmunk everything scsSect [assumSec, tModSec, genDefSec,
  iModSec, dataDefSec, dataConSec]

assumSec, tModSec, genDefSec, iModSec, dataDefSec, dataConSec, scsSect :: SubSec
scsSect = sSubSec solutionCharSpec []
assumSec = (sSubSec assumption [(siCon assumptions_list)])
tModSec = (sSubSec thModel [(siTMod cpTMods)])
genDefSec = (sSubSec genDefn [])
iModSec = (sSubSec inModel [(siIMod iModels)])
dataDefSec = (sSubSec dataDefn [(siSent [data_definitions_intro]), (siDDef cpDDefs)])
dataConSec = (sSubSec dataConst [(siUQI cpInputConstraints), (siUQO cpOutputConstraints)])


-------------------------
-- 4.2.1 : Assumptions --
-------------------------

assumptions_list :: [Contents]
assumptions_list = assumpList newAssumptions

assumpList :: [AssumpChunk] -> [Contents]
assumpList = map Assumption

assumptions_assum1, assumptions_assum2, assumptions_assum3, assumptions_assum4, assumptions_assum5, 
  assumptions_assum6, assumptions_assum7 :: [Sentence]

allObject :: Sentence -> [Sentence]
allObject thing = [S "All objects are", thing]

thereNo :: [Sentence] -> [Sentence]
thereNo [x]      = [S "There is no", x, S "involved throughout the", 
  (phrase simulation)]
thereNo l        = [S "There are no", foldlList l, S "involved throughout the", 
  (phrase simulation)]

assumptions_assum1 = allObject (plural CP.rigidBody)
assumptions_assum2 = allObject (getAcc twoD)
assumptions_assum3 = [S "The library uses a", (phrase CP.cartesian)]
assumptions_assum4 = [S "The axes are defined using", 
  (phrase CP.rightHand)]
assumptions_assum5 = [S "All", (plural CP.rigidBody), 
  (plural CP.collision), S "are vertex-to-edge", 
  (plural CP.collision)]

assumptions_assum6 = thereNo [(phrase CP.damping)]
assumptions_assum7 = thereNo [(plural CM.constraint), (plural CP.joint)]

{-assumptions_list = enumSimple 1 (getAcc assumption) $ map (foldlSent) 
  [assumptions_assum1, assumptions_assum2, assumptions_assum3, assumptions_assum4, assumptions_assum5, 
  assumptions_assum6, assumptions_assum7]-}

assumptions_list_a :: [[Sentence]]
assumptions_list_a = [assumptions_assum1, assumptions_assum2, assumptions_assum3, assumptions_assum4,
  assumptions_assum5, assumptions_assum6, assumptions_assum7]


--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

general_definitions_intro :: Contents
-- general_definitions_GDefs :: [Contents]

general_definitions_intro = foldlSP 
  [S "This", (phrase section_), S "collects the", (plural CM.law) `sAnd` 
  (plural CM.equation), S "that will be used in deriving the", 
  (plural dataDefn) `sC` S "which in turn will be used to build the", 
  (plural inModel)]

-- GDefs not yet implemented --
{-
general_definitions_GDefs :: [Contents]
general_definitions_GDefs = map (Definition . General) gDefs)
-}

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------

data_definitions_intro :: Sentence
data_definitions_intro = foldlSent [S "The", (phrase CPP.dimension)
   `sOf` S "each", (phrase quantity), S "is also given"]

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

------------------------------
-- Collision Diagram        --
------------------------------
{-- should be paired with the last instance model for this example
secCollisionDiagram = Paragraph $ foldlSent [ S "This section presents an image", 
  S "of a typical collision between two 2D rigid bodies labeled A and B,"  
  S "showing the position of the two objects, the collision normal vector n and",
  S "the vectors from the approximate center of mass of each object to the point",
  S "of collision P, rAP and rBP. Note that this figure only presents", 
  S "vertex-to-edge collisions, as per our assumptions (A5)."]
--}

{--fig_1 = Figure (titleize figure +:+ S "1:" +:+ S "Collision between two rigid bodies")
"CollisionDiagram.png" --}


------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------

requirements :: Section
requirements = reqF [functional_requirements, nonfunctional_requirements]

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

functional_requirements :: Section
functional_requirements_list :: Contents

functional_requirements = SRS.funcReq [functional_requirements_list] []

functional_requirements_req1, functional_requirements_req2, functional_requirements_req3, functional_requirements_req4, functional_requirements_req5, functional_requirements_req6,
  functional_requirements_req7, functional_requirements_req8 :: Sentence

  -- | template for requirements
requirementTemplate :: Sentence -> Sentence -> Sentence -> Sentence -> Sentence
requirementTemplate a b x z = foldlSent [S "Determine the", a `sAnd` b, 
  S "over a period of", (phrase QP.time), S "of the", x, z]

  -- | with added constraint
requirementS :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence -> Sentence
requirementS a b d = requirementTemplate (plural a) (plural b) ((getAcc twoD)
  +:+ (plural CP.rigidBody)) d

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
  (makeRef solution_characteristics_specification)]

functional_requirements_req5 = requirementS (QP.position) (QP.velocity) 
  (S "acted upon by a" +:+ (phrase QP.force))

functional_requirements_req6 = requirementS' (QM.orientation) (QP.angularVelocity)

functional_requirements_req7 = foldlSent [S "Determine if any of the", 
  (plural CP.rigidBody), S "in the", (phrase CP.space), 
  S "have collided"]

functional_requirements_req8 = requirementS (QP.position) (QP.velocity) 
  (S "that have undergone a" +:+ (phrase CP.collision))

-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
functional_requirements_list' :: [Sentence]
functional_requirements_list' = [functional_requirements_req1, functional_requirements_req2, functional_requirements_req3, functional_requirements_req4, functional_requirements_req5, functional_requirements_req6,
  functional_requirements_req7, functional_requirements_req8]

functional_requirements_list = enumSimple 1 (getAcc requirement) functional_requirements_list'

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

nonfunctional_requirements :: Section
nonfunctional_requirements_intro :: Contents

nonfunctional_requirements = SRS.nonfuncReq [nonfunctional_requirements_intro] []

chpmnkPriorityNFReqs :: [ConceptChunk]
chpmnkPriorityNFReqs = [correctness, understandability, portability,
  reliability, maintainability]

nonfunctional_requirements_intro = foldlSP 
  [(titleize' game), S "are resource intensive, so", phrase performance,
  S "is a high" +:+. phrase priority, S "Other", plural nonfunctionalRequirement,
  S "that are a", phrase priority +: S "are", 
  foldlList (map phrase chpmnkPriorityNFReqs)]

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

likely_changes :: Section
likely_changes_intro, likely_changes_list :: Contents

likely_changes = SRS.likeChg [likely_changes_intro, likely_changes_list] []

likely_changes_intro = foldlSP [S "This", (phrase section_), S "lists the", 
  (plural likelyChg), S "to be made to the", (phrase physics), (phrase game), 
  (phrase library)]

likely_changes_likelyChg_LC_solver, likely_changes_likelyChg_LC_collisions, likely_changes_likelyChg_LC_damping, 
  likely_changes_likelyChg_LC_constraints :: Sentence

--these statements look like they could be parametrized
likely_changes_likelyChg_LC_solver = (S "internal" +:+ (getAcc CM.ode) :+: 
  S "-solving" +:+ phrase algorithm +:+ S "used by the" +:+
  (phrase library)) `maybeChanged` (S "in the future")

likely_changes_likelyChg_LC_collisions = (phrase library) `maybeExpanded`
  (S "to deal with edge-to-edge and vertex-to-vertex" +:+ (plural CP.collision))

likely_changes_likelyChg_LC_damping = (phrase library) `maybeExpanded` (
  S "to include motion with" +:+ (phrase CP.damping))

likely_changes_likelyChg_LC_constraints = (phrase library) `maybeExpanded` (S "to include" +:+ 
  (plural CP.joint) `sAnd` (plural CM.constraint))

likely_changes_list' :: [Sentence]
likely_changes_list' = [likely_changes_likelyChg_LC_solver, likely_changes_likelyChg_LC_collisions, likely_changes_likelyChg_LC_damping,
  likely_changes_likelyChg_LC_constraints]

likely_changes_list = enumSimple 1 (getAcc likelyChg) likely_changes_list'

-----------------------------------------
-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --
-----------------------------------------

off_the_shelf_solutions :: Section
off_the_shelf_solutions_intro, off_the_shelf_solutions_2dlist, off_the_shelf_solutions_mid, off_the_shelf_solutions_3dlist :: Contents

off_the_shelf_solutions = SRS.offShelfSol [off_the_shelf_solutions_intro, off_the_shelf_solutions_2dlist, off_the_shelf_solutions_mid, off_the_shelf_solutions_3dlist] []

off_the_shelf_solutions_intro = off_the_shelf_solutions_intro_param problem_description physLib

off_the_shelf_solutions_intro_param :: NamedIdea n => Section -> n -> Contents
off_the_shelf_solutions_intro_param problmDescSec lib = Paragraph $ foldlSentCol 
  [S "As mentioned in", (makeRef problmDescSec) `sC`
  S "there already exist free", (phrase openSource), (phrase game) +:+.
  (plural lib), S "Similar", (getAcc twoD), (plural lib), S "are"]

off_the_shelf_solutions_2dlist = enumBullet [(S "Box2D: http://box2d.org/"),
  (S "Nape Physics Engine: http://napephys.com/")]

off_the_shelf_solutions_mid = Paragraph $ foldl (+:+) (EmptyS) [S "Free", (phrase openSource), 
        S "3D", (phrase game), (plural physLib), S "include:"]

off_the_shelf_solutions_3dlist = enumBullet [
  (S "Bullet: http://bulletphysics.org/"),
  (S "Open Dynamics Engine: http://www.ode.org/"),
  (S "Newton Game Dynamics: http://newtondynamics.com/")]

-----------------------------------------------------
-- SECTION 8 : Traceability Matrices and Graph    --
-----------------------------------------------------

traceability_matrices_and_graph :: Section
traceability_matrices_and_graph = traceMGF [traceability_matrices_and_graph_table1, traceability_matrices_and_graph_table2, traceability_matrices_and_graph_table3] traceability_matrices_and_graph_traces 
  [traceability_matrices_and_graph_table1, traceability_matrices_and_graph_table2, traceability_matrices_and_graph_table3] []
--traceability_matrices_and_graph = SRS.traceyMandG [traceability_matrices_and_graph_intro1, traceability_matrices_and_graph_table1, traceability_matrices_and_graph_table2, traceability_matrices_and_graph_table3] []

traceability_matrices_and_graph_traces, traceability_matrices_and_graph_trace1, traceability_matrices_and_graph_trace2, traceability_matrices_and_graph_trace3 :: [Sentence]
traceability_matrices_and_graph_traces = map (foldlList) [traceability_matrices_and_graph_trace1, traceability_matrices_and_graph_trace2, traceability_matrices_and_graph_trace3]

traceability_matrices_and_graph_trace1 = [(plural goalStmt), (plural requirement), (plural inModel), 
  (plural datumConstraint) +:+. S "with each other"]

traceability_matrices_and_graph_trace2 = [(plural thModel), (plural genDefn), (plural dataDefn), 
  (plural inModel), S "on the" +:+. plural assumption]

traceability_matrices_and_graph_trace3 = [(plural thModel), (plural genDefn), (plural dataDefn), 
  (plural inModel) +:+ S "on each other"]

-- these look like they could be generated by the sections above
traceability_matrices_and_graph_instaModel, traceability_matrices_and_graph_assump, traceability_matrices_and_graph_funcReq, traceability_matrices_and_graph_data, traceability_matrices_and_graph_goalstmt, traceability_matrices_and_graph_theoryModel, 
  traceability_matrices_and_graph_genDef, traceability_matrices_and_graph_dataDef, traceability_matrices_and_graph_likelyChg :: [String]

traceability_matrices_and_graph_instaModelRef, traceability_matrices_and_graph_assumpRef, traceability_matrices_and_graph_funcReqRef, traceability_matrices_and_graph_goalstmtRef, 
  traceability_matrices_and_graph_theoryModelRef, traceability_matrices_and_graph_genDefRef, traceability_matrices_and_graph_dataDefRef, traceability_matrices_and_graph_likelyChgRef, 
  traceability_matrices_and_graph_dataRef :: [Sentence]

traceability_matrices_and_graph_instaModel = ["IM1", "IM2", "IM3"]
traceability_matrices_and_graph_instaModelRef = map (refFromType Theory) iModels

traceability_matrices_and_graph_theoryModel = ["T1", "T2", "T3", "T4", "T5"]
traceability_matrices_and_graph_theoryModelRef = map (refFromType Theory) cpTMods

traceability_matrices_and_graph_dataDef = ["DD1","DD2","DD3","DD4","DD5","DD6","DD7","DD8"]
traceability_matrices_and_graph_dataDefRef = map (refFromType Data) cpDDefs

traceability_matrices_and_graph_assump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7"]
traceability_matrices_and_graph_assumpRef = makeListRef assumptions_list_a problem_description

traceability_matrices_and_graph_funcReq =  ["R1","R2","R3", "R4", "R5", "R6", "R7", "R8"]
traceability_matrices_and_graph_funcReqRef = makeListRef functional_requirements_list' functional_requirements

traceability_matrices_and_graph_data = ["Data Constraints"]
traceability_matrices_and_graph_dataRef = [makeRef solution_characteristics_specification]

traceability_matrices_and_graph_goalstmt = ["GS1", "GS2", "GS3", "GS4"]
traceability_matrices_and_graph_goalstmtRef = makeListRef goal_statements_list' problem_description

traceability_matrices_and_graph_genDef = ["GD1", "GD2", "GD3", "GD4", "GD5", "GD6", "GD7"]
traceability_matrices_and_graph_genDefRef = makeListRef traceability_matrices_and_graph_genDef solution_characteristics_specification

traceability_matrices_and_graph_likelyChg = ["LC1", "LC2", "LC3", "LC4"]
traceability_matrices_and_graph_likelyChgRef = makeListRef likely_changes_list' likely_changes


{-- Matrices generation below --}

gS1_t1, gS2_t1, gS3_t1, gS4_t1, r1_t1, r2_t1, r3_t1, r4_t1, r5_t1, r6_t1, r7_t1, 
  r8_t1 :: [String]
gS1_t1 = ["IM1"]
gS2_t1 = ["IM2"]
gS3_t1 = ["IM3"]
gS4_t1 = ["IM3", "R7"]
r1_t1 = []
r2_t1 = ["IM1", "IM2", "R4"]
r3_t1 = ["IM3", "R4"]
r4_t1 = ["Data Constraints"]
r5_t1 = ["IM1"]
r6_t1 = ["IM2"]
r7_t1 = ["R1"]
r8_t1 = ["IM3", "R7"]

traceability_matrices_and_graph_row_header_t1, traceability_matrices_and_graph_col_header_t1 :: [Sentence]
traceability_matrices_and_graph_row_header_t1 = zipWith itemRefToSent traceability_matrices_and_graph_row_t1 (traceability_matrices_and_graph_instaModelRef ++ 
  (take 3 traceability_matrices_and_graph_funcReqRef) ++ traceability_matrices_and_graph_dataRef)
traceability_matrices_and_graph_col_header_t1 = zipWith itemRefToSent 
  (traceability_matrices_and_graph_goalstmt ++ traceability_matrices_and_graph_funcReq) (traceability_matrices_and_graph_goalstmtRef ++ traceability_matrices_and_graph_funcReqRef)

traceability_matrices_and_graph_row_t1 :: [String]
traceability_matrices_and_graph_row_t1 = traceability_matrices_and_graph_instaModel ++ ["R1","R4","R7"] ++ traceability_matrices_and_graph_data

traceability_matrices_and_graph_columns_t1 :: [[String]]
traceability_matrices_and_graph_columns_t1 = [gS1_t1, gS2_t1, gS3_t1, gS4_t1, r1_t1, r2_t1, r3_t1, r4_t1, 
  r5_t1, r6_t1, r7_t1, r8_t1]

traceability_matrices_and_graph_table1 :: Contents
traceability_matrices_and_graph_table1 = Table (EmptyS:(traceability_matrices_and_graph_row_header_t1))
  (makeTMatrix traceability_matrices_and_graph_col_header_t1 traceability_matrices_and_graph_columns_t1 traceability_matrices_and_graph_row_t1)
  (showingCxnBw (traceyMatrix) (titleize' requirement +:+ sParen (makeRef requirements)
  `sC` (titleize' goalStmt) +:+ sParen (makeRef problem_description) `sAnd` S "Other" +:+
  titleize' item)) True "TraceyReqGoalsOther"

traceability_matrices_and_graph_columns_t2 :: [[String]]
traceability_matrices_and_graph_columns_t2 = [t1_t2, t2_t2, t3_t2, t4_t2, t5_t2, gD1_t2, gD2_t2, gD3_t2,
  gD4_t2, gD5_t2, gD6_t2, gD7_t2, dD1_t2, dD2_t2, dD3_t2, dD4_t2, dD5_t2, dD6_t2,
  dD7_t2, dD8_t2, iM1_t2, iM2_t2, iM3_t2, lC1, lC2, lC3, lC4]

t1_t2, t2_t2, t3_t2, t4_t2, t5_t2, gD1_t2, gD2_t2, gD3_t2, gD4_t2, gD5_t2, 
  gD6_t2, gD7_t2, dD1_t2, dD2_t2, dD3_t2, dD4_t2, dD5_t2, dD6_t2, dD7_t2, dD8_t2, 
  iM1_t2, iM2_t2, iM3_t2, lC1, lC2, lC3, lC4 :: [String]
t1_t2 = []
t2_t2 = []
t3_t2 = []
t4_t2 = ["A1"]
t5_t2 = []
gD1_t2 = []
gD2_t2 = []
gD3_t2 = ["A2","A3"]
gD4_t2 = []
gD5_t2 = []
gD6_t2 = []
gD7_t2 = []
dD1_t2 = ["A1","A2"]
dD2_t2 = ["A1","A2","A6"]
dD3_t2 = ["A1","A2","A6"]
dD4_t2 = ["A1","A2","A6"]
dD5_t2 = ["A1","A2","A6"]
dD6_t2 = ["A1","A2","A6"]
dD7_t2 = ["A1","A2","A6"]
dD8_t2 = ["A1","A2","A4","A5"]
iM1_t2 = ["A1","A2","A6","A7"]
iM2_t2 = ["A1","A2","A4","A6","A7"]
iM3_t2 = ["A1","A2","A5","A6","A7"]
lC1 = []
lC2 = ["A5"]
lC3 = ["A6"]
lC4 = ["A7"]

traceability_matrices_and_graph_row_t2, traceability_matrices_and_graph_cols_t2 :: [String]
traceability_matrices_and_graph_row_t2 = traceability_matrices_and_graph_assump

traceability_matrices_and_graph_cols_t2 = (traceability_matrices_and_graph_theoryModel ++ traceability_matrices_and_graph_genDef ++ traceability_matrices_and_graph_dataDef ++ traceability_matrices_and_graph_instaModel ++
  traceability_matrices_and_graph_likelyChg) 
traceability_matrices_and_graph_cols_ref_t2 :: [Sentence]
traceability_matrices_and_graph_cols_ref_t2 = (traceability_matrices_and_graph_theoryModelRef ++ traceability_matrices_and_graph_genDefRef ++ traceability_matrices_and_graph_dataDefRef ++ 
  traceability_matrices_and_graph_instaModelRef ++ traceability_matrices_and_graph_likelyChgRef)

traceability_matrices_and_graph_row_header_t2, traceability_matrices_and_graph_col_header_t2 :: [Sentence]
traceability_matrices_and_graph_row_header_t2 = zipWith itemRefToSent (traceability_matrices_and_graph_row_t2) (traceability_matrices_and_graph_assumpRef)
traceability_matrices_and_graph_col_header_t2 = zipWith itemRefToSent (traceability_matrices_and_graph_cols_t2) (traceability_matrices_and_graph_cols_ref_t2)

traceability_matrices_and_graph_table2 :: Contents
traceability_matrices_and_graph_table2 = Table (EmptyS:traceability_matrices_and_graph_row_header_t2)
  (makeTMatrix traceability_matrices_and_graph_col_header_t2 traceability_matrices_and_graph_columns_t2 traceability_matrices_and_graph_row_t2) 
  (showingCxnBw (traceyMatrix) (titleize' assumption +:+ sParen (makeRef problem_description) 
  `sAnd` S "Other" +:+ titleize' item)) True "TraceyAssumpsOther"


traceability_matrices_and_graph_columns_t3 :: [[String]]
traceability_matrices_and_graph_columns_t3 = [t1_t3, t2_t3, t3_t3, t4_t3, t5_t3, gD1_t3, gD2_t3, gD3_t3, 
  gD4_t3, gD5_t3, gD6_t3, gD7_t3, dD1_t3, dD2_t3, dD3_t3, dD4_t3, dD5_t3, dD6_t3,
  dD7_t3, dD8_t3, iM1_t3, iM2_t3, iM3_t3]

t1_t3, t2_t3, t3_t3, t4_t3, t5_t3, gD1_t3, gD2_t3, gD3_t3, gD4_t3, gD5_t3, gD6_t3,
  gD7_t3, dD1_t3, dD2_t3, dD3_t3, dD4_t3, dD5_t3, dD6_t3, dD7_t3, dD8_t3, iM1_t3,
  iM2_t3, iM3_t3 :: [String]

t1_t3 = [] 
t2_t3 = []
t3_t3 = []
t4_t3 = []
t5_t3 = ["GD6", "GD7"]
gD1_t3 = ["T1"]
gD2_t3 = ["T2", "GD1"]
gD3_t3 = ["T1", "T3"]
gD4_t3 = []
gD5_t3 = ["GD4"]
gD6_t3 = []
gD7_t3 = []
dD1_t3 = []
dD2_t3 = []
dD3_t3 = []
dD4_t3 = []
dD5_t3 = []
dD6_t3 = []
dD7_t3 = []
dD8_t3 = ["T4", "GD1","GD4","GD5","GD7","IM3"]
iM1_t3 = ["T1", "GD3", "DD1","DD2","DD3","DD4"]
iM2_t3 = ["T5", "DD1", "DD2", "DD3", "DD4"]
iM3_t3 = ["GD1", "GD2", "GD6", "GD7", "DD1", "DD8"]

traceability_matrices_and_graph_row_t3 :: [String]
traceability_matrices_and_graph_row_ref_t3 :: [Sentence]
traceability_matrices_and_graph_row_t3 = traceability_matrices_and_graph_theoryModel ++ traceability_matrices_and_graph_genDef ++ traceability_matrices_and_graph_dataDef ++ traceability_matrices_and_graph_instaModel
traceability_matrices_and_graph_row_ref_t3 = traceability_matrices_and_graph_theoryModelRef ++ traceability_matrices_and_graph_genDefRef ++ traceability_matrices_and_graph_dataDefRef ++ 
  traceability_matrices_and_graph_instaModelRef

traceability_matrices_and_graph_col_header_t3, traceability_matrices_and_graph_row_header_t3 :: [Sentence]
traceability_matrices_and_graph_col_header_t3 = zipWith itemRefToSent (traceability_matrices_and_graph_row_t3) (traceability_matrices_and_graph_row_ref_t3)
traceability_matrices_and_graph_row_header_t3 = traceability_matrices_and_graph_col_header_t3

traceability_matrices_and_graph_table3 :: Contents
traceability_matrices_and_graph_table3 = Table (EmptyS:traceability_matrices_and_graph_row_header_t3)
  (makeTMatrix traceability_matrices_and_graph_col_header_t3 traceability_matrices_and_graph_columns_t3 traceability_matrices_and_graph_row_t3)
  (showingCxnBw (traceyMatrix) (titleize' item `sAnd` 
  S "Other" +:+ titleize' section_)) True "TraceyItemsSecs"

-----------------------------------
-- VALUES OF AUXILIARY CONSTANTS --
-----------------------------------

values_of_auxiliary_constatnts :: Section
values_of_auxiliary_constatnts = valsOfAuxConstantsF chipmunk []

----------------
-- REFERENCES --
----------------
--}
-- To be added --
