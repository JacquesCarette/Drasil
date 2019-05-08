module Drasil.GamePhysics.Body where

import Language.Drasil hiding (Vector, organization)
import Language.Drasil.Code (CodeSpec, codeSpec)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)

import Drasil.DocLang (DerivationDisplay(..), DocDesc, DocSection(..), 
  Emphasis(..), Field(..), Fields, InclUnits(IncludeUnits), IntroSec(..), 
  IntroSub(..), RefSec(..), RefTab(..), SCSSub(..), SSDSec(SSDProg), 
  SSDSub(SSDSubVerb, SSDSolChSpec), SolChSpec(SCSProg), SubSec, TConvention(..), 
  TSIntro(..), Verbosity(Verbose), ExistingSolnSec(..), GSDSec(..), GSDSub(..),
  TraceabilitySec(TraceabilityProg), ReqrmntSec(..), ReqsSub(FReqsSub, NonFReqsSub),
  LCsSec(..), UCsSec(..), generateTraceMap',
  assembler, dataConstraintUncertainty,
  inDataConstTbl, intro, mkDoc, outDataConstTbl,
  mkEnumSimpleD, outDataConstTbl, reqF, sSubSec, siCon, siSTitl, siSent,
  traceMGF, tsymb, valsOfAuxConstantsF, getDocDesc, egetDocDesc, generateTraceMap,
  getTraceMapFromTM, getTraceMapFromGD, getTraceMapFromDD, getTraceMapFromIM, getSCSSub,
  generateTraceTable, solution_label)

import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation as Doc(assumption, body,
  concept, condition, consumer, dataDefn, datumConstraint, document, endUser,
  environment, funcReqDom, game, genDefn, goalStmt, guide, inModel,
  information, input_, interface, item, model, nonfunctionalRequirement,
  object, organization, physical, physicalConstraint, physicalSim, physics,
  priority, problem, problemDescription, product_, project, property, quantity,
  realtime, reference, requirement, section_, simulation, software, softwareSys,
  srs, srsDomains, system, systemConstraint, sysCont, task, template,
  termAndDef, thModel, traceyMatrix, user, userCharacteristic, doccon, doccon')
import Data.Drasil.Concepts.Education (frstYr, highSchoolCalculus,
  highSchoolPhysics, educon)
import Data.Drasil.Concepts.Software (physLib, understandability, portability,
  reliability, maintainability, performance, correctness, softwarecon, reliability)
import Data.Drasil.People (alex, luthfi)
import Data.Drasil.Phrase (for')
import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), foldlList, 
  foldlSent, foldlSent_, foldlSentCol, foldlSP, foldlSPCol, sAnd, showingCxnBw, 
  sOf, sOr)
import Data.Drasil.SI_Units (metre, kilogram, second, newton, radian,
  derived, fundamentals, joule)
import Data.Drasil.Software.Products (openSource, prodtcon, sciCompS, videoGame)
import Data.Drasil.Utils (makeTMatrix, itemRefToSent,
  makeListRef, bulletFlat, bulletNested, enumBullet)

import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (ctrOfMass, dimension)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody, elasticity, 
  cartesian, friction, rightHand, collision, space, physicCon)
import qualified Data.Drasil.Concepts.Math as CM (equation, surface, law, mathcon, mathcon')
import qualified Data.Drasil.Quantities.Math as QM (orientation)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import qualified Data.Drasil.Quantities.Physics as QP (angularVelocity, force, 
  position, time, velocity)
import Drasil.GamePhysics.Assumptions(assumptions)
import Drasil.GamePhysics.Changes (unlikelyChangesList', unlikelyChangeswithIntro,
 likelyChangesListwithIntro, likelyChangesList')
import Drasil.GamePhysics.Concepts (chipmunk, cpAcronyms, twoD)
import Drasil.GamePhysics.DataDefs (cpDDefs, cpQDefs, dataDefns)
import Drasil.GamePhysics.Goals (goals)
import Drasil.GamePhysics.IMods (iModels_new, im1_new, im2_new, im3_new)
import Drasil.GamePhysics.References (cpCitations, parnas1972, parnasClements1984)
import Drasil.GamePhysics.TMods (t1NewtonSL_new, t2NewtonTL_new, 
  t3NewtonLUG_new, t4ChaslesThm_new, t5NewtonSLR_new, cpTMods_new)
import Drasil.GamePhysics.Unitals (cpSymbolsAll, cpOutputConstraints,
  inputSymbols, outputSymbols, cpInputConstraints, gamephySymbols)

import Control.Lens ((^.))
import qualified Data.Map as Map

authors :: People
authors = [alex, luthfi]

auths :: Sentence
auths = S $ manyNames authors

chipmunkSRS' :: Document
chipmunkSRS' = mkDoc mkSRS for' chipmunkSysInfo

check_si :: [UnitDefn] -- FIXME
check_si = collectUnits everything symbTT 

mkSRS :: DocDesc 
mkSRS = [RefSec $ RefProg intro [TUnits, tsymb tableOfSymbols, TAandA],
  IntroSec $ IntroProg para1_introduction_intro (short chipmunk)
  [IPurpose para1_purpose_of_document_intro,
   IScope scope_of_requirements_intro_p1 scope_of_requirements_intro_p2,
   IChar [] [S "rigid body dynamics", phrase highSchoolCalculus] [],
   IOrgSec organization_of_documents_intro inModel (SRS.inModel [] []) EmptyS],
   GSDSec $ GSDProg2 [
    SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList],
    UsrChars [user_characteristics_intro], SystCons [] []],
   SSDSec $ SSDProg [SSDSubVerb problem_description
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
            [t1NewtonSL_new, t2NewtonTL_new, t3NewtonLUG_new, t4ChaslesThm_new, t5NewtonSLR_new]
        , GDs [] [] [] HideDerivation -- No Gen Defs for Gamephysics
        , DDs [] ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields)
            [im1_new, im2_new, im3_new] ShowDerivation
        , Constraints EmptyS dataConstraintUncertainty (S "FIXME")
            [inDataConstTbl cpInputConstraints, outDataConstTbl cpOutputConstraints]
        ]
      ],
    ReqrmntSec $ ReqsProg [
      FReqsSub functional_requirements_list,
      NonFReqsSub [performance] gmpriorityNFReqs -- The way to render the NonFReqsSub is right for here, fixme.
        (S "Games are resource intensive") (S "")],
    LCsSec $ LCsProg likelyChangesListwithIntro,
    UCsSec $ UCsProg unlikelyChangeswithIntro,
    ExistingSolnSec $ ExistSolnVerb off_the_shelf_solutions,
    TraceabilitySec $ TraceabilityProg [traceTable1, traceMatTabReqGoalOther, traceMatTabAssump,
      traceMatTabDefnModel] traceability_matrices_and_graph_traces
      (map LlC [traceTable1, traceMatTabReqGoalOther, traceMatTabAssump, traceMatTabDefnModel]) [],
    Verbatim values_of_auxiliary_constatnts,
    Bibliography]
      where tableOfSymbols = [TSPurpose, TypogConvention[Vector Bold], SymbOrder]

game_label :: TraceMap
game_label = Map.union (generateTraceMap mkSRS) $ generateTraceMap' game_concins

game_refby :: RefbyMap
game_refby = generateRefbyMap game_label

game_datadefn :: [DataDefinition]
game_datadefn = getTraceMapFromDD $ getSCSSub mkSRS

game_insmodel :: [InstanceModel]
game_insmodel = getTraceMapFromIM $ getSCSSub mkSRS

game_gendef :: [GenDefn]
game_gendef = getTraceMapFromGD $ getSCSSub mkSRS

game_theory :: [TheoryModel]
game_theory = getTraceMapFromTM $ getSCSSub mkSRS

game_concins :: [ConceptInstance]
game_concins = assumptions ++ likelyChangesList' ++ unlikelyChangesList' ++
  functional_requirements_list'

game_section :: [Section]
game_section = game_sec

game_sec :: [Section]
game_sec = extractSection chipmunkSRS'

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

    --FIXME: Need to be able to print defn for gravitational constant.

chipmunkSysInfo :: SystemInformation
chipmunkSysInfo = SI {
  _sys = chipmunk,
  _kind = srs,
  _authors = authors,
  _quants = symbTT, 
  _concepts = ([] :: [DefinedQuantityDict]),
  _definitions = cpDDefs,
  _datadefs = dataDefns,
  _inputs = inputSymbols,
  _outputs = outputSymbols, 
  _defSequence = cpQDefs,
  _constraints = cpInputConstraints,
  _constants = [],
  _sysinfodb = everything,
  _usedinfodb = usedDB,
   refdb = cpRefDB
}

symbTT :: [DefinedQuantityDict]
symbTT = ccss (getDocDesc mkSRS) (egetDocDesc mkSRS) everything

cpRefDB :: ReferenceDB
cpRefDB = rdb cpCitations game_concins

--FIXME: All named ideas, not just acronyms.

chipUnits :: [UnitDefn] -- FIXME
chipUnits = map unitWrapper [metre, kilogram, second, joule] ++ map unitWrapper [newton, radian]

everything :: ChunkDB
everything = cdb (map qw iModels_new ++ map qw cpSymbolsAll) (map nw cpSymbolsAll
  ++ map nw cpAcronyms ++ map nw prodtcon ++ map nw iModels_new
  ++ map nw softwarecon ++ map nw doccon ++ map nw doccon'
  ++ map nw CP.physicCon ++ map nw educon ++ [nw algorithm] ++ map nw derived
  ++ map nw fundamentals ++ map nw CM.mathcon ++ map nw CM.mathcon')
  (map cw gamephySymbols ++ srsDomains ++ map cw iModels_new) chipUnits
  game_label game_refby game_datadefn game_insmodel game_gendef game_theory
  game_concins game_section []

usedDB :: ChunkDB
usedDB = cdb (map qw symbTT) (map nw cpSymbolsAll ++ map nw cpAcronyms
 ++ map nw check_si) ([] :: [ConceptChunk]) check_si game_label game_refby
 game_datadefn game_insmodel game_gendef game_theory game_concins game_section
 []

printSetting :: PrintingInformation
printSetting = PI everything defaultConfiguration

chipCode :: CodeSpec
chipCode = codeSpec chipmunkSysInfo []

resourcePath :: String
resourcePath = "../../../datafiles/GamePhysics/"

gmpriorityNFReqs :: [ConceptChunk]
gmpriorityNFReqs = [correctness, understandability, portability, reliability,
  maintainability]

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
para1_purpose_of_document_intro = para1_purpose_of_document_param chipmunk 
  document programDescription (plural game) (map plural detailsAndGoal)

programDescription :: Sentence
programDescription = foldlSent_ [(phrase openSource), getAcc twoD, 
  (phrase CP.rigidBody), (phrase physLib)]

para1_purpose_of_document_param :: (Idea a, NamedIdea b) => a -> b -> Sentence -> Sentence ->
  [Sentence] -> Sentence
para1_purpose_of_document_param progName typeOf progDescrip appOf listOf = foldlSent 
  [S "This", (phrase typeOf), S "descibes the modeling of an",
  progDescrip, S "used for" +:+. appOf, S "The", 
  foldlList Comma List listOf, S "used in", (short progName), 
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
  
scope_of_requirements_intro_p2 = foldlSent_ [S "simulates how these", 
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
  (phrase sciCompS), S "proposed by", makeCiteS parnas1972 `sAnd` 
  makeCiteS parnasClements1984]

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------
--------------------------
-- 3.1 : System Context --
--------------------------

sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [makeRef2S sysCtxFig1 +:+ S "shows the" +:+. phrase sysCont,
   S "A circle represents an external entity outside the" +:+ phrase software
   `sC` S "the", phrase user, S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself", (sParen $ short chipmunk) +:+. EmptyS,
   S "Arrows are used to show the data flow between the" +:+ phrase system,
   S "and its" +:+ phrase environment]

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) 
  (resourcePath ++ "sysctx.png")

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol [S "The interaction between the", phrase product_,
   S "and the", phrase user, S "is through an application programming" +:+.
   phrase interface, S "The responsibilities of the", phrase user, 
   S "and the", phrase system, S "are as follows"]

sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide initial" +:+ plural condition +:+ S "of the" +:+
    phrase physical +:+ S"state of the" +:+ phrase simulation `sC`
    plural (CP.rigidBody) +:+ S "present, and" +:+ plural QP.force +:+.
    S "applied to them",
  S "Ensure application programming" +:+ phrase interface +:+
    S "use complies with the" +:+ phrase user +:+. phrase guide,
  S "Ensure required" +:+ phrase software +:+ plural assumption +:+
    S "(FIXME REF)" +:+ S "are appropriate for any particular" +:+
    phrase problem +:+ S "the" +:+ phrase software +:+. S "addresses"]

sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Determine if the" +:+ plural input_ +:+ S "and" +:+
    phrase simulation +:+ S "state satisfy the required" +:+
    (phrase physical `sAnd` plural systemConstraint) +:+. S "(FIXME REF)",
  S "Calculate the new state of all" +:+ plural CP.rigidBody +:+
    S "within the" +:+ phrase simulation +:+ S "at each" +:+
    phrase simulation +:+. S "step",
  S "Provide updated" +:+ phrase physical +:+ S "state of all" +:+
    plural CP.rigidBody +:+ S "at the end of a" +:+ phrase simulation +:+.
    S "step"]

sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short chipmunk +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

userCharacteristicSect :: SubSec
userCharacteristicSect = sSubSec userCharacteristic [(siCon [user_characteristics_intro])]

user_characteristics_intro :: Contents
user_characteristics_intro = foldlSP
  [S "The", phrase endUser `sOf` short chipmunk,
  S "should have an understanding of", phrase frstYr, S "programming",
  plural concept `sAnd` S "an understanding of", phrase highSchoolPhysics]

-------------------------------
-- 3.3 : System Constraints  --
-------------------------------

systemConstraintSect :: SubSec
systemConstraintSect = sSubSec systemConstraint []

---------------------------------------------
-- SECTION 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

-- NOTE: Section 4 remains incomplete. General definitions and instance models
-- have not been encoded.

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
  (phrase app), S "developers to include", (phrase Doc.physics), S "in their" +:+. 
  (plural product_), S "There are a few free" `sC` (phrase openSource) `sAnd` S "high quality",
  (plural lib), S "available to be used for", phrase consumer, plural product_ +:+. 
  (sParen $ makeRef2S off_the_shelf_solutions), S "By creating a simple, lightweight, fast and portable",
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

terminology_label :: Reference
terminology_label = makeLstRef "terminologyGM" "terminologyGM"

terminology_and_definitions_bullets = LlC $ enumBullet terminology_label
  (map (\x -> (at_start x) +: EmptyS +:+ (x ^. defn)) terminology_and_definitions_terms)

-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

goal_statements_list :: [Contents]

goalStatementSect :: SubSec
goalStatementSect = sSubSec goalStmt [siCon goal_statements_list]

{-goalStatementStruct :: (NamedIdea a, NamedIdea b) => Sentence -> [a] -> 
  Sentence -> Sentence -> [a] -> b -> Sentence -> Sentence -> [Sentence]
goalStatementStruct state inputs wrt adjective outputs objct condition1 condition2 = 
  [S "Given the", initial state, (listOfInputs wrt), adjective, S "a set of", 
  (plural objct) `sC` S "determine", condition1, listOfOutputs, 
  S "over a period of", (phrase QP.time), condition2]
  where initial EmptyS      = S "initial"
        initial p           = p `sC` (S "initial")
        listOfInputs EmptyS = (foldlList Comma List $ map plural inputs)
        listOfInputs i      = (foldlList Comma List $ map plural inputs) `sC` S "and" +:+ i
        listOfOutputs       = (foldlList Comma List $ map plural outputs)
--GS1: Given the physical properties, initial positions and velocities, 
--and forces applied on a set of rigid bodies, determine their new positions 
--and velocities over a period of time. 
goal_statements_G_linear = goalStatementStruct (plural physicalProperty) 
  (take 2 inputSymbols) (plural QP.force) (S "applied on")
  (take 2 outputSymbols) CP.rigidBody
  (S "their new") EmptyS

goal_statements_G_angular = goalStatementStruct (plural physicalProperty) 
  (drop 3 $ take 5 inputSymbols) (plural QP.force) (S "applied on")
  (drop 3 $ take 5 inputSymbols) CP.rigidBody
  (S "their new") EmptyS-}

{-goal_statements_G_detectCollision = goalStatementStruct EmptyS
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
  S "that have undergone a" +:+ (phrase CP.collision))-}

{-goal_statements_G_linear, goal_statements_G_angular, 
  goal_statements_G_detectCollision, goal_statements_G_collision :: [Sentence]

goal_statements_list' :: [Sentence]
goal_statements_list' = map (foldlSent) [goal_statements_G_linear, 
  goal_statements_G_angular, goal_statements_G_detectCollision, 
  goal_statements_G_collision]-}

  {-, 
  goal_statements_G_detectCollision, goal_statements_G_collision-} 

  {-, goal_statements_G_detectCollision, 
  goal_statements_G_collision-}

goal_statements_list = mkEnumSimpleD goals

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

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
functional_requirements_list :: [Contents]

functional_requirements = SRS.funcReq functional_requirements_list []

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

-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
functional_requirements_list' :: [ConceptInstance]
functional_requirements_list' = [reqSS, reqIIC, reqISP, reqVPC, reqCTOT,
  reqCROT, reqDC, reqDCROT]

functional_requirements_list = mkEnumSimpleD
  functional_requirements_list'

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
  foldlList Comma List (map phrase chpmnkPriorityNFReqs)]

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

-----------------------------------------
-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --
-----------------------------------------

off_the_shelf_solutions :: Section
off_the_shelf_solutions_intro, off_the_shelf_solutions_2dlist, 
  off_the_shelf_solutions_mid, off_the_shelf_solutions_3dlist :: Contents

off_the_shelf_solutions = SRS.offShelfSol [off_the_shelf_solutions_intro, 
  off_the_shelf_solutions_2dlist, off_the_shelf_solutions_mid, off_the_shelf_solutions_3dlist] []

off_the_shelf_solutions_intro = off_the_shelf_solutions_intro_param problem_description physLib

off_the_shelf_solutions_intro_param :: NamedIdea n => Section -> n -> Contents
off_the_shelf_solutions_intro_param problmDescSec lib = mkParagraph $ foldlSentCol 
  [S "As mentioned in", (makeRef2S problmDescSec) `sC`
  S "there already exist free", (phrase openSource), (phrase game) +:+.
  (plural lib), S "Similar", (getAcc twoD), (plural lib), S "are"]

off_the_shelf_solutions_2dlist = LlC $ enumBullet solution_label [(S "Box2D: http://box2d.org/"),
  (S "Nape Physics Engine: http://napephys.com/")]

off_the_shelf_solutions_mid = mkParagraph $ foldl (+:+) (EmptyS) [S "Free", (phrase openSource), 
        S "3D", (phrase game), (plural physLib), S "include:"]

off_the_shelf_solutions_3dlist = LlC $ enumBullet solution_label [
  (S "Bullet: http://bulletphysics.org/"),
  (S "Open Dynamics Engine: http://www.ode.org/"),
  (S "Newton Game Dynamics: http://newtondynamics.com/")]

-----------------------------------------------------
-- SECTION 8 : Traceability Matrices and Graph    --
-----------------------------------------------------
traceTable1 :: LabelledContent
traceTable1 = generateTraceTable chipmunkSysInfo

traceability_matrices_and_graph :: Section
traceability_matrices_and_graph = traceMGF [traceMatTabReqGoalOther, traceMatTabAssump,
  traceMatTabDefnModel] traceability_matrices_and_graph_traces
  (map LlC [traceMatTabReqGoalOther, traceMatTabAssump, traceMatTabDefnModel]) []

traceability_matrices_and_graph_traces, traceability_matrices_and_graph_trace1,
  traceability_matrices_and_graph_trace2, traceability_matrices_and_graph_trace3 :: [Sentence]
traceability_matrices_and_graph_traces = map (foldlList Comma List) 
  [traceability_matrices_and_graph_trace1, traceability_matrices_and_graph_trace2,
   traceability_matrices_and_graph_trace3]

traceability_matrices_and_graph_trace1 = [(plural goalStmt), 
  (plural requirement), (plural inModel), (plural datumConstraint) +:+. S "with each other"]

traceability_matrices_and_graph_trace2 = [(plural thModel), (plural genDefn), (plural dataDefn), 
  (plural inModel), S "on the" +:+. plural assumption]

traceability_matrices_and_graph_trace3 = [(plural thModel), (plural genDefn), (plural dataDefn), 
  (plural inModel) +:+ S "on each other"]

-- these look like they could be generated by the sections above
traceMatInstaModel, traceMatAssump, traceMatFuncReq, traceMatData,
  traceMatGoalStmt, traceMatTheoryModel, traceMatGenDef, traceMatDataDef,
  traceMatLikelyChg :: [String]

traceMatInstaModelRef, traceMatAssumpRef, traceMatFuncReqRef, traceMatGoalStmtRef,
  traceMatTheoryModelRef, traceMatGenDefRef, traceMatDataDefRef,
  traceMatLikelyChgRef, traceMatDataRef :: [Sentence]

traceMatInstaModel = ["IM1", "IM2", "IM3"]
traceMatInstaModelRef = map makeRef2S iModels_new

traceMatTheoryModel = ["T1", "T2", "T3", "T4", "T5"]
traceMatTheoryModelRef = map makeRef2S cpTMods_new

traceMatDataDef = ["DD1","DD2","DD3","DD4","DD5","DD6","DD7","DD8"]
traceMatDataDefRef = map makeRef2S dataDefns

traceMatAssump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7"]
traceMatAssumpRef = map makeRef2S assumptions

traceMatFuncReq =  ["R1","R2","R3", "R4", "R5", "R6", "R7", "R8"]
traceMatFuncReqRef = map makeRef2S functional_requirements_list'

traceMatData = ["Data Constraints"]
traceMatDataRef = [makeRef2S $ SRS.solCharSpec ([]::[Contents]) ([]::[Section])]

traceMatGoalStmt = ["GS1", "GS2", "GS3", "GS4"]
traceMatGoalStmtRef = makeListRef goals problem_description

traceMatGenDef = ["GD1", "GD2", "GD3", "GD4", "GD5", "GD6", "GD7"]
traceMatGenDefRef = replicate (length traceMatGenDef) (makeRef2S $ SRS.solCharSpec ([]::[Contents]) ([]::[Section])) -- FIXME: hack?

traceMatLikelyChg = ["LC1", "LC2", "LC3", "LC4"]
traceMatLikelyChgRef = map makeRef2S likelyChangesList'


{-- Matrices generation below --}

traceMatTabReqGoalOtherGS1, traceMatTabReqGoalOtherGS2, traceMatTabReqGoalOtherGS3,
  traceMatTabReqGoalOtherGS4, traceMatTabReqGoalOtherReq1, traceMatTabReqGoalOtherReq2,
  traceMatTabReqGoalOtherReq3, traceMatTabReqGoalOtherReq4, traceMatTabReqGoalOtherReq5,
  traceMatTabReqGoalOtherReq6, traceMatTabReqGoalOtherReq7,
  traceMatTabReqGoalOtherReq8 :: [String]
traceMatTabReqGoalOtherGS1 = ["IM1"]
traceMatTabReqGoalOtherGS2 = ["IM2"]
traceMatTabReqGoalOtherGS3 = ["IM3"]
traceMatTabReqGoalOtherGS4 = ["IM3", "R7"]
traceMatTabReqGoalOtherReq1 = []
traceMatTabReqGoalOtherReq2 = ["IM1", "IM2", "R4"]
traceMatTabReqGoalOtherReq3 = ["IM3", "R4"]
traceMatTabReqGoalOtherReq4 = ["Data Constraints"]
traceMatTabReqGoalOtherReq5 = ["IM1"]
traceMatTabReqGoalOtherReq6 = ["IM2"]
traceMatTabReqGoalOtherReq7 = ["R1"]
traceMatTabReqGoalOtherReq8 = ["IM3", "R7"]

traceMatTabReqGoalOtherRowHead, traceMatTabReqGoalOtherColHead :: [Sentence]
traceMatTabReqGoalOtherRowHead = zipWith itemRefToSent traceMatTabReqGoalOtherRow
  (traceMatInstaModelRef ++ (take 3 traceMatFuncReqRef) ++ traceMatDataRef)
traceMatTabReqGoalOtherColHead = zipWith itemRefToSent (traceMatGoalStmt ++
  traceMatFuncReq) (traceMatGoalStmtRef ++ traceMatFuncReqRef)

traceMatTabReqGoalOtherRow :: [String]
traceMatTabReqGoalOtherRow = traceMatInstaModel ++ ["R1","R4","R7"] ++
  traceMatData

traceMatTabReqGoalOtherCol :: [[String]]
traceMatTabReqGoalOtherCol = [traceMatTabReqGoalOtherGS1, traceMatTabReqGoalOtherGS2,
  traceMatTabReqGoalOtherGS3, traceMatTabReqGoalOtherGS4, traceMatTabReqGoalOtherReq1,
  traceMatTabReqGoalOtherReq2, traceMatTabReqGoalOtherReq3, traceMatTabReqGoalOtherReq4,
  traceMatTabReqGoalOtherReq5, traceMatTabReqGoalOtherReq6, traceMatTabReqGoalOtherReq7,
  traceMatTabReqGoalOtherReq8]

traceMatTabReqGoalOther :: LabelledContent
traceMatTabReqGoalOther = llcc (makeTabRef "TraceyReqGoalsOther") $ Table 
  (EmptyS:(traceMatTabReqGoalOtherRowHead))
  (makeTMatrix traceMatTabReqGoalOtherColHead traceMatTabReqGoalOtherCol
  traceMatTabReqGoalOtherRow)
  (showingCxnBw (traceyMatrix) (titleize' requirement +:+ sParen (makeRef2S requirements)
  `sC` (titleize' goalStmt) +:+ sParen (makeRef2S problem_description) `sAnd` S "Other" +:+
  titleize' item)) True

traceMatTabAssumpCol' :: [[String]]
traceMatTabAssumpCol' = [traceMatTabAssumpMT1, traceMatTabAssumpMT2,
  traceMatTabAssumpMT3, traceMatTabAssumpMT4, traceMatTabAssumpMT5,
  traceMatTabAssumpGD1, traceMatTabAssumpGD2, traceMatTabAssumpGD3,
  traceMatTabAssumpGD4, traceMatTabAssumpGD5, traceMatTabAssumpGD6,
  traceMatTabAssumpGD7, traceMatTabAssumpDD1, traceMatTabAssumpDD2,
  traceMatTabAssumpDD3, traceMatTabAssumpDD4, traceMatTabAssumpDD5,
  traceMatTabAssumpDD6, traceMatTabAssumpDD7, traceMatTabAssumpDD8,
  traceMatTabAssumpIM1, traceMatTabAssumpIM2, traceMatTabAssumpIM3,
  traceMatTabAssumpLC1, traceMatTabAssumpLC2, traceMatTabAssumpLC3,
  traceMatTabAssumpLC4]

traceMatTabAssumpMT1, traceMatTabAssumpMT2, traceMatTabAssumpMT3,
  traceMatTabAssumpMT4, traceMatTabAssumpMT5, traceMatTabAssumpGD1,
  traceMatTabAssumpGD2, traceMatTabAssumpGD3, traceMatTabAssumpGD4,
  traceMatTabAssumpGD5, traceMatTabAssumpGD6, traceMatTabAssumpGD7,
  traceMatTabAssumpDD1, traceMatTabAssumpDD2, traceMatTabAssumpDD3,
  traceMatTabAssumpDD4, traceMatTabAssumpDD5, traceMatTabAssumpDD6,
  traceMatTabAssumpDD7, traceMatTabAssumpDD8, traceMatTabAssumpIM1,
  traceMatTabAssumpIM2, traceMatTabAssumpIM3, traceMatTabAssumpLC1,
  traceMatTabAssumpLC2, traceMatTabAssumpLC3, traceMatTabAssumpLC4 :: [String]
traceMatTabAssumpMT1 = []
traceMatTabAssumpMT2 = []
traceMatTabAssumpMT3 = []
traceMatTabAssumpMT4 = ["A1"]
traceMatTabAssumpMT5 = []
traceMatTabAssumpGD1 = []
traceMatTabAssumpGD2 = []
traceMatTabAssumpGD3 = ["A2","A3"]
traceMatTabAssumpGD4 = []
traceMatTabAssumpGD5 = []
traceMatTabAssumpGD6 = []
traceMatTabAssumpGD7 = []
traceMatTabAssumpDD1 = ["A1","A2"]
traceMatTabAssumpDD2 = ["A1","A2","A6"]
traceMatTabAssumpDD3 = ["A1","A2","A6"]
traceMatTabAssumpDD4 = ["A1","A2","A6"]
traceMatTabAssumpDD5 = ["A1","A2","A6"]
traceMatTabAssumpDD6 = ["A1","A2","A6"]
traceMatTabAssumpDD7 = ["A1","A2","A6"]
traceMatTabAssumpDD8 = ["A1","A2","A4","A5"]
traceMatTabAssumpIM1 = ["A1","A2","A6","A7"]
traceMatTabAssumpIM2 = ["A1","A2","A4","A6","A7"]
traceMatTabAssumpIM3 = ["A1","A2","A5","A6","A7"]
traceMatTabAssumpLC1 = []
traceMatTabAssumpLC2 = ["A5"]
traceMatTabAssumpLC3 = ["A6"]
traceMatTabAssumpLC4 = ["A7"]

traceMatTabAssumpRow, traceMatTabAssumpCol :: [String]
traceMatTabAssumpRow = traceMatAssump

traceMatTabAssumpCol = traceMatTheoryModel ++ traceMatGenDef ++
  traceMatDataDef ++ traceMatInstaModel ++ traceMatLikelyChg
traceMatTabAssumpColRef :: [Sentence]
traceMatTabAssumpColRef = traceMatTheoryModelRef ++ traceMatGenDefRef ++
  traceMatDataDefRef ++ traceMatInstaModelRef ++ traceMatLikelyChgRef

traceMatTabAssumpRowHead, traceMatTabAssumpColHead :: [Sentence]
traceMatTabAssumpRowHead = zipWith itemRefToSent traceMatTabAssumpRow
  traceMatAssumpRef
traceMatTabAssumpColHead = zipWith itemRefToSent traceMatTabAssumpCol
  traceMatTabAssumpColRef

traceMatTabAssump :: LabelledContent
traceMatTabAssump = llcc (makeTabRef "TraceyAssumpsOther") $ Table
  (EmptyS:traceMatTabAssumpRowHead)
  (makeTMatrix traceMatTabAssumpColHead traceMatTabAssumpCol' traceMatTabAssumpRow)
  (showingCxnBw (traceyMatrix) (titleize' assumption +:+ sParen (makeRef2S problem_description)
  `sAnd` S "Other" +:+ titleize' item)) True

traceMatTabDefnModelCol :: [[String]]
traceMatTabDefnModelCol = [traceMatTabDefnModelTM1, traceMatTabDefnModelTM2,
  traceMatTabDefnModelTM3, traceMatTabDefnModelTM4, traceMatTabDefnModelTM5,
  traceMatTabDefnModelGD1, traceMatTabDefnModelGD2, traceMatTabDefnModelGD3,
  traceMatTabDefnModelGD4, traceMatTabDefnModelGD5, traceMatTabDefnModelGD6,
  traceMatTabDefnModelGD7, traceMatTabDefnModelDD1, traceMatTabDefnModelDD2,
  traceMatTabDefnModelDD3, traceMatTabDefnModelDD4, traceMatTabDefnModelDD5,
  traceMatTabDefnModelDD6, traceMatTabDefnModelDD7, traceMatTabDefnModelDD8,
  traceMatTabDefnModelIM1, traceMatTabDefnModelIM2, traceMatTabDefnModelIM3]

traceMatTabDefnModelTM1, traceMatTabDefnModelTM2, traceMatTabDefnModelTM3,
  traceMatTabDefnModelTM4, traceMatTabDefnModelTM5, traceMatTabDefnModelGD1,
  traceMatTabDefnModelGD2, traceMatTabDefnModelGD3, traceMatTabDefnModelGD4,
  traceMatTabDefnModelGD5, traceMatTabDefnModelGD6, traceMatTabDefnModelGD7,
  traceMatTabDefnModelDD1, traceMatTabDefnModelDD2, traceMatTabDefnModelDD3,
  traceMatTabDefnModelDD4, traceMatTabDefnModelDD5, traceMatTabDefnModelDD6,
  traceMatTabDefnModelDD7, traceMatTabDefnModelDD8, traceMatTabDefnModelIM1,
  traceMatTabDefnModelIM2, traceMatTabDefnModelIM3 :: [String]

traceMatTabDefnModelTM1 = []
traceMatTabDefnModelTM2 = []
traceMatTabDefnModelTM3 = []
traceMatTabDefnModelTM4 = []
traceMatTabDefnModelTM5 = ["GD6", "GD7"]
traceMatTabDefnModelGD1 = ["T1"]
traceMatTabDefnModelGD2 = ["T2", "GD1"]
traceMatTabDefnModelGD3 = ["T1", "T3"]
traceMatTabDefnModelGD4 = []
traceMatTabDefnModelGD5 = ["GD4"]
traceMatTabDefnModelGD6 = []
traceMatTabDefnModelGD7 = []
traceMatTabDefnModelDD1 = []
traceMatTabDefnModelDD2 = []
traceMatTabDefnModelDD3 = []
traceMatTabDefnModelDD4 = []
traceMatTabDefnModelDD5 = []
traceMatTabDefnModelDD6 = []
traceMatTabDefnModelDD7 = []
traceMatTabDefnModelDD8 = ["T4", "GD1","GD4","GD5","GD7","IM3"]
traceMatTabDefnModelIM1 = ["T1", "GD3", "DD1","DD2","DD3","DD4"]
traceMatTabDefnModelIM2 = ["T5", "DD1", "DD2", "DD3", "DD4"]
traceMatTabDefnModelIM3 = ["GD1", "GD2", "GD6", "GD7", "DD1", "DD8"]

traceMatTabDefnModelRow :: [String]
traceMatTabDefnModelRowRef :: [Sentence]
traceMatTabDefnModelRow = traceMatTheoryModel ++ traceMatGenDef ++
  traceMatDataDef ++ traceMatInstaModel
traceMatTabDefnModelRowRef = traceMatTheoryModelRef ++ traceMatGenDefRef ++
  traceMatDataDefRef ++ traceMatInstaModelRef

traceMatTabDefnModelColHead, traceMatTabDefnModelRowHead :: [Sentence]
traceMatTabDefnModelColHead = zipWith itemRefToSent traceMatTabDefnModelRow
  traceMatTabDefnModelRowRef
traceMatTabDefnModelRowHead = traceMatTabDefnModelColHead

traceMatTabDefnModel :: LabelledContent
traceMatTabDefnModel = llcc (makeTabRef "TraceyItemsSecs") $ Table 
  (EmptyS:traceMatTabDefnModelRowHead)
  (makeTMatrix traceMatTabDefnModelColHead traceMatTabDefnModelCol
  traceMatTabDefnModelRow) (showingCxnBw (traceyMatrix) (titleize' item `sAnd`
  S "Other" +:+ titleize' section_)) True

-----------------------------------
-- VALUES OF AUXILIARY CONSTANTS --
-----------------------------------

values_of_auxiliary_constatnts :: Section
values_of_auxiliary_constatnts = valsOfAuxConstantsF chipmunk []

----------------
-- REFERENCES --
----------------
-- To be added --
