module Drasil.GamePhysics.Body where

import Control.Lens ((^.))

import Data.Drasil.Concepts.Documentation (assumption, body,
  concept, condition, consumer, dataConst, dataDefn, datumConstraint,
  document, endUser, environment, game, genDefn, generalSystemDescription,
  goalStmt, guide, inModel, information, input_, interface, item,
  model, nonfunctionalRequirement, object, organization, physical,
  physicalConstraint, physicalProperty, physicalSim, physics, priority,
  problem, problemDescription, product_, project, property, quantity, realtime,
  reference, requirement, section_, simulation, software, softwareSys,
  solutionCharSpec, srs, system, systemConstraint, sysCont, task, template,
  termAndDef, thModel, traceyMatrix, user, userCharacteristic)
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
import Drasil.GamePhysics.Changes (likelyChanges, likelyChangesList', unlikelyChanges)
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
import qualified Data.Drasil.Concepts.Math as CM (equation, surface,
  constraint, law)

import qualified Data.Drasil.Quantities.Math as QM (orientation)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import qualified Data.Drasil.Quantities.Physics as QP (time, 
  position, force, velocity, angularVelocity, linearVelocity)
import Data.Drasil.SentenceStructures (foldlSent, foldlSent_, foldlList, sOf,
  sAnd, sOr, foldlSentCol, foldlSP, foldlSPCol, showingCxnBw)
import Data.Drasil.Software.Products (videoGame, openSource, sciCompS)
import Data.Drasil.Utils (makeTMatrix, itemRefToSent, refFromType,
  makeListRef, bulletFlat, bulletNested, enumSimple, enumBullet)

import qualified Drasil.SRS as SRS
import qualified Drasil.Sections.ReferenceMaterial as RM


authors :: People
authors = [alex, luthfi]

auths :: Sentence
auths = S $ manyNames authors

chipmunkSRS' :: Document
chipmunkSRS' = mkDoc mkSRS for' chipmunkSysInfo

mkSRS :: DocDesc 
mkSRS = RefSec (RefProg RM.intro [TUnits, tsymb tableOfSymbols, TAandA]) :
  IntroSec (
    IntroProg introSectIntro (short chipmunk)
  [IPurpose docPurpIntro,
   IScope reqScopeIntroP1 reqScopeIntroP2,
   IChar (S "rigid body dynamics") (phrase highSchoolCalculus) (EmptyS),
   IOrgSec docOrgIntro inModel solCharSpec EmptyS]) :
  (map Verbatim [genSysDesc, specSysDesc, req, likelyChanges, unlikelyChanges, offShelfSoln, traceMat, auxConstants])  ++
  (Bibliography : [])
    where tableOfSymbols = [TSPurpose, TypogConvention[Vector Bold], SymbOrder]

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
cpRefDB = rdb [] [] [] [] [] cpCitations -- FIXME: Convert the rest to new chunk types

--FIXME: All named ideas, not just acronyms.

chipUnits :: [UnitDefn]
chipUnits = map unitWrapper [metre, kilogram, second] ++ map unitWrapper [newton, radian]

everything :: ChunkDB
everything = cdb cpSymbolsAll (map nw cpSymbolsAll ++ map nw cpAcronyms) ([] :: [ConceptChunk]) -- FIXME: Fill in Concepts
  chipUnits

chipCode :: CodeSpec
chipCode = codeSpec chipmunkSysInfo []

resourcePath :: String
resourcePath = "../../../datafiles/GamePhysics/"


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

introSectIntro :: Sentence
introSectIntro = foldlSent
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

docPurpIntro :: Sentence
docPurpIntro = docPurpParam chipmunk document programDescription
  (plural game) (map plural detailsAndGoal)

programDescription :: Sentence
programDescription = foldlSent_ [(phrase openSource), getAcc twoD, 
  (phrase CP.rigidBody), (phrase physLib)]

docPurpParam :: (Idea a, NamedIdea b) => a -> b -> Sentence -> Sentence ->
  [Sentence] -> Sentence
docPurpParam progName typeOf progDescrip appOf listOf = foldlSent
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
reqScopeIntroP1, reqScopeIntroP2 :: Sentence

reqScopeIntroP1 = foldlSent_
  [S "the", (phrase physicalSim) `sOf` (getAcc twoD),
  (plural CP.rigidBody), S "acted on by", plural QP.force]
  
reqScopeIntroP2 = foldlSent_ [S "simulate how these",
  (plural CP.rigidBody), S "interact with one another"]

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

-------------------------------------
-- 2.3 : Organization of Documents --
-------------------------------------

docOrgIntro :: Sentence

docOrgIntro = foldlSent
  [S "The", (phrase organization), S "of this", (phrase document), 
  S "follows the", phrase template, S "for an", (getAcc srs), S "for", 
  (phrase sciCompS), S "proposed by", (sSqBrNum 1) `sAnd` (sSqBrNum 2)]

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

genSysDesc :: Section
genSysDesc = assembler chipmunk everything generalSystemDescriptionSect
  [sysContext, userCharacteristicSect, systemConstraintSect]

generalSystemDescriptionSect :: SubSec
generalSystemDescriptionSect = sSubSec generalSystemDescription []

--------------------------
-- 3.1 : System Context --
--------------------------

sysContext :: SubSec
sysContext = sSubSec sysCont [siSTitl, (siCon [sysCtxIntro, sysCtxFig1,
  sysCtxDesc, sysCtxList])]

sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [makeRef sysCtxFig1 +:+ S "shows the" +:+. phrase sysCont,
   S "A circle represents an external entity outside the" +:+ phrase software
   `sC` S "the", phrase user, S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself", (sParen $ short chipmunk) +:+. EmptyS,
   S "Arrows are used to show the data flow between the" +:+ phrase system,
   S "and its" +:+ phrase environment]

sysCtxFig1 :: Contents
sysCtxFig1 = fig (titleize sysCont) (resourcePath ++ "sysctx.png") "sysCtxDiag"

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol
  [S "The interaction between the", phrase product_, S "and the", phrase user,
   S "is through an application programming" +:+. phrase interface,
   S "The responsibilities of the", phrase user, S "and the", phrase system,
   S "are as follows"]

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
sysCtxList = Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

userCharacteristicSect :: SubSec
userCharacteristicSect = sSubSec userCharacteristic [(siCon [userCharIntro])]

userCharIntro :: Contents
userCharIntro = foldlSP
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

specSysDesc :: Section
specSysDesc = specSysDescr physLib [probDesc, solCharSpec]

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

probDesc :: Section
probDescIntro :: Sentence

probDesc = assembler chipmunk everything problemDescriptionSect [termAndDefSect,
  goalStatementSect]

problemDescriptionSect :: SubSec
problemDescriptionSect = sSubSec problemDescription [(siSent [probDescIntro])]

probDescIntro = probDescIntroParam physLib game

probDescIntroParam :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence
probDescIntroParam lib app = foldlSent
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
  (sParen $ makeRef offShelfSoln), S "By creating a simple, lightweight, fast and portable",
  (getAcc twoD), (phrase CP.rigidBody), (phrase lib) `sC` (phrase app),
  S "development will be more accessible to the masses" `sAnd` S "higher quality",
  (plural product_), S "will be produced"]


-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

termAndDefnBullets :: Contents

termAndDefSect :: SubSec
termAndDefSect = sSubSec termAndDef [(siSTitl), (siCon [termAndDefnBullets])]

termAndDefnTerms :: [ConceptChunk]
termAndDefnTerms = [CP.rigidBody, CP.elasticity, CPP.ctrOfMass,
  CP.cartesian, CP.rightHand]

termAndDefnBullets = enumBullet
  (map (\x -> (at_start x) +: EmptyS +:+ (x ^. defn)) termAndDefnTerms)


-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

goalStmtList :: Contents

goalStatementSect :: SubSec
goalStatementSect = sSubSec goalStmt [(siCon [goalStmtList])]

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

goalStmt1, goalStmt2, goalStmt3, goalStmt4 :: [Sentence]

goalStmt1 = goalStatementStruct (plural physicalProperty)
  (take 2 inputSymbols) (plural QP.force) (S "applied on")
  (take 2 outputSymbols) CP.rigidBody
  (S "their new") EmptyS

goalStmt2 = goalStatementStruct (plural physicalProperty)
  (drop 3 $ take 5 inputSymbols) (plural QP.force) (S "applied on")
  (drop 3 $ take 5 inputSymbols) CP.rigidBody
  (S "their new") EmptyS

goalStmt3 = goalStatementStruct EmptyS
  (take 2 inputSymbols) EmptyS (S "of")
  (take 0 inputSymbols) CP.rigidBody
  (S "if any of them will collide with one another") EmptyS

goalStatement4Inputs :: [UnitalChunk]
goalStatement4Inputs = [QP.position, QM.orientation, QP.linearVelocity, 
  QP.angularVelocity]

goalStmt4 = goalStatementStruct (plural physicalProperty)
  (goalStatement4Inputs) --fixme input symbols
  EmptyS (S "of")
  (goalStatement4Inputs) --fixme input symbols
  CP.rigidBody (S "the new") (S "of the" +:+ (plural CP.rigidBody) +:+
  S "that have undergone a" +:+ (phrase CP.collision))

goalStmtList' :: [Sentence]
goalStmtList' = map (foldlSent) [goalStmt1, goalStmt2, goalStmt3,
  goalStmt4]

goalStmtList = enumSimple 1 (getAcc goalStmt) goalStmtList'

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

solCharSpec :: Section
solCharSpec = assembler chipmunk everything scsSect [assumSec, tModSec, genDefSec,
  iModSec, dataDefSec, dataConSec]

assumSec, tModSec, genDefSec, iModSec, dataDefSec, dataConSec, scsSect :: SubSec
scsSect = sSubSec solutionCharSpec []
assumSec = (sSubSec assumption [(siCon [assumpList])])
tModSec = (sSubSec thModel [(siTMod cpTMods)])
genDefSec = (sSubSec genDefn [])
iModSec = (sSubSec inModel [(siIMod iModels)])
dataDefSec = (sSubSec dataDefn [(siSent [dataDefnIntro]), (siDDef cpDDefs)])
dataConSec = (sSubSec dataConst [(siUQI cpInputConstraints),
  (siUQO cpOutputConstraints)])


-------------------------
-- 4.2.1 : Assumptions --
-------------------------

assumpList :: Contents

assumpListAssum1, assumpListAssum2, assumpListAssum3, assumpListAssum4,
  assumpListAssum5, assumpListAssum6, assumpListAssum7 :: [Sentence]

allObject :: Sentence -> [Sentence]
allObject thing = [S "All objects are", thing]

thereNo :: [Sentence] -> [Sentence]
thereNo [x]      = [S "There is no", x, S "involved throughout the", 
  (phrase simulation)]
thereNo l        = [S "There are no", foldlList l, S "involved throughout the", 
  (phrase simulation)]

assumpListAssum1 = allObject (plural CP.rigidBody)
assumpListAssum2 = allObject (getAcc twoD)
assumpListAssum3 = [S "The library uses a", (phrase CP.cartesian)]
assumpListAssum4 = [S "The axes are defined using",
  (phrase CP.rightHand)]
assumpListAssum5 = [S "All", (plural CP.rigidBody),
  (plural CP.collision), S "are vertex-to-edge",
  (plural CP.collision)]

assumpListAssum6 = thereNo [(phrase CP.damping)]
assumpListAssum7 = thereNo [(plural CM.constraint), (plural CP.joint)]

assumpList = enumSimple 1 (getAcc assumption) $ map foldlSent
  [assumpListAssum1, assumpListAssum2, assumpListAssum3, assumpListAssum4,
  assumpListAssum5, assumpListAssum6, assumpListAssum7]

assumpListS :: [[Sentence]]
assumpListS = [assumpListAssum1, assumpListAssum2, assumpListAssum3,
  assumpListAssum4, assumpListAssum5, assumpListAssum6, assumpListAssum7]


--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

genDefnIntro :: Contents
-- s4_2_3_GDefs :: [Contents]

genDefnIntro = foldlSP
  [S "This", (phrase section_), S "collects the", (plural CM.law) `sAnd` 
  (plural CM.equation), S "that will be used in deriving the", 
  (plural dataDefn) `sC` S "which in turn will be used to build the", 
  (plural inModel)]

-- GDefs not yet implemented --
{-
s4_2_3_GDefs :: [Contents]
s4_2_3_GDefs = map (Definition . General) gDefs)
-}

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------

dataDefnIntro :: Sentence
dataDefnIntro = foldlSent [S "The", (phrase CPP.dimension)
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

req :: Section
req = reqF [funcReq, nonFuncReq]

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

funcReq :: Section
funcReqList :: Contents

funcReq = SRS.funcReq [funcReqList] []

funcReqReq1, funcReqReq2, funcReqReq3, funcReqReq4, funcReqReq5, funcReqReq6,
  funcReqReq7, funcReqReq8 :: Sentence

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
funcReqReq1 = foldlSent [S "Create a", (phrase CP.space), S "for all of the",
  (plural CP.rigidBody), S "in the", (phrase physicalSim), 
  S "to interact in"]

funcReqReq2 = foldlSent [S "Input the initial",
  (plural QPP.mass) `sC` (plural QP.velocity) `sC`
  (plural QM.orientation) `sC` (plural QP.angularVelocity),
  S "of" `sC` S "and", (plural QP.force), S "applied on",
  (plural CP.rigidBody)]

funcReqReq3 = foldlSent [S "Input the", (phrase CM.surface),
  (plural property), S "of the", plural body, S "such as",
  (phrase CP.friction) `sOr` (phrase CP.elasticity)]

funcReqReq4 = foldlSent [S "Verify that the", plural input_,
  S "satisfy the required", plural physicalConstraint, S "from",
  (makeRef solCharSpec)]

funcReqReq5 = requirementS (QP.position) (QP.velocity)
  (S "acted upon by a" +:+ (phrase QP.force))

funcReqReq6 = requirementS' (QM.orientation) (QP.angularVelocity)

funcReqReq7 = foldlSent [S "Determine if any of the",
  (plural CP.rigidBody), S "in the", (phrase CP.space),
  S "have collided"]

funcReqReq8 = requirementS (QP.position) (QP.velocity)
  (S "that have undergone a" +:+ (phrase CP.collision))

-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
funcReqListS :: [Sentence]
funcReqListS = [funcReqReq1, funcReqReq2, funcReqReq3, funcReqReq4, funcReqReq5,
  funcReqReq6, funcReqReq7, funcReqReq8]

funcReqList = enumSimple 1 (getAcc requirement) funcReqListS

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

nonFuncReq :: Section
nonFuncReqIntro :: Contents

nonFuncReq = SRS.nonfuncReq [nonFuncReqIntro] []

chpmnkPriorityNFReqs :: [ConceptChunk]
chpmnkPriorityNFReqs = [correctness, understandability, portability,
  reliability, maintainability]

nonFuncReqIntro = foldlSP
  [(titleize' game), S "are resource intensive, so", phrase performance,
  S "is a high" +:+. phrase priority, S "Other", plural nonfunctionalRequirement,
  S "that are a", phrase priority +: S "are",
  foldlList (map phrase chpmnkPriorityNFReqs)]

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------



-----------------------------------------
-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --
-----------------------------------------

offShelfSoln :: Section
offShelfSolnIntro, offShelfSoln2dAltList, offShelfSoln3dAltIntro,
  offShelfSoln3dAltList :: Contents

offShelfSoln = SRS.offShelfSol [offShelfSolnIntro, offShelfSoln2dAltList,
  offShelfSoln3dAltIntro, offShelfSoln3dAltList] []

offShelfSolnIntro = offShelfSolnIntroParam probDesc physLib

offShelfSolnIntroParam :: NamedIdea n => Section -> n -> Contents
offShelfSolnIntroParam problmDescSec lib = Paragraph $ foldlSentCol
  [S "As mentioned in", (makeRef problmDescSec) `sC`
  S "there already exist free", (phrase openSource), (phrase game) +:+.
  (plural lib), S "Similar", (getAcc twoD), (plural lib), S "are"]

offShelfSoln2dAltList = enumBullet [(S "Box2D: http://box2d.org/"),
  (S "Nape Physics Engine: http://napephys.com/")]

offShelfSoln3dAltIntro = Paragraph $ foldl (+:+) (EmptyS) [S "Free",
  (phrase openSource), S "3D", (phrase game), (plural physLib), S "include:"]

offShelfSoln3dAltList = enumBullet [
  (S "Bullet: http://bulletphysics.org/"),
  (S "Open Dynamics Engine: http://www.ode.org/"),
  (S "Newton Game Dynamics: http://newtondynamics.com/")]

-----------------------------------------------------
-- SECTION 8 : Traceability Matrices and Graph    --
-----------------------------------------------------

traceMat :: Section
traceMat = traceMGF [traceMatTabReqGoalOther, traceMatTabAssump, traceMatTabDefnModel]
  traceMatTabDescs [traceMatTabReqGoalOther, traceMatTabAssump, traceMatTabDefnModel] []
--s8 = SRS.traceyMandG [s8_intro1, s8_table1, s8_table2, s8_table3] []

traceMatTabDescs, traceMatTabReqGoalOtherDesc, traceMatTabAssumpDesc,
  traceMatTabDefnModelDesc :: [Sentence]
traceMatTabDescs = map (foldlList) [traceMatTabReqGoalOtherDesc,
  traceMatTabAssumpDesc, traceMatTabDefnModelDesc]

traceMatTabReqGoalOtherDesc = [(plural goalStmt), (plural requirement),
  (plural inModel), (plural datumConstraint) +:+. S "with each other"]

traceMatTabAssumpDesc = [(plural thModel), (plural genDefn), (plural dataDefn),
  (plural inModel), S "on the" +:+. plural assumption]

traceMatTabDefnModelDesc = [(plural thModel), (plural genDefn), (plural dataDefn),
  (plural inModel) +:+ S "on each other"]

-- these look like they could be generated by the sections above
traceMatInstaModel, traceMatAssump, traceMatFuncReq, traceMatData,
  traceMatGoalStmt, traceMatTheoryModel, traceMatGenDef, traceMatDataDef,
  traceMatLikelyChg :: [String]

traceMatInstaModelRef, traceMatAssumpRef, traceMatFuncReqRef, traceMatGoalStmtRef,
  traceMatTheoryModelRef, traceMatGenDefRef, traceMatDataDefRef,
  traceMatLikelyChgRef, traceMatDataRef :: [Sentence]

traceMatInstaModel = ["IM1", "IM2", "IM3"]
traceMatInstaModelRef = map (refFromType Theory) iModels

traceMatTheoryModel = ["T1", "T2", "T3", "T4", "T5"]
traceMatTheoryModelRef = map (refFromType Theory) cpTMods

traceMatDataDef = ["DD1","DD2","DD3","DD4","DD5","DD6","DD7","DD8"]
traceMatDataDefRef = map (refFromType Data) cpDDefs

traceMatAssump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7"]
traceMatAssumpRef = makeListRef assumpListS probDesc

traceMatFuncReq =  ["R1","R2","R3", "R4", "R5", "R6", "R7", "R8"]
traceMatFuncReqRef = makeListRef funcReqListS funcReq

traceMatData = ["Data Constraints"]
traceMatDataRef = [makeRef solCharSpec]

traceMatGoalStmt = ["GS1", "GS2", "GS3", "GS4"]
traceMatGoalStmtRef = makeListRef goalStmtList' probDesc

traceMatGenDef = ["GD1", "GD2", "GD3", "GD4", "GD5", "GD6", "GD7"]
traceMatGenDefRef = makeListRef traceMatGenDef solCharSpec

traceMatLikelyChg = ["LC1", "LC2", "LC3", "LC4"]
traceMatLikelyChgRef = makeListRef likelyChangesList' likelyChanges


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

traceMatTabReqGoalOther :: Contents
traceMatTabReqGoalOther = Table (EmptyS:(traceMatTabReqGoalOtherRowHead))
  (makeTMatrix traceMatTabReqGoalOtherColHead traceMatTabReqGoalOtherCol
  traceMatTabReqGoalOtherRow)
  (showingCxnBw (traceyMatrix) (titleize' requirement +:+ sParen (makeRef req)
  `sC` (titleize' goalStmt) +:+ sParen (makeRef probDesc) `sAnd` S "Other" +:+
  titleize' item)) True "TraceyReqGoalsOther"

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

traceMatTabAssump :: Contents
traceMatTabAssump = Table (EmptyS:traceMatTabAssumpRowHead)
  (makeTMatrix traceMatTabAssumpColHead traceMatTabAssumpCol' traceMatTabAssumpRow)
  (showingCxnBw (traceyMatrix) (titleize' assumption +:+ sParen (makeRef probDesc)
  `sAnd` S "Other" +:+ titleize' item)) True "TraceyAssumpsOther"


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

traceMatTabDefnModel :: Contents
traceMatTabDefnModel = Table (EmptyS:traceMatTabDefnModelRowHead)
  (makeTMatrix traceMatTabDefnModelColHead traceMatTabDefnModelCol
  traceMatTabDefnModelRow) (showingCxnBw (traceyMatrix) (titleize' item `sAnd`
  S "Other" +:+ titleize' section_)) True "TraceyItemsSecs"

-----------------------------------
-- VALUES OF AUXILIARY CONSTANTS --
-----------------------------------

auxConstants :: Section
auxConstants = valsOfAuxConstantsF chipmunk []

----------------
-- REFERENCES --
----------------
--}
-- To be added --
