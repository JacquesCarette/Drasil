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


authors :: People
authors = [alex, luthfi]

auths :: Sentence
auths = S $ manyNames authors

chipmunkSRS' :: Document
chipmunkSRS' = mkDoc mkSRS for' chipmunkSysInfo

mkSRS :: DocDesc 
mkSRS = RefSec (RefProg RM.intro [TUnits, tsymb tableOfSymbols, TAandA]) :
  IntroSec (
    IntroProg para1_s2_intro (short chipmunk) 
  [IPurpose (para1_s2_1_intro), 
   IScope s2_2_intro_p1 s2_2_intro_p2, 
   IChar (S "rigid body dynamics") (phrase highSchoolCalculus) (EmptyS), 
   IOrgSec s2_4_intro inModel s4_2 EmptyS]) :
  (map Verbatim [s3, s4, s5, s6, s7, s8, s9])  ++ 
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

para1_s2_intro :: Sentence
para1_s2_intro = foldlSent
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

para1_s2_1_intro :: Sentence
para1_s2_1_intro = para1_s2_1_param chipmunk document programDescription 
  (plural game) (map plural detailsAndGoal)

programDescription :: Sentence
programDescription = foldlSent_ [(phrase openSource), getAcc twoD, 
  (phrase CP.rigidBody), (phrase physLib)]

para1_s2_1_param :: (Idea a, NamedIdea b) => a -> b -> Sentence -> Sentence ->
  [Sentence] -> Sentence
para1_s2_1_param progName typeOf progDescrip appOf listOf = foldlSent 
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
s2_2_intro_p1, s2_2_intro_p2 :: Sentence

s2_2_intro_p1 = foldlSent_
  [S "the", (phrase physicalSim) `sOf` (getAcc twoD), 
  (plural CP.rigidBody), S "acted on by", plural QP.force]
  
s2_2_intro_p2 = foldlSent_ [S "simulate how these", 
  (plural CP.rigidBody), S "interact with one another"]

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

-------------------------------------
-- 2.3 : Organization of Documents --
-------------------------------------

s2_4_intro :: Sentence

s2_4_intro = foldlSent 
  [S "The", (phrase organization), S "of this", (phrase document), 
  S "follows the", phrase template, S "for an", (getAcc srs), S "for", 
  (phrase sciCompS), S "proposed by", (sSqBrNum 1) `sAnd` (sSqBrNum 2)]

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

s3 :: Section
s3 = assembler chipmunk everything generalSystemDescriptionSect
  [userCharacteristicSect, systemConstraintSect]

generalSystemDescriptionSect :: SubSec
generalSystemDescriptionSect = sSubSec generalSystemDescription []

--------------------------------
-- 3.1 : User Characteristics --
--------------------------------

userCharacteristicSect :: SubSec
userCharacteristicSect = sSubSec userCharacteristic [(siCon [s3_1_intro])]

s3_1_intro :: Contents
s3_1_intro = foldlSP
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

s4 :: Section
s4 = specSysDescr physLib [s4_1, s4_2]

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

s4_1 :: Section
s4_1_intro :: Sentence

s4_1 = assembler chipmunk everything problemDescriptionSect [termAndDefSect, 
  goalStatementSect]

problemDescriptionSect :: SubSec
problemDescriptionSect = sSubSec problemDescription [(siSent [s4_1_intro])]

s4_1_intro = s4_1_intro_param physLib game

s4_1_intro_param :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence
s4_1_intro_param lib app = foldlSent 
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
  (sParen $ makeRef s7), S "By creating a simple, lightweight, fast and portable",
  (getAcc twoD), (phrase CP.rigidBody), (phrase lib) `sC` (phrase app),
  S "development will be more accessible to the masses" `sAnd` S "higher quality",
  (plural product_), S "will be produced"]


-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

s4_1_1_bullets :: Contents

termAndDefSect :: SubSec
termAndDefSect = sSubSec termAndDef [(siSTitl), (siCon [s4_1_1_bullets])]

s4_1_1_terms :: [ConceptChunk]
s4_1_1_terms = [CP.rigidBody, CP.elasticity, CPP.ctrOfMass, 
  CP.cartesian, CP.rightHand]

s4_1_1_bullets = enumBullet 
  (map (\x -> (at_start x) +: EmptyS +:+ (x ^. defn)) s4_1_1_terms)


-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

s4_1_2_list :: Contents

goalStatementSect :: SubSec
goalStatementSect = sSubSec goalStmt [(siCon [s4_1_2_list])]

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

s4_1_2_stmt1 = goalStatementStruct (plural physicalProperty) 
  (take 2 inputSymbols) (plural QP.force) (S "applied on")
  (take 2 outputSymbols) CP.rigidBody 
  (S "their new") EmptyS

s4_1_2_stmt2 = goalStatementStruct (plural physicalProperty) 
  (drop 3 $ take 5 inputSymbols) (plural QP.force) (S "applied on")
  (drop 3 $ take 5 inputSymbols) CP.rigidBody 
  (S "their new") EmptyS

s4_1_2_stmt3 = goalStatementStruct EmptyS
  (take 2 inputSymbols) EmptyS (S "of")
  (take 0 inputSymbols) CP.rigidBody
  (S "if any of them will collide with one another") EmptyS

goalStatement4Inputs :: [UnitalChunk]
goalStatement4Inputs = [QP.position, QM.orientation, QP.linearVelocity, 
  QP.angularVelocity]

s4_1_2_stmt4 = goalStatementStruct (plural physicalProperty)
  (goalStatement4Inputs) --fixme input symbols
  EmptyS (S "of")
  (goalStatement4Inputs) --fixme input symbols
  CP.rigidBody (S "the new") (S "of the" +:+ (plural CP.rigidBody) +:+ 
  S "that have undergone a" +:+ (phrase CP.collision))

s4_1_2_stmt1, s4_1_2_stmt2, s4_1_2_stmt3, s4_1_2_stmt4 :: [Sentence]

s4_1_2_list' :: [Sentence]
s4_1_2_list' = map (foldlSent) [s4_1_2_stmt1, s4_1_2_stmt2, s4_1_2_stmt3, 
  s4_1_2_stmt4]

s4_1_2_list = enumSimple 1 (getAcc goalStmt) s4_1_2_list'

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

s4_2 :: Section
s4_2 = assembler chipmunk everything scsSect [assumSec, tModSec, genDefSec,
  iModSec, dataDefSec, dataConSec]

assumSec, tModSec, genDefSec, iModSec, dataDefSec, dataConSec, scsSect :: SubSec
scsSect = sSubSec solutionCharSpec []
assumSec = (sSubSec assumption [(siCon [s4_2_1_list])])
tModSec = (sSubSec thModel [(siTMod cpTMods)])
genDefSec = (sSubSec genDefn [])
iModSec = (sSubSec inModel [(siIMod iModels)])
dataDefSec = (sSubSec dataDefn [(siSent [s4_2_4_intro]), (siDDef cpDDefs)])
dataConSec = (sSubSec dataConst [(siUQI cpInputConstraints), (siUQO cpOutputConstraints)])


-------------------------
-- 4.2.1 : Assumptions --
-------------------------

s4_2_1_list :: Contents

s4_2_1_assum1, s4_2_1_assum2, s4_2_1_assum3, s4_2_1_assum4, s4_2_1_assum5, 
  s4_2_1_assum6, s4_2_1_assum7 :: [Sentence]

allObject :: Sentence -> [Sentence]
allObject thing = [S "All objects are", thing]

thereNo :: [Sentence] -> [Sentence]
thereNo [x]      = [S "There is no", x, S "involved throughout the", 
  (phrase simulation)]
thereNo l        = [S "There are no", foldlList l, S "involved throughout the", 
  (phrase simulation)]

s4_2_1_assum1 = allObject (plural CP.rigidBody)
s4_2_1_assum2 = allObject (getAcc twoD)
s4_2_1_assum3 = [S "The library uses a", (phrase CP.cartesian)]
s4_2_1_assum4 = [S "The axes are defined using", 
  (phrase CP.rightHand)]
s4_2_1_assum5 = [S "All", (plural CP.rigidBody), 
  (plural CP.collision), S "are vertex-to-edge", 
  (plural CP.collision)]

s4_2_1_assum6 = thereNo [(phrase CP.damping)]
s4_2_1_assum7 = thereNo [(plural CM.constraint), (plural CP.joint)]

s4_2_1_list = enumSimple 1 (getAcc assumption) $ map (foldlSent) 
  [s4_2_1_assum1, s4_2_1_assum2, s4_2_1_assum3, s4_2_1_assum4, s4_2_1_assum5, 
  s4_2_1_assum6, s4_2_1_assum7]

s4_2_1_list_a :: [[Sentence]]
s4_2_1_list_a = [s4_2_1_assum1, s4_2_1_assum2, s4_2_1_assum3, s4_2_1_assum4,
  s4_2_1_assum5, s4_2_1_assum6, s4_2_1_assum7]


--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

s4_2_3_intro :: Contents
-- s4_2_3_GDefs :: [Contents]

s4_2_3_intro = foldlSP 
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

s4_2_4_intro :: Sentence
s4_2_4_intro = foldlSent [S "The", (phrase CPP.dimension)
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

s5 :: Section
s5 = reqF [s5_1, s5_2]

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

s5_1 :: Section
s5_1_list :: Contents

s5_1 = SRS.funcReq [s5_1_list] []

s5_1_req1, s5_1_req2, s5_1_req3, s5_1_req4, s5_1_req5, s5_1_req6,
  s5_1_req7, s5_1_req8 :: Sentence

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
s5_1_req1 = foldlSent [S "Create a", (phrase CP.space), S "for all of the",
  (plural CP.rigidBody), S "in the", (phrase physicalSim), 
  S "to interact in"]

s5_1_req2 = foldlSent [S "Input the initial", 
  (plural QPP.mass) `sC` (plural QP.velocity) `sC` 
  (plural QM.orientation) `sC` (plural QP.angularVelocity), 
  S "of" `sC` S "and", (plural QP.force), S "applied on", 
  (plural CP.rigidBody)]

s5_1_req3 = foldlSent [S "Input the", (phrase CM.surface), 
  (plural property), S "of the", plural body, S "such as",
  (phrase CP.friction) `sOr` (phrase CP.elasticity)]

s5_1_req4 = foldlSent [S "Verify that the", plural input_,
  S "satisfy the required", plural physicalConstraint, S "from", 
  (makeRef s4_2)]

s5_1_req5 = requirementS (QP.position) (QP.velocity) 
  (S "acted upon by a" +:+ (phrase QP.force))

s5_1_req6 = requirementS' (QM.orientation) (QP.angularVelocity)

s5_1_req7 = foldlSent [S "Determine if any of the", 
  (plural CP.rigidBody), S "in the", (phrase CP.space), 
  S "have collided"]

s5_1_req8 = requirementS (QP.position) (QP.velocity) 
  (S "that have undergone a" +:+ (phrase CP.collision))

-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
s5_1_list' :: [Sentence]
s5_1_list' = [s5_1_req1, s5_1_req2, s5_1_req3, s5_1_req4, s5_1_req5, s5_1_req6,
  s5_1_req7, s5_1_req8]

s5_1_list = enumSimple 1 (getAcc requirement) s5_1_list'

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

s5_2 :: Section
s5_2_intro :: Contents

s5_2 = SRS.nonfuncReq [s5_2_intro] []

chpmnkPriorityNFReqs :: [ConceptChunk]
chpmnkPriorityNFReqs = [correctness, understandability, portability,
  reliability, maintainability]

s5_2_intro = foldlSP 
  [(titleize' game), S "are resource intensive, so", phrase performance,
  S "is a high" +:+. phrase priority, S "Other", plural nonfunctionalRequirement,
  S "that are a", phrase priority +: S "are", 
  foldlList (map phrase chpmnkPriorityNFReqs)]

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

s6 :: Section
s6_intro, s6_list :: Contents

s6 = SRS.likeChg [s6_intro, s6_list] []

s6_intro = foldlSP [S "This", (phrase section_), S "lists the", 
  (plural likelyChg), S "to be made to the", (phrase physics), (phrase game), 
  (phrase library)]

s6_likelyChg_stmt1, s6_likelyChg_stmt2, s6_likelyChg_stmt3, 
  s6_likelyChg_stmt4 :: Sentence

--these statements look like they could be parametrized
s6_likelyChg_stmt1 = (S "internal" +:+ (getAcc CM.ode) :+: 
  S "-solving" +:+ phrase algorithm +:+ S "used by the" +:+
  (phrase library)) `maybeChanged` (S "in the future")

s6_likelyChg_stmt2 = (phrase library) `maybeExpanded`
  (S "to deal with edge-to-edge and vertex-to-vertex" +:+ (plural CP.collision))

s6_likelyChg_stmt3 = (phrase library) `maybeExpanded` (
  S "to include motion with" +:+ (phrase CP.damping))

s6_likelyChg_stmt4 = (phrase library) `maybeExpanded` (S "to include" +:+ 
  (plural CP.joint) `sAnd` (plural CM.constraint))

s6_list' :: [Sentence]
s6_list' = [s6_likelyChg_stmt1, s6_likelyChg_stmt2, s6_likelyChg_stmt3,
  s6_likelyChg_stmt4]

s6_list = enumSimple 1 (getAcc likelyChg) s6_list'

-----------------------------------------
-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --
-----------------------------------------

s7 :: Section
s7_intro, s7_2dlist, s7_mid, s7_3dlist :: Contents

s7 = SRS.offShelfSol [s7_intro, s7_2dlist, s7_mid, s7_3dlist] []

s7_intro = s7_intro_param s4_1 physLib

s7_intro_param :: NamedIdea n => Section -> n -> Contents
s7_intro_param problmDescSec lib = Paragraph $ foldlSentCol 
  [S "As mentioned in", (makeRef problmDescSec) `sC`
  S "there already exist free", (phrase openSource), (phrase game) +:+.
  (plural lib), S "Similar", (getAcc twoD), (plural lib), S "are"]

s7_2dlist = enumBullet [(S "Box2D: http://box2d.org/"),
  (S "Nape Physics Engine: http://napephys.com/")]

s7_mid = Paragraph $ foldl (+:+) (EmptyS) [S "Free", (phrase openSource), 
        S "3D", (phrase game), (plural physLib), S "include:"]

s7_3dlist = enumBullet [
  (S "Bullet: http://bulletphysics.org/"),
  (S "Open Dynamics Engine: http://www.ode.org/"),
  (S "Newton Game Dynamics: http://newtondynamics.com/")]

-----------------------------------------------------
-- SECTION 8 : Traceability Matrices and Graph    --
-----------------------------------------------------

s8 :: Section
s8 = traceMGF [s8_table1, s8_table2, s8_table3] s8_traces 
  [s8_table1, s8_table2, s8_table3] []
--s8 = SRS.traceyMandG [s8_intro1, s8_table1, s8_table2, s8_table3] []

s8_traces, s8_trace1, s8_trace2, s8_trace3 :: [Sentence]
s8_traces = map (foldlList) [s8_trace1, s8_trace2, s8_trace3]

s8_trace1 = [(plural goalStmt), (plural requirement), (plural inModel), 
  (plural datumConstraint) +:+. S "with each other"]

s8_trace2 = [(plural thModel), (plural genDefn), (plural dataDefn), 
  (plural inModel), S "on the" +:+. plural assumption]

s8_trace3 = [(plural thModel), (plural genDefn), (plural dataDefn), 
  (plural inModel) +:+ S "on each other"]

-- these look like they could be generated by the sections above
s8_instaModel, s8_assump, s8_funcReq, s8_data, s8_goalstmt, s8_theoryModel, 
  s8_genDef, s8_dataDef, s8_likelyChg :: [String]

s8_instaModelRef, s8_assumpRef, s8_funcReqRef, s8_goalstmtRef, 
  s8_theoryModelRef, s8_genDefRef, s8_dataDefRef, s8_likelyChgRef, 
  s8_dataRef :: [Sentence]

s8_instaModel = ["IM1", "IM2", "IM3"]
s8_instaModelRef = map (refFromType Theory) iModels

s8_theoryModel = ["T1", "T2", "T3", "T4", "T5"]
s8_theoryModelRef = map (refFromType Theory) cpTMods

s8_dataDef = ["DD1","DD2","DD3","DD4","DD5","DD6","DD7","DD8"]
s8_dataDefRef = map (refFromType Data) cpDDefs

s8_assump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7"]
s8_assumpRef = makeListRef s4_2_1_list_a s4_1

s8_funcReq =  ["R1","R2","R3", "R4", "R5", "R6", "R7", "R8"]
s8_funcReqRef = makeListRef s5_1_list' s5_1

s8_data = ["Data Constraints"]
s8_dataRef = [makeRef s4_2]

s8_goalstmt = ["GS1", "GS2", "GS3", "GS4"]
s8_goalstmtRef = makeListRef s4_1_2_list' s4_1

s8_genDef = ["GD1", "GD2", "GD3", "GD4", "GD5", "GD6", "GD7"]
s8_genDefRef = makeListRef s8_genDef s4_2

s8_likelyChg = ["LC1", "LC2", "LC3", "LC4"]
s8_likelyChgRef = makeListRef s6_list' s6


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

s8_row_header_t1, s8_col_header_t1 :: [Sentence]
s8_row_header_t1 = zipWith itemRefToSent s8_row_t1 (s8_instaModelRef ++ 
  (take 3 s8_funcReqRef) ++ s8_dataRef)
s8_col_header_t1 = zipWith itemRefToSent 
  (s8_goalstmt ++ s8_funcReq) (s8_goalstmtRef ++ s8_funcReqRef)

s8_row_t1 :: [String]
s8_row_t1 = s8_instaModel ++ ["R1","R4","R7"] ++ s8_data

s8_columns_t1 :: [[String]]
s8_columns_t1 = [gS1_t1, gS2_t1, gS3_t1, gS4_t1, r1_t1, r2_t1, r3_t1, r4_t1, 
  r5_t1, r6_t1, r7_t1, r8_t1]

s8_table1 :: Contents
s8_table1 = Table (EmptyS:(s8_row_header_t1))
  (makeTMatrix s8_col_header_t1 s8_columns_t1 s8_row_t1)
  (showingCxnBw (traceyMatrix) (titleize' requirement +:+ sParen (makeRef s5)
  `sC` (titleize' goalStmt) +:+ sParen (makeRef s4_1) `sAnd` S "Other" +:+
  titleize' item)) True "TraceyReqGoalsOther"

s8_columns_t2 :: [[String]]
s8_columns_t2 = [t1_t2, t2_t2, t3_t2, t4_t2, t5_t2, gD1_t2, gD2_t2, gD3_t2,
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

s8_row_t2, s8_cols_t2 :: [String]
s8_row_t2 = s8_assump

s8_cols_t2 = (s8_theoryModel ++ s8_genDef ++ s8_dataDef ++ s8_instaModel ++
  s8_likelyChg) 
s8_cols_ref_t2 :: [Sentence]
s8_cols_ref_t2 = (s8_theoryModelRef ++ s8_genDefRef ++ s8_dataDefRef ++ 
  s8_instaModelRef ++ s8_likelyChgRef)

s8_row_header_t2, s8_col_header_t2 :: [Sentence]
s8_row_header_t2 = zipWith itemRefToSent (s8_row_t2) (s8_assumpRef)
s8_col_header_t2 = zipWith itemRefToSent (s8_cols_t2) (s8_cols_ref_t2)

s8_table2 :: Contents
s8_table2 = Table (EmptyS:s8_row_header_t2)
  (makeTMatrix s8_col_header_t2 s8_columns_t2 s8_row_t2) 
  (showingCxnBw (traceyMatrix) (titleize' assumption +:+ sParen (makeRef s4_1) 
  `sAnd` S "Other" +:+ titleize' item)) True "TraceyAssumpsOther"


s8_columns_t3 :: [[String]]
s8_columns_t3 = [t1_t3, t2_t3, t3_t3, t4_t3, t5_t3, gD1_t3, gD2_t3, gD3_t3, 
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

s8_row_t3 :: [String]
s8_row_ref_t3 :: [Sentence]
s8_row_t3 = s8_theoryModel ++ s8_genDef ++ s8_dataDef ++ s8_instaModel
s8_row_ref_t3 = s8_theoryModelRef ++ s8_genDefRef ++ s8_dataDefRef ++ 
  s8_instaModelRef

s8_col_header_t3, s8_row_header_t3 :: [Sentence]
s8_col_header_t3 = zipWith itemRefToSent (s8_row_t3) (s8_row_ref_t3)
s8_row_header_t3 = s8_col_header_t3

s8_table3 :: Contents
s8_table3 = Table (EmptyS:s8_row_header_t3)
  (makeTMatrix s8_col_header_t3 s8_columns_t3 s8_row_t3)
  (showingCxnBw (traceyMatrix) (titleize' item `sAnd` 
  S "Other" +:+ titleize' section_)) True "TraceyItemsSecs"

-----------------------------------
-- VALUES OF AUXILIARY CONSTANTS --
-----------------------------------

s9 :: Section
s9 = valsOfAuxConstantsF chipmunk []

----------------
-- REFERENCES --
----------------
--}
-- To be added --
