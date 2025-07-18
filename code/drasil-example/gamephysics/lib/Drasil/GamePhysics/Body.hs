module Drasil.GamePhysics.Body where

import Language.Drasil hiding (organization, section)
import Drasil.SRSDocument
import qualified Drasil.DocLang.SRS as SRS
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation as Doc (assumption, concept,
  condition, consumer, endUser, environment, game, guide, input_, interface,
  object, physical, physicalSim, physics, problem, product_, project,
  quantity, realtime, section_, simulation, software, softwareSys,
  srsDomains, system, systemConstraint, sysCont, task, user, doccon, doccon',
  property, problemDescription)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.TheoryConcepts as Doc (dataDefn, inModel)
import Data.Drasil.Concepts.Education (frstYr, highSchoolCalculus,
  highSchoolPhysics, educon)
import Data.Drasil.Concepts.Software (physLib, softwarecon)
import Data.Drasil.People (alex, luthfi, olu)
import Data.Drasil.SI_Units (siUnits)
import Data.Drasil.Software.Products (openSource, prodtcon, videoGame)

import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (ctrOfMass, dimension)
import qualified Data.Drasil.Concepts.Physics as CP (elasticity, physicCon,
  physicCon', rigidBody, collision, damping)
import qualified Data.Drasil.Concepts.Math as CM (cartesian, equation, law,
  mathcon, mathcon', rightHand, line, point)
import qualified Data.Drasil.Quantities.Physics as QP (force, time)

import Drasil.GamePhysics.Assumptions (assumptions)
import Drasil.GamePhysics.Changes (likelyChgs, unlikelyChgs)
import Drasil.GamePhysics.Concepts (acronyms, threeD, twoD, centreMass)
import Drasil.GamePhysics.DataDefs (dataDefs)
import Drasil.GamePhysics.Goals (goals)
import Drasil.GamePhysics.IMods (iMods, instModIntro)
import Drasil.GamePhysics.MetaConcepts (progName)
import Drasil.GamePhysics.References (citations, uriReferences)
import Drasil.GamePhysics.Requirements (funcReqs, nonfuncReqs, pymunk)
import Drasil.GamePhysics.TMods (tMods)
import Drasil.GamePhysics.Unitals (symbolsAll, outputConstraints,
  inputSymbols, outputSymbols, inputConstraints, defSymbols)
import Drasil.GamePhysics.GenDefs (generalDefns)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize short) si

fullSI :: System
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

resourcePath :: String
resourcePath = "../../../../datafiles/gamephysics/"

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $ RefProg intro [TUnits, tsymb tableOfSymbols, TAandA abbreviationsList],
  IntroSec $ IntroProg para1_introduction_intro (short progName)
  [IPurpose $ purpDoc progName Verbose,
   IScope scope,
   IChar [] [S "rigid body dynamics", phrase highSchoolCalculus] [],
   IOrgSec inModel (SRS.inModel [] []) EmptyS],
   GSDSec $ GSDProg [
    SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList],
    UsrChars [userCharacteristicsIntro], SystCons [] []],
   SSDSec $ SSDProg
      [ SSDProblem $ PDProg probDescIntro []
        [ TermsAndDefs Nothing terms
        , Goals [S "the kinematic" +:+ plural property `sC` S "and" +:+ plural QP.force +:+
                 sParen (S "including any" +:+ phrase CP.collision +:+ plural QP.force) +:+
                 S "applied on a set of" +:+ plural CP.rigidBody]]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inputConstraints
        , CorrSolnPpties outputConstraints []
        ]
      ],
    ReqrmntSec $ ReqsProg [
      FReqsSub' [],
      NonFReqsSub
    ],
    LCsSec,
    UCsSec,
    OffShelfSolnsSec $ OffShelfSolnsProg offShelfSols,
    TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
    AuxConstntSec $ AuxConsProg progName [],
    Bibliography]
      where tableOfSymbols = [TSPurpose, TypogConvention[Vector Bold], SymbOrder, VectorUnits]

si :: System
si = SI {
  _sys         = progName,
  _kind        = Doc.srs,
  _authors     = [alex, luthfi, olu],
  _purpose     = [purp],
  _background  = [],
  _motivation  = [],
  _scope       = [],
  -- FIXME: The _quants field should be filled in with all the symbols, however
  -- #1658 is why this is empty, otherwise we end up with unused (and probably
  -- should be removed) symbols. But that's for another time. This is "fine"
  -- because _quants are only used relative to #1658.
  _quants      = [] :: [QuantityDict], -- map qw iMods ++ map qw symbolsAll,
  _instModels  = iMods,
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = inputSymbols,
  _outputs     = outputSymbols, 
  _constraints = inputConstraints,
  _constants   = [],
  _systemdb   = symbMap
}

purp :: Sentence
purp = foldlSent_ [S "simulate", short twoD, phrase CP.rigidBody,
  phrase Doc.physics, S "for use in", phrase game, S "development"]

concIns :: [ConceptInstance]
concIns = assumptions ++ goals ++ likelyChgs ++ unlikelyChgs ++ funcReqs ++ nonfuncReqs

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

--FIXME: Need to be able to print defn for gravitational constant.

--FIXME: All named ideas, not just acronyms.

ideaDicts :: [IdeaDict]
ideaDicts =
  -- Actual IdeaDicts
  doccon ++ educon ++ prodtcon ++
  -- CIs
  map nw [progName, centreMass] ++ map nw doccon' ++ map nw CM.mathcon' ++
  map nw CP.physicCon'
  
conceptChunks :: [ConceptChunk]
conceptChunks = 
  -- ConceptChunks
  algorithm : softwarecon ++ CP.physicCon ++ CM.mathcon ++ srsDomains ++
  -- DefinedQuantityDicts
  map cw defSymbols

symbMap :: ChunkDB
symbMap = cdb symbolsAll ideaDicts conceptChunks
  siUnits dataDefs iMods generalDefns tMods concIns [] allRefs citations

abbreviationsList :: [IdeaDict]
abbreviationsList =
  -- QuantityDicts
  map nw symbolsAll ++
  -- CIs
  map nw acronyms

-- | Holds all references and links used in the document.
allRefs :: [Reference]
allRefs = [externalLinkRef, pymunk] ++ uriReferences ++ offShelfSolRefs

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
  [S "Due to the rising cost of developing", plural videoGame `sC` 
  S "developers are looking" `S.for` S "ways to save time and money for their" +:+.
  plural project, S "Using an", phrase openSource, 
  phrase physLib,
  S "that is reliable and free will cut down development costs and lead",
  S "to better quality" +:+. plural product_ ,
  S "The document describes the program",
  S " based on the original, manually created version of" +:+
  namedRef externalLinkRef (S "GamePhysics")]

externalLinkRef :: Reference
externalLinkRef = makeURI "GamePhysicsSRSLink" 
  "https://github.com/smiths/caseStudies/blob/master/CaseStudies/gamephys" 
  (shortname' $ S "GamePhysicsSRSLink")
-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------
-- Purpose of Document automatically generated in IPurpose


---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------
scope :: Sentence
scope = foldlSent_ [phraseNP (the physicalSim) `S.of_` short twoD,
  plural CP.rigidBody, S "acted on by", plural QP.force]

--scope_of_requirements_intro_p2 = EmptyS
  
{-scope_of_requirements_intro_p2 = foldlSent_ [S "simulates how these", 
  plural CP.rigidBody, S "interact with one another"]
-}
----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

-------------------------------------
-- 2.3 : Organization of Documents --
-------------------------------------

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------
--------------------------
-- 3.1 : System Context --
--------------------------

sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [refS sysCtxFig1, S "shows the" +:+. phrase sysCont,
   S "A circle represents an entity external" `S.toThe` phrase software
   `sC` phraseNP (the user), S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself", sParen (short progName) +:+. EmptyS,
   S "Arrows are used to show the data flow between the", phraseNP (system
   `andIts` environment)]

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) 
  (resourcePath ++ "sysctx.png")

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol [S "The interaction between the", phraseNP (product_
   `andThe` user), S "is through an application programming" +:+.
   phrase interface, S "The responsibilities" `S.ofThe` phraseNP (user 
   `andThe` system), S "are as follows"]

sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide initial" +:+ pluralNP (condition `ofThePS`
  physical) +:+ S "state" `S.ofThe` phrase simulation `sC`
  plural CP.rigidBody +:+ S "present, and" +:+ plural QP.force +:+.
  S "applied to them",
  S "Ensure application programming" +:+ phrase interface +:+
  S "use complies with the" +:+ phrase user +:+. phrase guide,
  S "Ensure required" +:+
  namedRef (SRS.assumpt ([]::[Contents]) ([]::[Section])) (phrase software +:+ plural assumption) +:+
  S "are appropriate for any particular" +:+
  phrase problem +:+ phraseNP (the software) +:+. S "addresses"]

sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Determine if the" +:+ pluralNP (input_ `and_PS`
    simulation) +:+ S "state satisfy the required" +:+.
    namedRef (SRS.datCon ([]::[Contents]) ([]::[Section])) (phrase physical `S.and_` plural systemConstraint),
  S "Calculate the new state of all" +:+ plural CP.rigidBody +:+
    S "within the" +:+ phrase simulation +:+ S "at each" +:+
    phrase simulation +:+. S "step",
  S "Provide updated" +:+ phrase physical +:+ S "state of all" +:+
    plural CP.rigidBody +:+ S "at the end" `S.ofA` phrase simulation +:+.
    S "step"]

sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short progName +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

userCharacteristicsIntro :: Contents
userCharacteristicsIntro = foldlSP
  [S "The", phrase endUser `S.of_` short progName,
  S "should have an understanding of", phrase frstYr, S "programming",
  plural concept `S.and_` S "an understanding of", phrase highSchoolPhysics]

------------------------------
-- 3.3 : System Constraints --
------------------------------

---------------------------------------------
-- SECTION 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

-- NOTE: Section 4 remains incomplete. General definitions and instance models
-- have not been encoded.

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

probDescIntro :: Sentence
probDescIntro = foldlSent_
  [purp, S "in a", foldlList Comma List $ map S ["simple", "lightweight", "fast", "portable"],
  S "manner" `sC` S "which will allow" `S.for` S "the production of higher quality" +:+. plural product_,
  S "Creating a gaming", phrase physLib, S "is a difficult" +:+. phrase task, titleize' game,
  S "need", plural physLib, S "that simulate", plural object, S "acting under various", phrase physical,
  plural condition `sC` S "while simultaneously being fast and efficient enough to work in soft",
  phrase realtime, S "during the" +:+. phrase game, S "Developing a", 
  phrase physLib, S "from scratch takes a long period" `S.of_` phrase QP.time `S.and_`
  S "is very costly" `sC` S "presenting barriers of entry which make it difficult for",
  phrase game, S "developers to include", phrase Doc.physics, S "in their" +:+. 
  plural product_, S "There are a few free" `sC` phrase openSource `S.and_` S "high quality",
  namedRef (SRS.offShelfSol ([] :: [Contents]) ([] :: [Section])) (plural physLib),
  S "available to be used for", phrase consumer, plural product_]
  
-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

terms :: [ConceptChunk]
terms = [CP.rigidBody, CP.elasticity, CPP.ctrOfMass, CM.cartesian, CM.rightHand, CM.line, CM.point, CP.damping]

-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

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

generalDefinitionsIntro :: Contents
-- general_definitions_GDefs :: [Contents]

generalDefinitionsIntro = foldlSP 
  [S "This", phrase section_, S "collects the", pluralNP (CM.law `and_PP` 
  CM.equation), S "that will be used in deriving the", 
  plural dataDefn `sC` S "which in turn will be used to build the", 
  plural inModel]

-- GDefs not yet implemented --
{-
general_definitions_GDefs :: [Contents]
general_definitions_GDefs = map (Definition . General) gDefs)
-}

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------

dataDefinitionsIntro :: Sentence
dataDefinitionsIntro = foldlSent [atStartNP (the CPP.dimension)
   `S.of_` S "each", phrase quantity, S "is also given"]

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
-- in Requirements.hs

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

-----------------------------------------
-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --
-----------------------------------------

offShelfSols :: [Contents]
offShelfSols = [offShelfSolsIntro, offShelfSols2DList,
                offShelfSolsMid, offShelfSols3DList]

offShelfSolRefs :: [Reference]
offShelfSolRefs = [box2D, napePhysicsEngine, bullet, 
                   openDynamicsEngine, newtonGameDynamics]

offShelfSolsIntro, offShelfSols2DList, 
  offShelfSolsMid, offShelfSols3DList :: Contents

offShelfSolsIntro = mkParagraph $ foldlSentCol 
  [S "As mentioned" `S.inThe` namedRef (SRS.probDesc [] []) (phrase problemDescription) `sC`
  S "there already exist free", phrase openSource, phrase game +:+.
  plural physLib, S "Similar", short twoD, plural physLib, S "are"]

offShelfSols2DList = enumBulletU [
  namedRef box2D (S "Box2D"),
  namedRef napePhysicsEngine (S"Nape Physics Engine")]

offShelfSolsMid = mkParagraph $ foldl (+:+) EmptyS [S "Free", phrase openSource,
  short threeD, phrase game, plural physLib, S "include:"]

offShelfSols3DList = enumBulletU [
  namedRef bullet (S "Bullet"),
  namedRef openDynamicsEngine (S "Open Dynamics Engine"),
  namedRef newtonGameDynamics (S "Newton Game Dynamics")]

box2D, napePhysicsEngine, bullet, openDynamicsEngine,
  newtonGameDynamics :: Reference

box2D = makeURI "box2D" "http://box2d.org/" $ 
  shortname' $ S "Box2D"

napePhysicsEngine = makeURI "napePhysicsEngine" "http://napephys.com/" $ 
  shortname' $ S "Nape Physics Engine"

bullet = makeURI "bullet" "http://bulletphysics.org/" $ 
  shortname' $ S "Bullet"

openDynamicsEngine = makeURI "openDynamicsEngine" "http://www.ode.org/" $ 
  shortname' $ S "Open Dynamic Engine"

newtonGameDynamics = makeURI "newtonGameDynamics" "http://newtondynamics.com/" $ 
  shortname' $ S "Newton Game Dynamics"

-------------------------------------------------
-- SECTION 8 : Traceability Matrices and Graph --
-------------------------------------------------

-----------------------------------
-- VALUES OF AUXILIARY CONSTANTS --
-----------------------------------

----------------
-- REFERENCES --
----------------
-- To be added --
