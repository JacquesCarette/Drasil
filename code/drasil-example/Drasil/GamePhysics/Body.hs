module Drasil.GamePhysics.Body where

import Language.Drasil hiding (Vector, organization, section, sec)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (ChunkDB, RefbyMap, ReferenceDB, SystemInformation(SI),
  TraceMap, ccss, cdb, collectUnits, generateRefbyMap, rdb, refdb, _authors,
  _concepts, _constants, _constraints, _datadefs, _definitions, _defSequence,
  _inputs, _kind, _outputs, _quants, _sys, _sysinfodb, _usedinfodb)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import Utils.Drasil

import Drasil.DocLang (DerivationDisplay(..), DocDesc, DocSection(..), 
  Emphasis(..), Field(..), Fields, InclUnits(IncludeUnits), IntroSec(..), 
  IntroSub(..), RefSec(..), RefTab(..), SCSSub(..), SSDSec(SSDProg), 
  SSDSub(SSDSubVerb, SSDSolChSpec), SolChSpec(SCSProg), TConvention(..), 
  TSIntro(..), Verbosity(Verbose), ExistingSolnSec(..), GSDSec(..), GSDSub(..),
  TraceabilitySec(TraceabilityProg), ReqrmntSec(..), ReqsSub(..),
  LCsSec(..), UCsSec(..), AuxConstntSec(..), generateTraceMap',
  dataConstraintUncertainty, goalStmtF, inDataConstTbl, intro, mkDoc,
  outDataConstTbl, mkEnumSimpleD, outDataConstTbl, termDefnF, tsymb,
  getDocDesc, egetDocDesc, generateTraceMap, getTraceMapFromTM,
  getTraceMapFromGD, getTraceMapFromDD, getTraceMapFromIM, getSCSSub,
  traceMatStandard, solutionLabel)

import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation as Doc (assumption, concept,
  condition, consumer, document, endUser, environment, game, goalStmt, guide,
  information, input_, interface, model, object, organization, physical,
  physicalSim, physics, problem, product_, project, quantity, realtime,
  reference, section_, simulation, software, softwareSys, srsDomains, system,
  systemConstraint, sysCont, task, template, user, doccon, doccon')
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.IdeaDicts as Doc (dataDefn, inModel, thModel)
import Data.Drasil.Concepts.Education (frstYr, highSchoolCalculus,
  highSchoolPhysics, educon)
import Data.Drasil.Concepts.Software (physLib, softwarecon)
import Data.Drasil.People (alex, luthfi)
import Data.Drasil.SI_Units (metre, kilogram, second, newton, radian,
  derived, fundamentals, joule)
import Data.Drasil.Software.Products (openSource, prodtcon, sciCompS, videoGame)

import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (ctrOfMass, dimension)
import qualified Data.Drasil.Concepts.Physics as CP (elasticity, physicCon, rigidBody)
import qualified Data.Drasil.Concepts.Math as CM (cartesian, equation, law,
  mathcon, mathcon', rightHand)
import qualified Data.Drasil.Quantities.Physics as QP (force, time)

import Drasil.GamePhysics.Assumptions (assumptions)
import Drasil.GamePhysics.Changes (unlikelyChangesList', unlikelyChangeswithIntro,
 likelyChangesListwithIntro, likelyChangesList')
import Drasil.GamePhysics.Concepts (chipmunk, acronyms, threeD, twoD)
import Drasil.GamePhysics.DataDefs (qDefs, blockQDefs, dataDefns)
import Drasil.GamePhysics.Goals (goals)
import Drasil.GamePhysics.IMods (iModelsNew, instModIntro)
import Drasil.GamePhysics.References (citations, parnas1972, parnasClements1984)
import Drasil.GamePhysics.Requirements (funcReqs, nonfuncReqs, propsDeriv)
import Drasil.GamePhysics.TMods (tModsNew)
import Drasil.GamePhysics.Unitals (symbolsAll, outputConstraints,
  inputSymbols, outputSymbols, inputConstraints, defSymbols)

import Control.Lens ((^.))
import qualified Data.Map as Map

srs :: Document
srs = mkDoc mkSRS for' sysInfo

checkSi :: [UnitDefn] -- FIXME
checkSi = collectUnits everything symbTT 

mkSRS :: DocDesc 
mkSRS = [RefSec $ RefProg intro [TUnits, tsymb tableOfSymbols, TAandA],
  IntroSec $ IntroProg para1_introduction_intro (short chipmunk)
  [IPurpose para1_purpose_of_document_intro,
   IScope scope_of_requirements_intro_p1 EmptyS,
   IChar [] [S "rigid body dynamics", phrase highSchoolCalculus] [],
   IOrgSec organizationOfDocumentsIntro inModel (SRS.inModel [] []) EmptyS],
   GSDSec $ GSDProg2 [
    SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList],
    UsrChars [userCharacteristicsIntro], SystCons [] []],
   SSDSec $ SSDProg [SSDSubVerb probDescription
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields) tModsNew
        , GDs [] [] [] HideDerivation -- No Gen Defs for Gamephysics
        , DDs [] ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
        , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields)
          iModelsNew ShowDerivation
        , Constraints EmptyS dataConstraintUncertainty (S "FIXME")
            [inDataConstTbl inputConstraints, outDataConstTbl outputConstraints]
        , CorrSolnPpties propsDeriv
        ]
      ],
    ReqrmntSec $ ReqsProg [
      FReqsSub funcReqs [],
      NonFReqsSub nonfuncReqs
    ],
    LCsSec $ LCsProg likelyChangesListwithIntro,
    UCsSec $ UCsProg unlikelyChangeswithIntro,
    ExistingSolnSec $ ExistSolnProg offShelfSols,
    TraceabilitySec $ TraceabilityProg (map fst traceabilityMatrices)
      (map (foldlList Comma List . snd) traceabilityMatrices) (map (LlC . fst) traceabilityMatrices) [],
    AuxConstntSec $ AuxConsProg chipmunk [],
    Bibliography]
      where tableOfSymbols = [TSPurpose, TypogConvention[Vector Bold], SymbOrder]

label :: TraceMap
label = Map.union (generateTraceMap mkSRS) $ generateTraceMap' concIns

refBy :: RefbyMap
refBy = generateRefbyMap label

dataDefs :: [DataDefinition]
dataDefs = getTraceMapFromDD $ getSCSSub mkSRS

iMods :: [InstanceModel]
iMods = getTraceMapFromIM $ getSCSSub mkSRS

genDef :: [GenDefn]
genDef = getTraceMapFromGD $ getSCSSub mkSRS

theory :: [TheoryModel]
theory = getTraceMapFromTM $ getSCSSub mkSRS

concIns :: [ConceptInstance]
concIns = assumptions ++ likelyChangesList' ++ unlikelyChangesList' ++
  funcReqs

section :: [Section]
section = sec

sec :: [Section]
sec = extractSection srs

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

    --FIXME: Need to be able to print defn for gravitational constant.

sysInfo :: SystemInformation
sysInfo = SI {
  _sys = chipmunk,
  _kind = Doc.srs,
  _authors = [alex, luthfi],
  _quants = symbTT, 
  _concepts = [] :: [DefinedQuantityDict],
  _definitions = qDefs,
  _datadefs = dataDefns,
  _inputs = inputSymbols,
  _outputs = outputSymbols, 
  _defSequence = blockQDefs,
  _constraints = inputConstraints,
  _constants = [],
  _sysinfodb = everything,
  _usedinfodb = usedDB,
   refdb = refDB
}

symbTT :: [DefinedQuantityDict]
symbTT = ccss (getDocDesc mkSRS) (egetDocDesc mkSRS) everything

refDB :: ReferenceDB
refDB = rdb citations concIns

--FIXME: All named ideas, not just acronyms.

units :: [UnitDefn] -- FIXME
units = map unitWrapper [metre, kilogram, second, joule] ++ map unitWrapper [newton, radian]

everything :: ChunkDB
everything = cdb (map qw iModelsNew ++ map qw symbolsAll) (map nw symbolsAll
  ++ map nw acronyms ++ map nw prodtcon ++ map nw iModelsNew
  ++ map nw softwarecon ++ map nw doccon ++ map nw doccon'
  ++ map nw CP.physicCon ++ map nw educon ++ [nw algorithm] ++ map nw derived
  ++ map nw fundamentals ++ map nw CM.mathcon ++ map nw CM.mathcon')
  (map cw defSymbols ++ srsDomains ++ map cw iModelsNew) units
  label refBy dataDefs iMods genDef theory
  concIns section []

usedDB :: ChunkDB
usedDB = cdb (map qw symbTT) (map nw symbolsAll ++ map nw acronyms
 ++ map nw checkSi) ([] :: [ConceptChunk]) checkSi label refBy
 dataDefs iMods genDef theory concIns section
 []

printSetting :: PrintingInformation
printSetting = PI everything defaultConfiguration



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

para1_introduction_intro :: Sentence
para1_introduction_intro = foldlSent
  [S "Due to the rising cost of developing", plural videoGame `sC` 
  S "developers are looking for ways to save time and money for their" +:+.
  plural project, S "Using an", phrase openSource, 
  phrase physLib,
  S "that is reliable and free will cut down development costs and lead",
  S "to better quality", plural product_]

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------

detailsAndGoal :: [CI]
detailsAndGoal = [thModel, goalStmt]

para1_purpose_of_document_intro :: Sentence
para1_purpose_of_document_intro = para1_purpose_of_document_param chipmunk 
  document programDescription (plural game) (map plural detailsAndGoal)

programDescription :: Sentence
programDescription = foldlSent_ [phrase openSource, getAcc twoD, 
  phrase CP.rigidBody, phrase physLib]

para1_purpose_of_document_param :: (Idea a, NamedIdea b) => a -> b -> Sentence -> Sentence ->
  [Sentence] -> Sentence
para1_purpose_of_document_param progName typeOf progDescrip appOf listOf = foldlSent 
  [S "This", phrase typeOf, S "describes the modeling of an",
  progDescrip, S "used for" +:+. appOf, S "The", 
  foldlList Comma List listOf, S "used in", short progName, 
  S "are provided. This", phrase typeOf, 
  S "is intended to be used as a", phrase reference, 
  S "to provide all necessary", phrase information, 
  S "to understand and verify the", phrase model]

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------
scope_of_requirements_intro_p1 :: Sentence

scope_of_requirements_intro_p1 = foldlSent_
  [S "the", phrase physicalSim `sOf` getAcc twoD, 
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

organizationOfDocumentsIntro :: Sentence

organizationOfDocumentsIntro = foldlSent 
  [S "The", phrase organization, S "of this", phrase document, 
  S "follows the", phrase template, S "for an", getAcc Doc.srs, S "for", 
  phrase sciCompS, S "proposed by", makeCiteS parnas1972 `sAnd` 
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
   phrase softwareSys, S "itself", sParen (short chipmunk) +:+. EmptyS,
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
    plural CP.rigidBody +:+ S "present, and" +:+ plural QP.force +:+.
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

userCharacteristicsIntro :: Contents
userCharacteristicsIntro = foldlSP
  [S "The", phrase endUser `sOf` short chipmunk,
  S "should have an understanding of", phrase frstYr, S "programming",
  plural concept `sAnd` S "an understanding of", phrase highSchoolPhysics]

-------------------------------
-- 3.3 : System Constraints  --
-------------------------------

---------------------------------------------
-- SECTION 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

-- NOTE: Section 4 remains incomplete. General definitions and instance models
-- have not been encoded.

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

probDescription :: Section
probDescription = SRS.probDesc [probDescIntro]
  [termAndDefn, goalStates]

probDescIntro :: Contents
probDescIntro = foldlSP 
  [S "Creating a gaming", phrase physLib, S "is a difficult" +:+. phrase task,
  titleize' game, S "need",  plural physLib, S "that simulate", plural object,
  S "acting under various", phrase physical, plural condition `sC` S "while", 
  S "simultaneously being fast and efficient enough to work in soft",
  phrase realtime, S "during the" +:+. phrase game, S "Developing a", 
  phrase physLib, S "from scratch takes a long period of", phrase QP.time `sAnd`
  S "is very costly" `sC` S "presenting barriers of entry which make it difficult for",
  phrase game, S "developers to include", phrase Doc.physics, S "in their" +:+. 
  plural product_, S "There are a few free" `sC` phrase openSource `sAnd` S "high quality",
  plural physLib, S "available to be used for", phrase consumer, plural product_ +:+. 
  sParen (makeRef2S $ SRS.offShelfSol ([] :: [Contents]) ([] :: [Section])),
  S "By creating a simple, lightweight, fast and portable",
  getAcc twoD, phrase CP.rigidBody, phrase physLib `sC` phrase game,
  S "development will be more accessible to the masses" `sAnd` S "higher quality",
  plural product_, S "will be produced"]
  
-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

termAndDefn :: Section
termAndDefn = termDefnF Nothing [termAndDefnBullets]

terminologyLabel :: Reference
terminologyLabel = makeLstRef "terminologyGM" "terminologyGM"

termAndDefnBullets :: Contents
termAndDefnBullets = LlC $ enumBullet terminologyLabel
  (map (\x -> atStart x +: EmptyS +:+ (x ^. defn))
    [CP.rigidBody, CP.elasticity, CPP.ctrOfMass, CM.cartesian, CM.rightHand])

-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

goalStates :: Section
goalStates = goalStmtF [S "the" +:+ plural input_] goalStateList

goalStateList :: [Contents]
goalStateList = mkEnumSimpleD goals

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
  [S "This", phrase section_, S "collects the", plural CM.law `sAnd` 
  plural CM.equation, S "that will be used in deriving the", 
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
dataDefinitionsIntro = foldlSent [S "The", phrase CPP.dimension
   `sOf` S "each", phrase quantity, S "is also given"]

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
                offShelfSolsMid,   offShelfSols3DList]

offShelfSolsIntro, offShelfSols2DList, 
  offShelfSolsMid, offShelfSols3DList :: Contents

offShelfSolsIntro = mkParagraph $ foldlSentCol 
  [S "As mentioned in", makeRef2S probDescription `sC`
  S "there already exist free", phrase openSource, phrase game +:+.
  plural physLib, S "Similar", getAcc twoD, plural physLib, S "are"]

offShelfSols2DList = LlC $ enumBullet solutionLabel [S "Box2D: http://box2d.org/",
  S "Nape Physics Engine: http://napephys.com/"]

offShelfSolsMid = mkParagraph $ foldl (+:+) EmptyS [S "Free", phrase openSource,
  getAcc threeD, phrase game, plural physLib, S "include:"]

offShelfSols3DList = LlC $ enumBullet solutionLabel [
  S "Bullet: http://bulletphysics.org/",
  S "Open Dynamics Engine: http://www.ode.org/",
  S "Newton Game Dynamics: http://newtondynamics.com/"]

-----------------------------------------------------
-- SECTION 8 : Traceability Matrices and Graph    --
-----------------------------------------------------

traceabilityMatrices :: [(LabelledContent, [Sentence])]
traceabilityMatrices = traceMatStandard sysInfo

-----------------------------------
-- VALUES OF AUXILIARY CONSTANTS --
-----------------------------------

----------------
-- REFERENCES --
----------------
-- To be added --
