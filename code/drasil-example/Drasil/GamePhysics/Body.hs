module Drasil.GamePhysics.Body where

import Language.Drasil hiding (Vector, organization, section, sec)
import Language.Drasil.Code (CodeSpec, codeSpec)
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
  dataConstraintUncertainty, goalStmtF,
  inDataConstTbl, intro, mkDoc, outDataConstTbl,
  mkEnumSimpleD, outDataConstTbl, termDefnF,
  traceMGF, tsymb, getDocDesc, egetDocDesc, generateTraceMap,
  getTraceMapFromTM, getTraceMapFromGD, getTraceMapFromDD, getTraceMapFromIM,
  getSCSSub, generateTraceTable, solutionLabel)

import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation as Doc (assumption, concept,
  condition, consumer, datumConstraint, document, endUser, environment, game,
  goalStmt, guide, information, input_, interface, item, model, object,
  organization, physical, physicalSim, physics, problem, product_, project,
  quantity, realtime, reference, requirement, section_, simulation, software,
  softwareSys, srsDomains, system, systemConstraint, sysCont, task, template,
  traceyMatrix, user, doccon, doccon')
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.IdeaDicts as Doc (dataDefn, genDefn, inModel, thModel)
import Data.Drasil.Concepts.Education (frstYr, highSchoolCalculus,
  highSchoolPhysics, educon)
import Data.Drasil.Concepts.Software (physLib, softwarecon)
import Data.Drasil.People (alex, luthfi)
import Data.Drasil.SI_Units (metre, kilogram, second, newton, radian,
  derived, fundamentals, joule)
import Data.Drasil.Software.Products (openSource, prodtcon, sciCompS, videoGame)

import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (ctrOfMass, dimension)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody, elasticity, 
  cartesian, rightHand, physicCon)
import qualified Data.Drasil.Concepts.Math as CM (equation, law, mathcon, mathcon')
import qualified Data.Drasil.Quantities.Physics as QP (force, time)

import Drasil.GamePhysics.Assumptions (assumptions)
import Drasil.GamePhysics.Changes (unlikelyChangesList', unlikelyChangeswithIntro,
 likelyChangesListwithIntro, likelyChangesList')
import Drasil.GamePhysics.Concepts (chipmunk, acronyms, threeD, twoD)
import Drasil.GamePhysics.DataDefs (qDefs, blockQDefs, dataDefns)
import Drasil.GamePhysics.Goals (goals)
import Drasil.GamePhysics.IMods (iModelsNew, instModIntro)
import Drasil.GamePhysics.References (citations, parnas1972, parnasClements1984)
import Drasil.GamePhysics.Requirements (funcReqs, nonfuncReqs,
    propsDeriv, requirements)
import Drasil.GamePhysics.TMods (tModsNew)
import Drasil.GamePhysics.Unitals (symbolsAll, outputConstraints,
  inputSymbols, outputSymbols, inputConstraints, defSymbols)

import Control.Lens ((^.))
import qualified Data.Map as Map

authors :: People
authors = [alex, luthfi]

auths :: Sentence
auths = S $ manyNames authors

srs :: Document
srs = mkDoc mkSRS for' sysInfo

checkSi :: [UnitDefn] -- FIXME
checkSi = collectUnits everything symbTT 

mkSRS :: DocDesc 
mkSRS = [RefSec $ RefProg intro [TUnits, tsymb tableOfSymbols, TAandA],
  IntroSec $ IntroProg para1_introduction_intro (short chipmunk)
  [IPurpose para1_purpose_of_document_intro,
   IScope scope_of_requirements_intro_p1 scope_of_requirements_intro_p2,
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
    TraceabilitySec $ TraceabilityProg [traceTable1, traceMatTabReqGoalOther, traceMatTabAssump,
      traceMatTabDefnModel] traceabilityMatricesAndGraphTraces
      (map LlC [traceTable1, traceMatTabReqGoalOther, traceMatTabAssump, traceMatTabDefnModel]) [],
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
  _authors = authors,
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

code :: CodeSpec
code = codeSpec sysInfo []

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
  [S "This", phrase typeOf, S "descibes the modeling of an",
  progDescrip, S "used for" +:+. appOf, S "The", 
  foldlList Comma List listOf, S "used in", short progName, 
  S "are provided. This", phrase typeOf, 
  S "is intended to be used as a", phrase reference, 
  S "to provide all necessary", phrase information, 
  S "to understand and verify the", phrase model]

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------
scope_of_requirements_intro_p1, scope_of_requirements_intro_p2 :: Sentence

scope_of_requirements_intro_p1 = foldlSent_
  [S "the", phrase physicalSim `sOf` getAcc twoD, 
  plural CP.rigidBody, S "acted on by", plural QP.force]
  
scope_of_requirements_intro_p2 = foldlSent_ [S "simulates how these", 
  plural CP.rigidBody, S "interact with one another"]

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
    [CP.rigidBody, CP.elasticity, CPP.ctrOfMass, CP.cartesian, CP.rightHand])

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
traceTable1 :: LabelledContent
traceTable1 = generateTraceTable sysInfo

traceabilityMatricesAndGraph :: Section
traceabilityMatricesAndGraph = traceMGF [traceMatTabReqGoalOther, traceMatTabAssump,
  traceMatTabDefnModel] traceabilityMatricesAndGraphTraces
  (map LlC [traceMatTabReqGoalOther, traceMatTabAssump, traceMatTabDefnModel]) []

traceabilityMatricesAndGraphTraces, traceability_matrices_and_graph_trace1,
  traceability_matrices_and_graph_trace2, traceability_matrices_and_graph_trace3 :: [Sentence]
traceabilityMatricesAndGraphTraces = map (foldlList Comma List) 
  [traceability_matrices_and_graph_trace1, traceability_matrices_and_graph_trace2,
   traceability_matrices_and_graph_trace3]

traceability_matrices_and_graph_trace1 = [plural goalStmt, 
  plural requirement, plural inModel, plural datumConstraint +:+. S "with each other"]

traceability_matrices_and_graph_trace2 = [plural thModel, plural genDefn, plural dataDefn, 
  plural inModel, S "on the" +:+. plural assumption]

traceability_matrices_and_graph_trace3 = [plural thModel, plural genDefn, plural dataDefn, 
  plural inModel +:+ S "on each other"]

-- these look like they could be generated by the sections above
traceMatInstaModel, traceMatAssump, traceMatFuncReq, traceMatData,
  traceMatGoalStmt, traceMatTheoryModel, traceMatGenDef, traceMatDataDef,
  traceMatLikelyChg :: [String]

traceMatInstaModelRef, traceMatAssumpRef, traceMatFuncReqRef, traceMatGoalStmtRef,
  traceMatTheoryModelRef, traceMatGenDefRef, traceMatDataDefRef,
  traceMatLikelyChgRef, traceMatDataRef :: [Sentence]

traceMatInstaModel = ["IM1", "IM2", "IM3"]
traceMatInstaModelRef = map makeRef2S iModelsNew

traceMatTheoryModel = ["T1", "T2", "T3", "T4", "T5"]
traceMatTheoryModelRef = map makeRef2S tModsNew

traceMatDataDef = ["DD1","DD2","DD3","DD4","DD5","DD6","DD7","DD8"]
traceMatDataDefRef = map makeRef2S dataDefns

traceMatAssump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7"]
traceMatAssumpRef = map makeRef2S assumptions

traceMatFuncReq =  ["R1","R2","R3", "R4", "R5", "R6", "R7", "R8"]
traceMatFuncReqRef = map makeRef2S funcReqs

traceMatData = ["Data Constraints"]
traceMatDataRef = [makeRef2S $ SRS.solCharSpec ([]::[Contents]) ([]::[Section])]

traceMatGoalStmt = ["GS1", "GS2", "GS3", "GS4"]
traceMatGoalStmtRef = makeListRef goals probDescription

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
  (traceMatInstaModelRef ++ take 3 traceMatFuncReqRef ++ traceMatDataRef)
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
  (EmptyS : traceMatTabReqGoalOtherRowHead)
  (makeTMatrix traceMatTabReqGoalOtherColHead traceMatTabReqGoalOtherCol
  traceMatTabReqGoalOtherRow)
  (showingCxnBw traceyMatrix (titleize' requirement +:+ sParen (makeRef2S requirements)
  `sC` titleize' goalStmt +:+ sParen (makeRef2S probDescription) `sAnd` S "Other" +:+
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
  (showingCxnBw traceyMatrix (titleize' assumption +:+ sParen (makeRef2S probDescription)
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
  traceMatTabDefnModelRow) (showingCxnBw traceyMatrix (titleize' item `sAnd`
  S "Other" +:+ titleize' section_)) True

-----------------------------------
-- VALUES OF AUXILIARY CONSTANTS --
-----------------------------------

----------------
-- REFERENCES --
----------------
-- To be added --
