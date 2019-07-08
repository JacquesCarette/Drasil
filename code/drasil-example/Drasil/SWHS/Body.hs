module Drasil.SWHS.Body where

import Language.Drasil hiding (organization, section, sec)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, RefbyMap, ReferenceDB,
  SystemInformation(SI), TraceMap, ccss, cdb, collectUnits, generateRefbyMap,
  getIdeaDict, rdb, refdb, _authors, _concepts, _constants, _constraints,
  _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs, _quants,
  _sys, _sysinfodb, _usedinfodb)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import Utils.Drasil

import Control.Lens ((^.))
import qualified Data.Map as Map

import Drasil.DocLang (AuxConstntSec (AuxConsProg), DocDesc, DocSection (..),
  Field(..), Fields, LFunc(TermExcept), Literature(Doc', Lit), IntroSec(IntroProg),
  IntroSub(IChar, IOrgSec, IPurpose, IScope), RefSec (RefProg), 
  RefTab (TAandA, TUnits), TSIntro(SymbConvention, SymbOrder, TSPurpose),
  ReqrmntSec(..), ReqsSub(..), SSDSub(..), SolChSpec (SCSProg), SSDSec(..), 
  InclUnits(..), DerivationDisplay(..), SCSSub(..), Verbosity(..),
  TraceabilitySec(TraceabilityProg), LCsSec(..), UCsSec(..),
  GSDSec(..), GSDSub(..), ProblemDescription(PDProg), PDSub(..),
  dataConstraintUncertainty, intro, mkDoc, outDataConstTbl, tsymb'',
  getDocDesc, egetDocDesc, ciGetDocDesc, generateTraceMap,
  generateTraceMap', getTraceMapFromTM, getTraceMapFromGD, getTraceMapFromDD,
  getTraceMapFromIM, getSCSSub, traceMatStandard)
import qualified Drasil.DocLang.SRS as SRS (inModel)

import Data.Drasil.Concepts.Thermodynamics (thermocon)
import Data.Drasil.Concepts.Documentation as Doc (assumption, column, condition,
  constraint, content, datum, definition, document, environment, goalStmt,
  information, input_, model, organization, output_, physical, physics, problem,
  property, purpose, quantity, reference, software, softwareSys, srs, srsDomains,
  sysCont, system, user, value, variable, doccon, doccon')
import Data.Drasil.IdeaDicts as Doc (inModel, thModel)
import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Concepts.Education (calculus, educon, engineering)
import Data.Drasil.Concepts.Math (de, equation, ode, unit_, mathcon, mathcon')
import Data.Drasil.Concepts.Software (program, softwarecon, correctness,
  understandability, reusability, maintainability, verifiability)
import Data.Drasil.Concepts.Physics (physicCon)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty, physicalcon)
import Data.Drasil.Software.Products (sciCompS, prodtcon)
import Data.Drasil.Quantities.Math (gradient, surface, uNormalVect, surArea)
import Data.Drasil.Quantities.PhysicalProperties (density, mass, vol)
import Data.Drasil.Quantities.Physics (energy, time, physicscon)
import Data.Drasil.Quantities.Thermodynamics (heatCapSpec, latentHeat, temp)

import Data.Drasil.People (brooks, spencerSmith, thulasi)
import Data.Drasil.SI_Units (metre, kilogram, second, centigrade, joule, watt,
  fundamentals, derived, m_2, m_3)
import qualified Data.Drasil.Concepts.Thermodynamics as CT (heatTrans,
  thermalConduction, htFlux, heatCapSpec, thermalEnergy, htTransTheo,
  thermalAnalysis, enerSrc)

import Drasil.SWHS.Assumptions (assumpPIS, assumptions)
import Drasil.SWHS.Changes (likelyChgs, unlikelyChgs)
import Drasil.SWHS.Concepts (acronymsFull, progName, sWHT, water, phsChgMtrl,
  coil, tank, transient, swhsPCM, phaseChangeMaterial, tankPCM, con)
import Drasil.SWHS.DataDefs (dataDefs, qDefs)
import Drasil.SWHS.GenDefs (genDefs)
import Drasil.SWHS.Goals (goals)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM,
  iMods, instModIntro)
import Drasil.SWHS.References (parnas1972, parnasClements1984, citations)
import Drasil.SWHS.Requirements (dataConTable1, funcReqs, inputInitQuantsTable,
  nfRequirements, propsDeriv)
import Drasil.SWHS.TMods (consThermE, sensHtE, latentHtE)
import Drasil.SWHS.Unitals (coilHTC, coilSA, eta, htCapSP, htCapW,
  htFluxC, htFluxP, htFluxIn, htFluxOut, inSA, outSA, pcmE,
  pcmHTC, pcmSA, pcmMass, specParamValList, constrained, inputs,
  outputs, symbols, symbolsAll, unitalChuncks, tauSP, tauW, tempC,
  tempPCM, tempW, thFluxVect, thickness, volHtGen, watE, wMass, absTol, relTol)

-------------------------------------------------------------------------------

units :: [UnitDefn]
units = map unitWrapper [metre, kilogram, second] ++ 
  map unitWrapper [centigrade, joule, watt]
--Will there be a table of contents?

unitsColl :: [UnitDefn]
unitsColl = collectUnits symMap symbTT

si :: SystemInformation
si = SI {
  _sys = swhsPCM,
  _kind = srs, 
  _authors = [thulasi, brooks, spencerSmith],
  _quants = symbols,
  _concepts = symbTT,
  _definitions = qDefs,
  _datadefs = dataDefs,
  _inputs = inputs,
  _outputs = map qw outputs,
  _defSequence = [] :: [Block QDefinition],
  _constraints = constrained,
  _constants = specParamValList,
  _sysinfodb = symMap,
  _usedinfodb = usedDB,
   refdb = refDB
}

resourcePath :: String
resourcePath = "../../../datafiles/SWHS/"

symMap :: ChunkDB
symMap = cdb (qw heatEInPCM : symbolsAll) -- heatEInPCM ?
  (nw heatEInPCM : map nw symbols ++ map nw acronymsFull
  ++ map nw thermocon ++ map nw units ++ map nw [m_2, m_3] ++ map nw [absTol, relTol]
  ++ map nw physicscon ++ map nw doccon ++ map nw softwarecon ++ map nw doccon' ++ map nw con
  ++ map nw prodtcon ++ map nw physicCon ++ map nw mathcon ++ map nw mathcon' ++ map nw specParamValList
  ++ map nw fundamentals ++ map nw educon ++ map nw derived ++ map nw physicalcon ++ map nw unitalChuncks
  ++ [nw swhsPCM, nw algorithm] ++ map nw compcon ++ [nw materialProprty])
  (cw heatEInPCM : map cw symbols ++ srsDomains) -- FIXME: heatEInPCM?
  (units ++ [m_2, m_3]) label refBy
  dataDefn insModel genDef theory concIns
  section labCon

usedDB :: ChunkDB
usedDB = cdb (map qw symbTT) (map nw symbols ++ map nw acronymsFull ++ map nw unitsColl)
 ([] :: [ConceptChunk]) unitsColl label refBy dataDefn insModel genDef
 theory concIns section labCon

refDB :: ReferenceDB
refDB = rdb citations concIns

printSetting :: PrintingInformation
printSetting = PI symMap defaultConfiguration

  --Note: The second symbols here is
    -- Redundant b/c the unitals are not really concepts (yet). There
    -- Will still likely be a better way to do this.
  --FIXME: Should be all Named, not just acronyms at the end.
acronyms :: [CI]
acronyms = ciGetDocDesc mkSRS

shortTT :: [IdeaDict]
shortTT = concatMap (`getIdeaDict` symMap) $ getDocDesc mkSRS

symbTT :: [DefinedQuantityDict]
symbTT = ccss (getDocDesc mkSRS) (egetDocDesc mkSRS) symMap

mkSRS :: DocDesc
mkSRS = [RefSec $ RefProg intro [
    TUnits,
    tsymb'' tSymbIntro (TermExcept [uNormalVect]),
    TAandA],
  IntroSec $
    IntroProg (introP1 CT.enerSrc energy swhsPCM phsChgMtrl
    progName CT.thermalEnergy latentHeat unit_) (introP2 swhsPCM program
    progName)
    [IPurpose $ purpDoc swhsPCM progName,
     IScope (scopeReqs1 CT.thermalAnalysis tankPCM) $
       scopeReqs2 temp CT.thermalEnergy water phsChgMtrl sWHT,
     IChar [] (charReader1 CT.htTransTheo ++ charReader2 de) [],
     IOrgSec orgDocIntro inModel (SRS.inModel [] [])
       $ orgDocEnd swhsPCM progName],
  GSDSec $ GSDProg2 
    [ SysCntxt [sysCntxtDesc progName, LlC sysCntxtFig, sysCntxtRespIntro progName, systContRespBullets]
    , UsrChars [userChars progName]
    , SystCons [] []
    ],
  SSDSec $
    SSDProg 
      [ SSDProblem $ PDProg probDescIntro []
        [ TermsAndDefs Nothing terms
        , PhySysDesc progName physSystParts figTank []
        , Goals goalInputs goals]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields) [consThermE, sensHtE, latentHtE]
        , GDs [] ([Label, Units] ++ stdFields) genDefs ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) dataDefs ShowDerivation
        , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields)
         [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM] ShowDerivation
        , Constraints  EmptyS dataConstraintUncertainty dataConTail
         [dataConTable1, dataConTable3]
        , CorrSolnPpties propsDeriv
        ]
      ],
  ReqrmntSec $ ReqsProg [
    FReqsSub funcReqs [inputInitQuantsTable],
    NonFReqsSub nfRequirements
  ],
  LCsSec $ LCsProg likelyChgs,
  UCsSec $ UCsProg unlikelyChgs,
  TraceabilitySec $
    TraceabilityProg (map fst traceabilityMatrices) (map (foldlList Comma List . snd) traceabilityMatrices)
      (map (LlC . fst) traceabilityMatrices) [],
  AuxConstntSec $ AuxConsProg progName specParamValList,
  Bibliography]

tSymbIntro :: [TSIntro]
tSymbIntro = [TSPurpose, SymbConvention
  [Lit (nw CT.heatTrans), Doc' (nw progName)], SymbOrder]

--- The document starts here
srs' :: Document
srs' = mkDoc mkSRS for si

label :: TraceMap
label = Map.union (generateTraceMap mkSRS) $ generateTraceMap' concIns
 
refBy :: RefbyMap
refBy = generateRefbyMap label 

dataDefn :: [DataDefinition]
dataDefn = getTraceMapFromDD $ getSCSSub mkSRS

insModel :: [InstanceModel]
insModel = getTraceMapFromIM $ getSCSSub mkSRS

genDef :: [GenDefn]
genDef = getTraceMapFromGD $ getSCSSub mkSRS

theory :: [TheoryModel]
theory = getTraceMapFromTM $ getSCSSub mkSRS

concIns :: [ConceptInstance]
concIns = goals ++ assumptions ++ likelyChgs ++ unlikelyChgs ++ funcReqs

section :: [Section]
section = sec

labCon :: [LabelledContent]
labCon = [dataConTable1, inputInitQuantsTable]

sec :: [Section]
sec = extractSection srs'

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

priorityNFReqs :: [ConceptChunk]
priorityNFReqs = [correctness, verifiability, understandability, reusability,
  maintainability]
-- It is sometimes hard to remember to add new sections both here and above.

-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --

------------------------------
-- Section 2 : INTRODUCTION --
------------------------------

-- In Concepts.hs "swhsPCM" gives "s for program name, and there is a
-- similar paragraph in each of the other solar water heating systems
-- incorporating PCM" which is not capitlaized whereas the stable version is

-- NamedChunks... Sometimes capitalized, sometimes not, sometimes plural,
-- sometimes not, sometimes need to be used in different tenses. How to
-- accomodate all this?

-- The second paragraph is general between examples. It can probably be
-- abstracted out.

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------

-- Besides program name, these two paragraphs are general, mostly repeated
-- between examples, and can be abstracted out.

--How to italicize words in sentence?
--How to cite?

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------

-- There is a similar paragraph in each example, but there's a lot of specific
-- info here. Would need to abstract out the object of analysis (i.e. solar
-- water heating tank incorporating PCM, 2D slope composed of homogeneous soil
-- layers, glass slab and blast, or 2D bodies acted on by forces) and also
-- abstract out the overall goal of the program (i.e. predict the temperature
-- and energy histories for the water and PCM, simulate how 2D rigid bodies
-- interact with each other, predict whether the glass slab is safe to use or
-- not, etc.). If that is done, then this paragraph can also be abstracted out.

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------
------------------------------------
-- 2.4 : Organization of Document --
------------------------------------
--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

--------------------------
-- 3.1 : System Context --
--------------------------

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

------------------------------
-- 3.3 : System Constraints --
------------------------------

---------------------------------------------
-- Section 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------


-------------------------------
-- 4.1 : Problem Description --
-------------------------------

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

terms :: [ConceptChunk]
terms = map cw [CT.htFlux, phaseChangeMaterial, CT.heatCapSpec, CT.thermalConduction, transient]

-- Included heat flux and specific heat in NamedChunks even though they are
-- already in SWHSUnits

-----------------------------------------
-- 4.1.2 : Physical System Description --
-----------------------------------------

physSystParts :: [Sentence]
physSystParts = map foldlSent_ [physSyst1 tank water, physSyst2 coil tank htFluxC,
  [short phsChgMtrl, S "suspended in" +:+. phrase tank,
  sParen (ch htFluxP +:+ S "represents the" +:+. phrase htFluxP)]]

physSyst1 :: ConceptChunk -> ConceptChunk -> [Sentence]
physSyst1 ta wa = [atStart ta, S "containing" +:+. phrase wa]

physSyst2 :: ConceptChunk -> ConceptChunk -> UnitalChunk -> [Sentence]
physSyst2 co ta hfc = [atStart co, S "at bottom of" +:+. phrase ta,
  sParen (ch hfc +:+ S "represents the" +:+. phrase hfc)]

-- Structure of list would be same between examples but content is completely
-- different

figTank :: LabelledContent
figTank = llcc (makeFigRef "Tank") $ fig (
  foldlSent_ [atStart sWHT `sC` S "with", phrase htFluxC `sOf`
  ch htFluxC `sAnd` phrase htFluxP `sOf` ch htFluxP])
  $ resourcePath ++ "Tank.png"

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

goalInputs :: [Sentence]
goalInputs  = [S "the" +:+ phrase tempC,
  S "the initial" +:+ plural condition +:+ S "for the" +:+ phrase tempW `andThe` phrase tempPCM,
  S "the material" +:+ plural property]

-- 2 examples include this paragraph, 2 don't. The "givens" would need to be
-- abstracted out if this paragraph were to be abstracted out.

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

-- Can booktabs colored links be used? The box links completely cover nearby
-- punctuation.

--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

-- Theory has to be RelationChunk....
-- No way to include "Source" or "Ref. By" sections?

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

-- SECTION 4.2.3 --
{--- General Definitions is automatically generated in solChSpecF
s4_2_3_genDefs :: [Contents]
s4_2_3_genDefs = map reldefn swhsRC

s4_2_3_deriv :: [Contents]
s4_2_3_deriv = [s4_2_3_deriv_1 rOfChng temp,
  s4_2_3_deriv_2 consThermE vol,
  s4_2_3_deriv_3,
  s4_2_3_deriv_4 gaussDiv surface vol thFluxVect uNormalVect unit_,
  s4_2_3_deriv_5,
  s4_2_3_deriv_6 vol volHtGen,
  s4_2_3_deriv_7,
  s4_2_3_deriv_8 htFluxIn htFluxOut inSA outSA density heatCapSpec
    temp vol assumption assump3 assump4 assump5 assump6,
  s4_2_3_deriv_9,
  s4_2_3_deriv_10 density mass vol,
  s4_2_3_deriv_11]-}

-- General Definitions is automatically generated 

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------
-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------
----------------------------
-- 4.2.6 Data Constraints --
----------------------------
------------------------------
-- Data Constraint: Table 1 --
------------------------------

------------------------------
-- Data Constraint: Table 2 --
------------------------------

------------------------------
-- Data Constraint: Table 3 --
------------------------------

dataConTable3 :: LabelledContent
dataConTable3 = outDataConstTbl outputConstraints
--FIXME: add "(by A11)" in Physical Constraints of `tempW` and `tempPCM`?

outputConstraints :: [ConstrConcept]
outputConstraints = [tempW, tempPCM, watE, pcmE]

-- Other Notes:
---- Will there be a way to have asterisks for certain pieces of the table?

----------------------------------------------
-- 4.2.7 : Properties of A Correct Solution --
----------------------------------------------

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------
-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------
--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

--------------------------------
-- Section 6b : UNLIKELY CHANGES --
--------------------------------

--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

traceabilityMatrices :: [(LabelledContent, [Sentence])]
traceabilityMatrices = traceMatStandard si

------------------------
-- Traceabilty Graphs --
------------------------
-------------------------------------------------
-- Section 8 :  Specification Parameter Values --
-------------------------------------------------
----------------------------
-- Section 9 : References --
----------------------------


-------------------------------------------------------------------------------


-- ============== --
-- Dead Knowledge --
-- ============== --


------------------------------
-- Section 2 : INTRODUCTION --
------------------------------

introP1 :: (NamedIdea en, Definition en) => ConceptChunk -> UnitalChunk -> en -> CI -> CI ->
  ConceptChunk -> UnitalChunk -> ConceptChunk -> Sentence
introP1 es en sp pcmat pro te lh un = foldlSent [
  S "Due to the", foldlList Comma List (map S ["increasing cost", "diminishing availability",
    "negative environmental impact of fossil fuels"]) `sC`
  S "there is a higher demand for renewable", plural es `sAnd` phrase en +:+.
  S "storage technology", sp ^. defn, sParen (short pcmat), S "use a renewable",
  phrase es `sAnd` S "provide a novel way of storing" +:+. phrase en,
  atStart sp, S "improve over the traditional",
  plural pro, S "because of their smaller size. The",
  S "smaller size is possible because of the ability of",
  short pcmat, S "to store", phrase te, S "as", phrase lh `sC`
  S "which allows higher", phrase te, S "storage capacity per",
  phrase un, S "weight"]

introP2 :: NamedIdea ni => ni -> ConceptChunk -> CI -> Sentence
introP2 sp pr pro = foldlSent_ [EmptyS +:+. phrase sp, S "The developed",
  phrase pr, S "will be referred to as", titleize pro,
  sParen (short pro)] -- SSP has same style sentence here

-- In Concepts.hs "swhsPCM" gives "s for program name, and there is a
-- similar paragraph in each of the other solar water heating systems
-- incorporating PCM" which is not capitlaized whereas the stable version is

-- NamedChunks... Sometimes capitalized, sometimes not, sometimes plural,
-- sometimes not, sometimes need to be used in different tenses. How to
-- accomodate all this?

-- The second paragraph is general between examples. It can probably be
-- abstracted out.

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------

purpDoc :: NamedIdea ni => ni -> CI -> Sentence
purpDoc sp pro = foldlSent [S "The main", phrase purpose, S "of this",
  phrase document, S "is to describe the modelling of" +:+.
  phrase sp, S "The", plural goalStmt `sAnd` plural thModel,
  S "used in the", short pro, S "code are provided, with an emphasis",
  S "on explicitly identifying", plural assumption `sAnd` S "unambiguous" +:+.
  plural definition, S "This", phrase document,
  S "is intended to be used as a", phrase reference,
  S "to provide ad hoc access to all", phrase information,
  S "necessary to understand and verify the" +:+. phrase model, S "The",
  short srs, S "is abstract because the", plural content, S "say what",
  phrase problem, S "is being solved, but do not say how to solve it"]


-- Besides program name, these two paragraphs are general, mostly repeated
-- between examples, and can be abstracted out.

--How to italicize words in sentence?
--How to cite?

---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------

scopeReqs1 :: ConceptChunk -> ConceptChunk -> Sentence
scopeReqs1 ta tp = foldlSent_ [phrase ta,
  S "of a single", phrase tp]

scopeReqs2 :: UnitalChunk -> ConceptChunk -> ConceptChunk -> CI ->
  ConceptChunk -> Sentence
scopeReqs2 t te wa pcmat sw = foldlSent_ [S "predicts the",
  phrase t `sAnd` phrase te,
  S "histories for the", phrase wa `sAnd` S "the" +:+.
  short pcmat, S "This entire", phrase document,
  S "is written assuming that the substances inside the",
  phrase sw, S "are", phrase wa `sAnd` short pcmat]

-- There is a similar paragraph in each example, but there's a lot of specific
-- info here. Would need to abstract out the object of analysis (i.e. solar
-- water heating tank rating PCM, 2D slope composed of homogeneous soil
-- layers, glass slab and blast, or 2D bodies acted on by forces) and also
-- abstract out the overall goal of the program (i.e. predict the temperature
-- and energy histories for the water and PCM, simulate how 2D rigid bodies
-- interact with each other, predict whether the glass slab is safe to use or
-- not, etc.). If that is done, then this paragraph can also be abstracted out.

----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

charReader1 :: ConceptChunk -> [Sentence]
charReader1 htt = [phrase htt +:+ S "from level 3 or 4" +:+
  S "mechanical" +:+ phrase engineering]

charReader2 :: CI -> [Sentence]
charReader2 diffeq = [plural diffeq +:+
  S "from level 1 and 2" +:+ phrase calculus]

------------------------------------
-- 2.4 : Organization of Document --
------------------------------------

orgDocIntro :: Sentence
orgDocIntro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the template for an", short srs,
  S "for", phrase sciCompS, S "proposed by", makeCiteS parnas1972 `sAnd` 
  makeCiteS parnasClements1984]

orgDocEnd :: NamedIdea ni => ni -> CI -> Sentence
orgDocEnd sp pro = foldlSent_ [S "The", plural inModel, 
  S "to be solved are referred to as" +:+. 
  foldlList Comma List (map makeRef2S iMods), S "The", plural inModel,
  S "provide the", phrase ode, sParen (short ode :+: S "s") `sAnd` 
  S "algebraic", plural equation, S "that", phrase model, S "the" +:+. 
  phrase sp, short pro, S "solves these", short ode :+: S "s"]

-- This paragraph is mostly general (besides program name and number of IMs),
-- but there are some differences between the examples that I'm not sure how to
-- account for. Specifically, the glass example references a Volere paper that
-- is not used for the other examples. Besides that, this paragraph could
-- probably be abstracted out with some changes (i.e. the other examples don't
-- include the last sentence, so we might not need to know the number of IMs
-- after all if we just leave that sentence out)

-- IM1 to IM4 : reference later

-- how to cite/reference?

-- If all SRS have the same basic layout, is it possible to automate
-- the sectioning? This would also improve the tediousness of declaring
-- LayoutObjs

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------

--------------------------
-- 3.1 : System Context --
--------------------------

sysCntxtDesc :: CI -> Contents
sysCntxtDesc pro = foldlSP [makeRef2S sysCntxtFig, S "shows the" +:+. phrase sysCont, 
  S "A circle represents an external entity outside the",
  phrase software `sC` S "the", phrase user, S "in this case. A",
  S "rectangle represents the", phrase softwareSys, S "itself" +:+.
  sParen (short pro), S "Arrows are used to show the",
  plural datum, S "flow between the", phrase system `sAnd`
  S "its", phrase environment]

sysCntxtFig :: LabelledContent
sysCntxtFig = llcc (makeFigRef "SysCon") $ fig (foldlSent_
  [makeRef2S sysCntxtFig +: EmptyS, titleize sysCont])
  $ resourcePath ++ "SystemContextFigure.png"

sysCntxtRespIntro :: CI -> Contents
sysCntxtRespIntro pro = foldlSPCol [short pro +:+. S "is mostly self-contained",
  S "The only external interaction is through the", phrase user +:+.
  S "interface", S "responsibilities" `ofThe'` phrase user `sAnd`
  S "the", phrase system, S "are as follows"]

systContRespBullets :: Contents
systContRespBullets = UlC $ ulcc $ Enumeration $ Bullet $ noRefs [userResp input_ datum,
  resp]

-- User Responsibilities --
userResp :: NamedChunk -> NamedChunk -> ItemType
userResp inp dat = Nested (titleize user +: S "Responsibilities")
  $ Bullet $ noRefs $ map Flat [

  foldlSent_ [S "Provide the", phrase inp, plural dat, S "to the",
  phrase system `sC` S "ensuring no errors in the", plural dat, S "entry"],

  foldlSent_ [S "Take care that consistent", plural unit_,
  S "are used for", phrase inp, plural variable]

  ]

-- SWHS Responsibilities --
resp :: ItemType
resp = Nested (short progName +: S "Responsibilities")
  $ Bullet $ noRefs $ map Flat [

  foldlSent_ [S "Detect", plural datum, S "type mismatch, such as a string of",
  S "characters instead of a floating point number"],

  foldlSent_ [S "Determine if the", plural input_, S "satisfy the required",
  phrase physical `sAnd` phrase software, plural constraint],

  foldlSent_ [S "Calculate the required", plural output_]

  ]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

userChars :: CI -> Contents
userChars pro = foldlSP [S "The end", phrase user `sOf`
  short pro, S "should have an understanding of undergraduate",
  S "Level 1 Calculus" `sAnd` titleize Doc.physics]

-- Some of these course names are repeated between examples, could potentially
-- be abstracted out.

------------------------------
-- 3.3 : System Constraints --
------------------------------

---------------------------------------------
-- Section 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

-- Completely general except for solar water heating tank (object of analysis)
-- and similar between all examples; can be abstracted out.

-- The swhsPCM reference at the end would be better if singular, but concept
-- is plural.

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

probDescIntro :: Sentence
probDescIntro = foldlSent_ [S "investigate the effect" `sOf` S "employing",
  short phsChgMtrl, S "within a", phrase sWHT]

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

-----------------------------------------
-- 4.1.2 : Physical System Description --
-----------------------------------------

-----------------------------
-- 4.1.3 : Goal Statements --
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

-- Theory has to be RelationChunk....
-- No way to include "Source" or "Ref. By" sections?

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

genDefDeriv3, genDefDeriv5, genDefDeriv7, genDefDeriv11 :: Contents

genDefDeriv1 :: ConceptChunk -> UnitalChunk -> Contents
genDefDeriv1 roc tem = foldlSPCol [S "Detailed derivation of simplified",
  phrase roc, S "of", phrase tem]

genDefDeriv2 :: LabelledContent -> UnitalChunk -> Contents
genDefDeriv2 t1ct vo = foldlSPCol [S "Integrating", makeRef2S t1ct,
  S "over a", phrase vo, sParen (ch vo) `sC` S "we have"]

genDefDeriv3 = eqUnR'
  (negate (intAll (eqSymb vol) (sy gradient $. sy thFluxVect)) +
  intAll (eqSymb vol) (sy volHtGen) $=
  intAll (eqSymb vol) (sy density * sy heatCapSpec * pderiv (sy temp) time))

genDefDeriv4 :: ConceptChunk -> DefinedQuantityDict -> UnitalChunk -> UnitalChunk ->
  DefinedQuantityDict -> ConceptChunk -> Contents
genDefDeriv4 gaussdiv su vo tfv unv un = foldlSPCol [S "Applying", titleize gaussdiv,
  S "to the first term over", (phrase su +:+ ch su `ofThe` phrase vo) `sC`
  S "with", ch tfv, S "as the", phrase tfv, S "for the",
  phrase surface `sAnd` ch unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

genDefDeriv5 = eqUnR'
  (negate (intAll (eqSymb surface) (sy thFluxVect $. sy uNormalVect)) +
  intAll (eqSymb vol) (sy volHtGen) $= 
  intAll (eqSymb vol) (sy density * sy heatCapSpec * pderiv (sy temp) time))

genDefDeriv6 :: UnitalChunk -> UnitalChunk -> Contents
genDefDeriv6 vo vhg = foldlSPCol [S "We consider an arbitrary" +:+.
  phrase vo, S "The", phrase vhg, S "is assumed constant. Then",
  sParen $ S $ show (1 :: Integer), S "can be written as"]

genDefDeriv7 = eqUnR'
  (sy htFluxIn * sy inSA - sy htFluxOut *
  sy outSA + sy volHtGen * sy vol $= 
  intAll (eqSymb vol) (sy density * sy heatCapSpec * pderiv (sy temp) time))

genDefDeriv10 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> Contents
genDefDeriv10 den ma vo = foldlSPCol [S "Using the fact that", ch den :+:
  S "=" :+: ch ma :+: S "/" :+: ch vo `sC` S "(2) can be written as"]

genDefDeriv11 = eqUnR'
  (sy mass * sy heatCapSpec * deriv (sy temp)
  time $= sy htFluxIn * sy inSA - sy htFluxOut
  * sy outSA + sy volHtGen * sy vol)

-- Created a unitalChunk for "S"... should I add it to table of symbols?
-- Add references to above when available (assumptions, GDs)
-- Replace relevant derivs with the regular derivative when it is available

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------

dataDefIntroEnd :: Sentence
dataDefIntroEnd = foldlSent [S "The dimension of each",
  phrase quantity, S "is also given"]

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

iModSubpar :: NamedChunk -> UncertQ -> UncertQ -> UncertQ -> ConceptChunk
  -> [Contents]
iModSubpar sol temw tempcm epcm pc = [foldlSP [S "The goals", foldlList Comma List $ map S
  ["GS1", "GS2", "GS3", "GS4"], S "are solved by" +:+. foldlList Comma List -- hardcoded GSs because Goals are not implemented yet
  [makeRef2S eBalanceOnWtr, makeRef2S eBalanceOnPCM, makeRef2S heatEInWtr, makeRef2S heatEInPCM], 
  S "The", plural sol, S "for", makeRef2S eBalanceOnWtr `sAnd` makeRef2S eBalanceOnPCM, 
  S "are coupled since the", phrase sol, S "for", ch temw `sAnd` ch tempcm +:+. S "depend on one another", 
  makeRef2S heatEInWtr, S "can be solved once", makeRef2S eBalanceOnWtr, S "has been solved. The", 
  phrase sol `sOf` makeRef2S eBalanceOnPCM `sAnd` makeRef2S heatEInPCM, S "are also coupled, since the", 
  phrase tempcm `sAnd` phrase epcm, S "depend on the", phrase pc]]

iMod1Para :: UnitalChunk -> ConceptChunk -> [Contents]
iMod1Para en wa = [foldlSPCol [S "Derivation of the",
  phrase en, S "balance on", phrase wa]]

iMod1Sent2 :: DataDefinition -> DataDefinition -> UnitalChunk ->
  UnitalChunk -> [Sentence]
iMod1Sent2 d1hf d2hf hfc hfp = [S "Using", makeRef2S d1hf `sAnd`
  makeRef2S d2hf, S "for", ch hfc `sAnd`
  ch hfp, S "respectively, this can be written as"]

iMod1Sent3 :: UnitalChunk -> UncertQ -> [Sentence]
iMod1Sent3 wm hcw = [S "Dividing (3) by", ch wm :+: ch hcw `sC`
  S "we obtain"]

iMod1Sent4 :: CI -> UncertQ -> UncertQ -> [Sentence]
iMod1Sent4 rs chtc csa = [S "Factoring the negative sign out of",
  S "second term" `ofThe` short rs,
  S "of", titleize equation,
  S "(4) and multiplying it by",
  ch chtc :+: ch csa :+: S "/" :+:
  ch chtc :+: ch csa, S "yields"]

iMod1Sent5 :: [Sentence]
iMod1Sent5 = [S "Which simplifies to"]

iMod1Sent6 :: [Sentence]
iMod1Sent6 = [S "Setting",
  (E $ sy tauW $= (sy wMass * sy htCapW) / (sy coilHTC * sy coilSA)) `sAnd`
  (E $ sy eta $= (sy pcmHTC * sy pcmSA) / (sy coilHTC * sy coilSA)) `sC`
  titleize equation, S "(5) can be written as"]

iMod1Sent7 :: [Sentence]
iMod1Sent7 = [S "Finally, factoring out", (E $ 1 / sy tauW) `sC` 
  S "we are left with the governing", short ode, S "for", makeRef2S eBalanceOnWtr]

iMod1Eqn1, iMod1Eqn2, iMod1Eqn3, iMod1Eqn4, iMod1Eqn5,
  iMod1Eqn6, iMod1Eqn7 :: Expr

iMod1Eqn1 = sy wMass * sy htCapW * deriv (sy tempW) time $=
  sy htFluxC * sy coilSA - sy htFluxP * sy pcmSA

iMod1Eqn2 = sy wMass * sy htCapW * deriv (sy tempW) time $=
  sy coilHTC * sy coilSA * (sy tempC - sy tempW) -
  sy pcmHTC * sy pcmSA * (sy tempW - sy tempPCM)

iMod1Eqn3 = deriv (sy tempW) time $= (sy coilHTC *
  sy coilSA) / (sy wMass * sy htCapW) * (sy tempC -
  sy tempW) - (sy pcmMass * sy pcmSA) / (sy wMass *
  sy htCapW) * (sy tempW - sy tempPCM)

iMod1Eqn4 = deriv (sy tempW) time $= (sy coilHTC *
  sy coilSA) / (sy wMass * sy htCapW) * (sy tempC - sy tempW) +
  ((sy coilHTC * sy coilSA) / (sy coilHTC * sy coilSA)) *
  ((sy pcmHTC * sy pcmSA) / (sy wMass * sy htCapW)) *
  (sy tempPCM - sy tempW)

iMod1Eqn5 = deriv (sy tempW) time $= (sy coilHTC *
  sy coilSA) / (sy wMass * sy htCapW) * (sy tempC - sy tempW) +
  ((sy pcmHTC * sy pcmSA) / (sy coilHTC * sy coilSA)) *
  ((sy coilHTC * sy coilSA) / (sy wMass * sy htCapW)) *
  (sy tempPCM - sy tempW)

iMod1Eqn6 = deriv (sy tempW) time $= (1 / sy tauW) *
  (sy tempC - sy tempW) + (sy eta / sy tauW) *
  (sy tempPCM - sy tempW)

iMod1Eqn7 = deriv (sy tempW) time $= (1 / sy tauW) *
  ((sy tempC - sy tempW) + sy eta * (sy tempPCM -
  sy tempW))

-- Should "energy balance" be a concept?
-- Add IM, GD, A, and EqnBlock references when available
-- Replace derivs with regular derivative when available
-- Fractions in paragraph?

iMod2Sent1 :: DataDefinition -> UnitalChunk -> [Sentence]
iMod2Sent1 d2hfp hfp = [S "Using", makeRef2S d2hfp, S "for", 
  ch hfp `sC` S "this", phrase equation, S "can be written as"]

iMod2Sent2 :: [Sentence]
iMod2Sent2 = [S "Dividing by", ch pcmMass :+: ch htCapSP,
  S "we obtain"]

iMod2Sent3 :: [Sentence]
iMod2Sent3 = [S "Setting", ch tauSP :+: S "=" :+: ch pcmMass :+: 
  ch htCapSP :+: S "/" :+: ch pcmHTC :+: ch pcmSA `sC`
  S "this can be written as"]

iMod2Eqn1, iMod2Eqn2, iMod2Eqn3, iMod2Eqn4 :: Expr

iMod2Eqn1 = sy pcmMass * sy htCapSP * deriv (sy tempPCM)
  time $= sy htFluxP * sy pcmSA

iMod2Eqn2 = sy pcmMass * sy htCapSP * deriv (sy tempPCM)
  time $= sy pcmHTC * sy pcmSA * (sy tempW - sy tempPCM)

iMod2Eqn3 = deriv (sy tempPCM) time $= (sy pcmHTC *
  sy pcmSA) / (sy pcmMass * sy htCapSP) * (sy tempW - sy tempPCM)

iMod2Eqn4 = deriv (sy tempPCM) time $= (1 / sy tauSP) *
  (sy tempW - sy tempPCM)

-- Add GD, A, and EqnBlock references when available
-- FIXME: Replace derivs with regular derivative when available
-- derivative notation in paragraph?

----------------------------
-- 4.2.6 Data Constraints --
----------------------------

-- I do not think Table 2 will end up being necessary for the Drasil version
---- The info from table 2 will likely end up in table 1.
dataContMid :: Sentence
dataContMid = foldlSent [S "The", phrase column, S "for", phrase software,
  plural constraint, S "restricts the range of",
  plural input_, S "to reasonable", plural value]

dataConTail :: Sentence
dataConTail = dataContMid :+:
  dataContFooter quantity surArea vol thickness phsChgMtrl

------------------------------
-- Data Constraint: Table 1 --
------------------------------

dataContFooter :: NamedChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  CI -> Sentence
dataContFooter qua sa vo htcm pcmat = foldlSent_ $ map foldlSent [

  [sParen (S "*"), S "These", plural qua, S "cannot be equal to zero" `sC`
  S "or there will be a divide by zero in the", phrase model],

  [sParen (S "+"), S "These", plural qua, S "cannot be zero" `sC`
  S "or there would be freezing", sParen (makeRef2S assumpPIS)],

  [sParen (S "++"), S "The", plural constraint, S "on the", phrase sa,
  S "are calculated by considering the", phrase sa, S "to", phrase vo +:+.
  S "ratio", S "The", phrase assumption, S "is that the lowest ratio is",
  (S $ show (1 :: Integer)) `sAnd`
  S "the highest possible is", E (2 / sy htcm) `sC` S "where",
  E $ sy htcm, S "is the thickness of a", Quote (S "sheet"), S "of" +:+.
  short pcmat, S "A thin sheet has the greatest", phrase sa, S "to",
  phrase vo, S "ratio"],

  [sParen (S "**"), S "The", phrase constraint, S "on the maximum", 
  phrase time, S "at the end of the simulation is the total number of seconds",
  S "in one day"]
  
  ]

------------------------------
-- Data Constraint: Table 2 --
------------------------------

-- See Section 8 - Specification Parameter Values for table 3 from case study

------------------------------
-- Data Constraint: Table 3 --
------------------------------

----------------------------------------------
-- 4.2.7 : Properties of A Correct Solution --
----------------------------------------------

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------
---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------
--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------
--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

-------------------------------------------------
-- Section 8 :  Specification Parameter Values --
-------------------------------------------------
----------------------------
-- Section 9 : References --
----------------------------
