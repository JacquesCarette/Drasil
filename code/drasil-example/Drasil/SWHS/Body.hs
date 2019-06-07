module Drasil.SWHS.Body where

import Language.Drasil hiding (organization, section, sec)
import Language.Drasil.Code (CodeSpec, codeSpec)
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
  Field(..), Fields, LFunc (TermExcept), Literature (Doc', Lit), IntroSec (IntroProg),
  IntroSub(IChar, IOrgSec, IPurpose, IScope), RefSec (RefProg), 
  RefTab (TAandA, TUnits), TSIntro (SymbConvention, SymbOrder, TSPurpose),
  ReqrmntSec(..), ReqsSub(..), SSDSub(..), SolChSpec (SCSProg), SSDSec(..), 
  InclUnits(..), DerivationDisplay(..), SCSSub(..), Verbosity(..),
  TraceabilitySec(TraceabilityProg), LCsSec(..), UCsSec(..),
  GSDSec(..), GSDSub(..),
  dataConstraintUncertainty, intro, mkDoc,
  mkEnumSimpleD, outDataConstTbl, physSystDesc, goalStmtF, termDefnF, 
  traceGIntro, tsymb'', getDocDesc, egetDocDesc, ciGetDocDesc, generateTraceMap,
  generateTraceMap', getTraceMapFromTM, getTraceMapFromGD, getTraceMapFromDD, 
  getTraceMapFromIM, getSCSSub, generateTraceTable, physSystDescriptionLabel)
import qualified Drasil.DocLang.SRS as SRS (likeChg, probDesc, unlikeChg, inModel)

import Data.Drasil.Concepts.Thermodynamics (thermocon)
import Data.Drasil.Concepts.Documentation as Doc (assumption, column, condition, constraint, 
  content, dataConst, datum, definition, document, environment, goalStmt, information, 
  input_, item, likelyChg, model, organization, output_, physical, physics, physSyst,
  problem, property, purpose, quantity, reference, requirement, section_, software,
  softwareSys, srs, srsDomains, sysCont, system, traceyGraph, traceyMatrix, user, value,
  variable, doccon, doccon')
import Data.Drasil.IdeaDicts as Doc (genDefn, inModel, thModel)
import qualified Data.Drasil.IdeaDicts as Doc (dataDefn)
import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Concepts.Education (calculus, educon, engineering)
import Data.Drasil.Concepts.Math (de, equation, ode, unit_, mathcon, mathcon')
import Data.Drasil.Concepts.Software (program, softwarecon, correctness,
  understandability, reusability, maintainability, verifiability)
import Data.Drasil.Concepts.Physics (physicCon)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty, physicalcon)
import Data.Drasil.Software.Products (sciCompS, compPro, prodtcon)
import Data.Drasil.Quantities.Math (gradient, surface, uNormalVect, surArea)
import Data.Drasil.Quantities.PhysicalProperties (density, mass, vol)
import Data.Drasil.Quantities.Physics (energy, time, physicscon)
import Data.Drasil.Quantities.Thermodynamics (heatCapSpec, latentHeat, temp)

import Data.Drasil.People (brooks, spencerSmith, thulasi)
import Data.Drasil.SentenceStructures (showingCxnBw)
import Data.Drasil.SI_Units (metre, kilogram, second, centigrade, joule, watt,
  fundamentals, derived, m_2, m_3)
import Data.Drasil.Utils (enumSimple, itemRefToSent, makeTMatrix, eqUnR', noRefs)
import qualified Data.Drasil.Concepts.Thermodynamics as CT (heatTrans,
  thermalConduction, htFlux, heatCapSpec, thermalEnergy, htTransTheo,
  thermalAnalysis, enerSrc)

import Drasil.SWHS.Assumptions (assumpPIS, assumptions)
import Drasil.SWHS.Changes (likelyChgs, unlikelyChgs)
import Drasil.SWHS.Concepts (acronymsFull, progName, sWHT, water, phsChgMtrl,
  coil, tank, transient, swhsPCM, phase_change_material, tank_pcm, con)
import Drasil.SWHS.DataDefs (dataDefs, qDefs)
import Drasil.SWHS.DataDesc (inputMod)
import Drasil.SWHS.GenDefs (genDefs)
import Drasil.SWHS.Goals (goals)
import Drasil.SWHS.IMods (eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM,
  iMods, instModIntro)
import Drasil.SWHS.References (parnas1972, parnasClements1984, citations)
import Drasil.SWHS.Requirements (dataConTable1, funcReqs, inputInitQuantsTable,
  nfRequirements, propsDeriv)
import Drasil.SWHS.TMods (consThermE, sensHtE, latentHtE, tMods)
import Drasil.SWHS.Unitals (coil_HTC, coil_SA, eta, htCap_S_P, htCap_W,
  ht_flux_C, ht_flux_P, ht_flux_in, ht_flux_out, in_SA, out_SA, pcm_E,
  pcm_HTC, pcm_SA, pcm_mass, specParamValList, constrained, inputs,
  outputs, symbols, symbolsAll, unitalChuncks, tau_S_P, tau_W, temp_C,
  temp_PCM, temp_W, thFluxVect, thickness, vol_ht_gen, w_E, w_mass, abs_tol, rel_tol, cons_tol)

-------------------------------------------------------------------------------

this_si :: [UnitDefn]
this_si = map unitWrapper [metre, kilogram, second] ++ 
  map unitWrapper [centigrade, joule, watt]
--Will there be a table of contents?

checkSi :: [UnitDefn]
checkSi = collectUnits symMap symbTT

authors :: Sentence
authors = S $ manyNames people

si :: SystemInformation
si = SI {
  _sys = swhsPCM,
  _kind = srs, 
  _authors = people,
  _quants = symbols,
  _concepts = symbTT,
  _definitions = qDefs,
  _datadefs = dataDefs,
  _inputs = map qw inputs,
  _outputs = map qw outputs,
  _defSequence = ([] :: [Block QDefinition]),
  _constraints = (constrained),
  _constants = [],
  _sysinfodb = symMap,
  _usedinfodb = usedDB,
   refdb = refDB
}

resourcePath :: String
resourcePath = "../../../datafiles/SWHS/"

symMap :: ChunkDB
symMap = cdb (qw heatEInPCM : symbolsAll) -- heatEInPCM ?
  (nw heatEInPCM : map nw symbols ++ map nw acronymsFull
  ++ map nw thermocon ++ map nw this_si ++ map nw [m_2, m_3] ++ map nw [abs_tol, rel_tol, cons_tol]
  ++ map nw physicscon ++ map nw doccon ++ map nw softwarecon ++ map nw doccon' ++ map nw con
  ++ map nw prodtcon ++ map nw physicCon ++ map nw mathcon ++ map nw mathcon' ++ map nw specParamValList
  ++ map nw fundamentals ++ map nw educon ++ map nw derived ++ map nw physicalcon ++ map nw unitalChuncks
  ++ [nw swhsPCM, nw algorithm] ++ map nw compcon ++ [nw materialProprty])
  (cw heatEInPCM : map cw symbols ++ srsDomains) -- FIXME: heatEInPCM?
  (this_si ++ [m_2, m_3]) label refBy
  dataDefn insModel genDef theory concIns
  section labCon

usedDB :: ChunkDB
usedDB = cdb (map qw symbTT) (map nw symbols ++ map nw acronymsFull ++ map nw checkSi)
 ([] :: [ConceptChunk]) checkSi label refBy dataDefn insModel genDef
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

people :: [Person]
people = [thulasi, brooks, spencerSmith]

mkSRS :: DocDesc
mkSRS = [RefSec $ RefProg intro [
    TUnits,
    tsymb'' tsymb_intro (TermExcept [uNormalVect]),
    TAandA],
  IntroSec $
    IntroProg (introP1 CT.enerSrc energy swhsPCM phsChgMtrl
    progName CT.thermalEnergy latentHeat unit_) (introP2 swhsPCM program
    progName)
    [IPurpose $ purpDoc swhsPCM progName,
     IScope (scopeReqs1 CT.thermalAnalysis tank_pcm) $
       scopeReqs2 temp CT.thermalEnergy water phsChgMtrl sWHT,
     IChar [] ((charReader1 CT.htTransTheo) ++ (charReader2 de)) [],
     IOrgSec orgDocIntro inModel (SRS.inModel [] [])
       $ orgDocEnd swhsPCM progName],
  GSDSec $ GSDProg2 
    [ SysCntxt [sysCntxtDesc progName, LlC sysCntxtFig, sysCntxtRespIntro progName, systContRespBullets]
    , UsrChars [userChars progName]
    , SystCons [] []
    ],
  SSDSec $
    SSDProg [SSDSubVerb probDescription
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
  LCsSec $ LCsProg likelyChgsList,
  UCsSec $ UCsProg unlikelyChgsList,
  TraceabilitySec $
    TraceabilityProg traceRefList traceTrailing (map LlC traceRefList ++
  (map UlC traceIntro2) ++
  [LlC traceFig1, LlC traceFig2]) [],
  AuxConstntSec $ AuxConsProg progName specParamValList,
  Bibliography]

code :: CodeSpec
code = codeSpec si [inputMod]

tsymb_intro :: [TSIntro]
tsymb_intro = [TSPurpose, SymbConvention
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
concIns = assumptions ++ likelyChgs ++ unlikelyChgs ++ funcReqs

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

probDescription :: Section
probDescription = SRS.probDesc [probDescIntro progName phsChgMtrl sWHT]
  [termAndDefn, physSystDescription, goalStates]

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

termAndDefn :: Section
termAndDefn = termDefnF Nothing [termAndDefnBullets]

-- Above paragraph is repeated in all examples, can be abstracted out. (Note:
-- GlassBR has an additional sentence with a reference at the end.)

termAndDefnBullets :: Contents
termAndDefnBullets = UlC $ ulcc $ Enumeration $ Bullet $ noRefs $ map tAndDMap
  [CT.htFlux, phase_change_material, CT.heatCapSpec,
  CT.thermalConduction, transient]

tAndDMap :: Concept c => c -> ItemType
tAndDMap c = Flat $ foldlSent [at_start c +: EmptyS, (c ^. defn)]

-- Structure of this list is same in all examples, probably can be automated.

-- Included heat flux and specific heat in NamedChunks even though they are
-- already in SWHSUnits

-----------------------------------------
-- 4.1.2 : Physical System Description --
-----------------------------------------

physSystDescription :: Section
physSystDescription = physSystDesc (short progName) fig_tank [physSystDescList, LlC fig_tank]

-- Above paragraph is general except for progName and figure. However, not
-- every example has a physical system. Also, the SSP example is different, so
-- this paragraph can not be abstracted out as is.

physSystDescList :: Contents
physSystDescList = LlC $ enumSimple physSystDescriptionLabel 1 (short physSyst) $ map foldlSent_ systDescList

systDescList :: [[Sentence]]
systDescList = [physSyst1 tank water, physSyst2 coil tank ht_flux_C,
  physSyst3 phsChgMtrl tank ht_flux_P]

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

goalStates :: Section
goalStates = goalStmtF (goalStateIntro temp_C temp_W temp_PCM) goalStateList

goalStateIntro :: (NamedIdea a, NamedIdea b, NamedIdea c) => a -> b -> c -> [Sentence]
goalStateIntro temc temw tempcm = [S "the" +:+ phrase temc,
  S "the initial" +:+ plural condition +:+ S "for the" +:+ phrase temw `andThe` phrase tempcm,
  S "the material" +:+ plural property]

-- 2 examples include this paragraph, 2 don't. The "givens" would need to be
-- abstracted out if this paragraph were to be abstracted out.

goalStateList :: [Contents]
goalStateList = mkEnumSimpleD goals

-- List structure is repeated between examples. (For all of these lists I am
-- imagining the potential for something like what was done with the lists in
-- MG, where you define goals, assumptions, physical system components, etc. in
-- separate files, import them and pass them as arguments to some "makeSRS"
-- function and the rest is automated.)

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
  s4_2_3_deriv_4 gauss_div surface vol thFluxVect uNormalVect unit_,
  s4_2_3_deriv_5,
  s4_2_3_deriv_6 vol vol_ht_gen,
  s4_2_3_deriv_7,
  s4_2_3_deriv_8 ht_flux_in ht_flux_out in_SA out_SA density heatCapSpec
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
--FIXME: add "(by A11)" in Physical Constraints of `temp_W` and `temp_PCM`?

outputConstraints :: [ConstrConcept]
outputConstraints = [temp_W, temp_PCM, w_E, pcm_E]

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

likelyChgsSect :: Section
likelyChgsSect = SRS.likeChg likelyChgsList []

likelyChgsList :: [Contents]
likelyChgsList = mkEnumSimpleD likelyChgs

--------------------------------
-- Section 6b : UNLIKELY CHANGES --
--------------------------------

unlikelyChgsSect :: Section
unlikelyChgsSect = SRS.unlikeChg unlikelyChgsList []

unlikelyChgsList :: [Contents]
unlikelyChgsList = mkEnumSimpleD unlikelyChgs

--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

traceRefList :: [LabelledContent]
traceRefList = [traceTableAll, traceTable1, traceTable2, traceTable3]

traceTableAll :: LabelledContent
traceTableAll = generateTraceTable si

traceTrailing :: [Sentence]
traceTrailing = [traceTrailing1, traceTrailing2, traceTrailing3]

traceInstaModel, traceData, traceFuncReq, traceLikelyChg, traceDataDefs, traceGenDefs,
  traceAssump, traceTheories :: [String]
  
traceDataRef, traceFuncReqRef, traceInstaModelRef, traceAssumpRef, traceTheoriesRef,
  traceDataDefRef, traceLikelyChgRef, traceGenDefRef :: [Sentence]

traceInstaModel = ["IM1", "IM2", "IM3", "IM4"]
traceInstaModelRef = map makeRef2S iMods --FIXME: iMods is a hack?

traceFuncReq = ["R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
  "R11"]
traceFuncReqRef = map makeRef2S funcReqs

traceData = ["Data Constraints"]
traceDataRef = [makeRef2S dataConTable1] --FIXME: Reference section?

traceAssump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
  "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19"]
traceAssumpRef = map makeRef2S assumptions

traceTheories = ["T1", "T2", "T3"]
traceTheoriesRef = map makeRef2S tMods

traceGenDefs = ["GD1", "GD2"]
traceGenDefRef = map makeRef2S genDefs --FIXME: genDefs is a hack?

traceDataDefs = ["DD1", "DD2", "DD3", "DD4", "DD5", "DD6"]
traceDataDefRef = map makeRef2S dataDefs

traceLikelyChg = ["LC1", "LC2", "LC3", "LC4", "LC5", "LC6"]
traceLikelyChgRef = map makeRef2S likelyChgs

{-Traceability Matrix 1-}

traceMRow1 :: [String]
traceMRow1 = traceTheories ++ traceGenDefs ++ traceDataDefs ++ traceInstaModel

traceMRowHeader1 :: [Sentence]
traceMRowHeader1 = zipWith itemRefToSent traceMRow1 
  (traceTheoriesRef ++ traceGenDefRef ++ traceDataDefRef ++ traceInstaModelRef)

traceMColumns1 :: [[String]]
traceMColumns1 = [trace1T1, trace1T2, trace1T3, trace1GD1, trace1GD2, trace1DD1,
  trace1DD2, trace1DD3, trace1DD4, trace1DD5, trace1DD6, trace1IM1, trace1IM2, trace1IM3, trace1IM4]

trace1T1, trace1T2, trace1T3, trace1GD1, trace1GD2, trace1DD1, trace1DD2,
  trace1DD3, trace1DD4, trace1DD5, trace1DD6, trace1IM1, trace1IM2, trace1IM3,
  trace1IM4 :: [String]

--list of each item that "X" item requires for traceability matrix
trace1T1 = []
trace1T2 = ["T3"]
trace1T3 = []
trace1GD1 = []
trace1GD2 = ["T1"]
trace1DD1 = ["GD1"]
trace1DD2 = ["GD1"]
trace1DD3 = []
trace1DD4 = []
trace1DD5 = []
trace1DD6 = ["DD3"]
trace1IM1 = ["GD2", "DD1", "DD2", "IM2"]
trace1IM2 = ["GD2", "DD2", "DD3", "DD4", "DD6", "IM1", "IM4"]
trace1IM3 = ["T2"]
trace1IM4 = ["T2", "T3", "DD2", "DD4", "DD5", "IM2"]

{-Traceability Matrix 2-}

traceMRow2 :: [String]
traceMRow2 = traceInstaModel ++ traceData ++ traceFuncReq

--column header
traceMRowHeader2 :: [Sentence]
traceMRowHeader2 = zipWith itemRefToSent traceMRow2 
  (traceInstaModelRef ++ traceDataRef ++ traceFuncReqRef)

--row header
traceMColHeader2 :: [Sentence]
traceMColHeader2 = zipWith itemRefToSent (traceInstaModel ++ traceFuncReq)
  (traceInstaModelRef ++ traceFuncReqRef)

traceMColumns2 :: [[String]]
traceMColumns2 = [trace2IM1, trace2IM2, trace2IM3, trace2IM4, trace2R1, 
  trace2R2, trace2R3, trace2R4, trace2R5, trace2R6, trace2R7, trace2R8, 
  trace2R9, trace2R10, trace2R11]

trace2IM1, trace2IM2, trace2IM3, trace2IM4, trace2R1, trace2R2,
  trace2R3, trace2R4, trace2R5, trace2R6, trace2R7, trace2R8, 
  trace2R9, trace2R10, trace2R11 :: [String]

--list of each item that "X" item requires for traceability matrix
trace2IM1 = ["IM2", "R1", "R2"]
trace2IM2 = ["IM1", "IM4", "R1", "R2"]
trace2IM3 = ["R1", "R2"]
trace2IM4 = ["IM2", "R1", "R2"]
trace2R1 = []
trace2R2 = ["R1"]
trace2R3 = ["Data Constraints"]
trace2R4 = ["IM1", "IM2", "R1", "R2"]
trace2R5 = ["IM1"]
trace2R6 = ["IM2"]
trace2R7 = ["IM3"]
trace2R8 = ["IM4"]
trace2R9 = ["IM3", "IM4"]
trace2R10 = ["IM2"]
trace2R11 = ["IM2"]

traceTable2 :: LabelledContent
traceTable2 = llcc (makeTabRef "Tracey1") $
  Table (EmptyS:traceMRowHeader2)
  (makeTMatrix (traceMColHeader2) (traceMColumns2) (traceMRow2))
  (showingCxnBw traceyMatrix
  (titleize' requirement `sAnd` titleize' inModel)) True

{-Traceability Matrix 3-}

traceMRow3 :: [String]
traceMRow3 = traceAssump

traceMRowHeader3, traceMColHeader3 :: [Sentence]
traceMRowHeader3 = zipWith itemRefToSent traceAssump traceAssumpRef

traceMColHeader3 = zipWith itemRefToSent
  (traceTheories ++ traceGenDefs ++ traceDataDefs ++ traceInstaModel ++ traceLikelyChg)
  (traceTheoriesRef ++ traceGenDefRef ++ traceDataDefRef ++ traceInstaModelRef ++ 
    traceLikelyChgRef)

traceMColumns3 :: [[String]]
traceMColumns3 = [trace3T1, trace3T2, trace3T3, trace3GD1, trace3GD2, trace3DD1,
  trace3DD2, trace3DD3, trace3DD4, trace3DD5, trace3DD6, trace3IM1, trace3IM2, trace3IM3, trace3IM4,
  trace3LC1, trace3LC2, trace3LC3, trace3LC4, trace3LC5, trace3LC6]

trace3T1, trace3T2, trace3T3, trace3GD1, trace3GD2, trace3DD1, trace3DD2, 
  trace3DD3, trace3DD4, trace3DD5, trace3DD6, trace3IM1, trace3IM2, trace3IM3, trace3IM4, trace3LC1,
  trace3LC2, trace3LC3, trace3LC4, trace3LC5, trace3LC6 :: [String]

trace3T1  = ["A1"]
trace3T2  = []
trace3T3  = []
trace3GD1 = ["A2"]
trace3GD2 = ["A3", "A4", "A5", "A6"]
trace3DD1 = ["A7", "A8", "A9"]
trace3DD2 = ["A3", "A4", "A10"]
trace3DD3 = []
trace3DD4 = []
trace3DD5 = []
trace3DD6 = []
trace3IM1 = ["A11", "A12", "A14", "A15", "A16", "A19"]
trace3IM2 = ["A12", "A13", "A16", "A17", "A18"]
trace3IM3 = ["A14", "A19"]
trace3IM4 = ["A13", "A18"]
trace3LC1 = ["A4"]
trace3LC2 = ["A8"]
trace3LC3 = ["A9"]
trace3LC4 = ["A11"]
trace3LC5 = ["A12"]
trace3LC6 = ["A15"]


-- These matrices can probably be generated automatically when enough info is
-- abstracted out.

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
  at_start sp, S "improve over the traditional",
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
charReader2 diffeq = [(plural diffeq) +:+
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

probDescIntro :: CI -> CI -> ConceptChunk -> Contents
probDescIntro pro pcmat sw = foldlSP [short pro, S "is a", phrase compPro,
  S "developed to investigate the effect of",
  S "employing", short pcmat, S "within a", phrase sw]

-- section is very different between all examples

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

-----------------------------------------
-- 4.1.2 : Physical System Description --
-----------------------------------------

physSyst1 :: ConceptChunk -> ConceptChunk -> [Sentence]
physSyst1 ta wa = [at_start ta, S "containing" +:+. phrase wa]
--
physSyst2 :: ConceptChunk -> ConceptChunk -> UnitalChunk -> [Sentence]
physSyst2 co ta hfc = [at_start co, S "at bottom of" +:+. phrase ta,
  sParen (ch hfc +:+ S "represents the" +:+. phrase hfc)]
--
physSyst3 :: CI -> ConceptChunk -> UnitalChunk -> [Sentence]
physSyst3 pcmat ta hfp = [short pcmat, S "suspended in" +:+. phrase ta,
  sParen (ch hfp +:+ S "represents the" +:+. phrase hfp)]

-- Structure of list would be same between examples but content is completely
-- different

fig_tank :: LabelledContent
fig_tank = llcc (makeFigRef "Tank") $ fig (
  foldlSent_ [at_start sWHT `sC` S "with", phrase ht_flux_C, S "of",
  ch ht_flux_C `sAnd` phrase ht_flux_P, S "of", ch ht_flux_P])
  $ resourcePath ++ "Tank.png"

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
  ((negate (int_all (eqSymb vol) ((sy gradient) $. (sy thFluxVect)))) +
  (int_all (eqSymb vol) (sy vol_ht_gen)) $=
  (int_all (eqSymb vol) ((sy density) * (sy heatCapSpec) * pderiv (sy temp) time)))

genDefDeriv4 :: ConceptChunk -> DefinedQuantityDict -> UnitalChunk -> UnitalChunk ->
  DefinedQuantityDict -> ConceptChunk -> Contents
genDefDeriv4 gaussdiv su vo tfv unv un = foldlSPCol [S "Applying", titleize gaussdiv,
  S "to the first term over", (phrase su +:+ ch su `ofThe` phrase vo) `sC`
  S "with", ch tfv, S "as the", phrase tfv, S "for the",
  phrase surface `sAnd` ch unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

genDefDeriv5 = eqUnR'
  ((negate (int_all (eqSymb surface) ((sy thFluxVect) $. (sy uNormalVect)))) +
  (int_all (eqSymb vol) (sy vol_ht_gen)) $= 
  (int_all (eqSymb vol) ((sy density) * (sy heatCapSpec) * pderiv (sy temp) time)))

genDefDeriv6 :: UnitalChunk -> UnitalChunk -> Contents
genDefDeriv6 vo vhg = foldlSPCol [S "We consider an arbitrary" +:+.
  phrase vo, S "The", phrase vhg, S "is assumed constant. Then",
  sParen $ S $ show (1 :: Integer), S "can be written as"]

genDefDeriv7 = eqUnR'
  ((sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out) *
  (sy out_SA) + (sy vol_ht_gen) * (sy vol) $= 
  (int_all (eqSymb vol) ((sy density) * (sy heatCapSpec) * pderiv (sy temp) time)))

genDefDeriv10 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> Contents
genDefDeriv10 den ma vo = foldlSPCol [S "Using the fact that", ch den :+:
  S "=" :+: ch ma :+: S "/" :+: ch vo `sC` S "(2) can be written as"]

genDefDeriv11 = eqUnR'
  ((sy mass) * (sy heatCapSpec) * deriv (sy temp)
  time $= (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out)
  * (sy out_SA) + (sy vol_ht_gen) * (sy vol))

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
iMod1Sent2 d1hf d2hf hfc hfp = [S "Using", (makeRef2S d1hf) `sAnd`
  (makeRef2S d2hf), S "for", ch hfc `sAnd`
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
  (E $ sy tau_W $= (sy w_mass * sy htCap_W) / (sy coil_HTC * sy coil_SA)) `sAnd`
  (E $ sy eta $= (sy pcm_HTC * sy pcm_SA) / (sy coil_HTC * sy coil_SA)) `sC`
  titleize equation, S "(5) can be written as"]

iMod1Sent7 :: [Sentence]
iMod1Sent7 = [S "Finally, factoring out", (E $ 1 / sy tau_W) `sC` 
  S "we are left with the governing", short ode, S "for", makeRef2S eBalanceOnWtr]

iMod1Eqn1, iMod1Eqn2, iMod1Eqn3, iMod1Eqn4, iMod1Eqn5,
  iMod1Eqn6, iMod1Eqn7 :: Expr

iMod1Eqn1 = ((sy w_mass) * (sy htCap_W) * deriv (sy temp_W) time $=
  (sy ht_flux_C) * (sy coil_SA) - (sy ht_flux_P) * (sy pcm_SA))

iMod1Eqn2 = ((sy w_mass) * (sy htCap_W) * deriv (sy temp_W) time $=
  (sy coil_HTC) * (sy coil_SA) * ((sy temp_C) - (sy temp_W)) -
  (sy pcm_HTC) * (sy pcm_SA) * ((sy temp_W) - (sy temp_PCM)))

iMod1Eqn3 = (deriv (sy temp_W) time $= ((sy coil_HTC) *
  (sy coil_SA)) / ((sy w_mass) * (sy htCap_W)) * ((sy temp_C) -
  (sy temp_W)) - ((sy pcm_mass) * (sy pcm_SA)) / ((sy w_mass) *
  (sy htCap_W)) * ((sy temp_W) - (sy temp_PCM)))

iMod1Eqn4 = (deriv (sy temp_W) time $= ((sy coil_HTC) *
  (sy coil_SA)) / ((sy w_mass) * (sy htCap_W)) * ((sy temp_C) - (sy temp_W)) +
  (((sy coil_HTC) * (sy coil_SA)) / ((sy coil_HTC) * (sy coil_SA))) *
  (((sy pcm_HTC) * (sy pcm_SA)) / ((sy w_mass) * (sy htCap_W))) *
  ((sy temp_PCM) - (sy temp_W)))

iMod1Eqn5 = (deriv (sy temp_W) time $= ((sy coil_HTC) *
  (sy coil_SA)) / ((sy w_mass) * (sy htCap_W)) * ((sy temp_C) - (sy temp_W)) +
  (((sy pcm_HTC) * (sy pcm_SA)) / ((sy coil_HTC) * (sy coil_SA))) *
  (((sy coil_HTC) * (sy coil_SA)) / ((sy w_mass) * (sy htCap_W))) *
  ((sy temp_PCM) - (sy temp_W)))

iMod1Eqn6 = (deriv (sy temp_W) time $= (1 / (sy tau_W)) *
  ((sy temp_C) - (sy temp_W)) + ((sy eta) / (sy tau_W)) *
  ((sy temp_PCM) - (sy temp_W)))

iMod1Eqn7 = (deriv (sy temp_W) time $= (1 / (sy tau_W)) *
  (((sy temp_C) - (sy temp_W)) + (sy eta) * ((sy temp_PCM) -
  (sy temp_W))))

-- Should "energy balance" be a concept?
-- Add IM, GD, A, and EqnBlock references when available
-- Replace derivs with regular derivative when available
-- Fractions in paragraph?

iMod2Sent1 :: DataDefinition -> UnitalChunk -> [Sentence]
iMod2Sent1 d2hfp hfp = [S "Using", makeRef2S d2hfp, S "for", 
  ch hfp `sC` S "this", phrase equation, S "can be written as"]

iMod2Sent2 :: [Sentence]
iMod2Sent2 = [S "Dividing by", ch pcm_mass :+: ch htCap_S_P,
  S "we obtain"]

iMod2Sent3 :: [Sentence]
iMod2Sent3 = [S "Setting", ch tau_S_P :+: S "=" :+: ch pcm_mass :+: 
  ch htCap_S_P :+: S "/" :+: ch pcm_HTC :+: ch pcm_SA `sC`
  S "this can be written as"]

iMod2Eqn1, iMod2Eqn2, iMod2Eqn3, iMod2Eqn4 :: Expr

iMod2Eqn1 = ((sy pcm_mass) * (sy htCap_S_P) * deriv (sy temp_PCM)
  time $= (sy ht_flux_P) * (sy pcm_SA))

iMod2Eqn2 = ((sy pcm_mass) * (sy htCap_S_P) * deriv (sy temp_PCM)
  time $= (sy pcm_HTC) * (sy pcm_SA) * ((sy temp_W) - (sy temp_PCM)))

iMod2Eqn3 = (deriv (sy temp_PCM) time $= ((sy pcm_HTC) *
  (sy pcm_SA)) / ((sy pcm_mass) * (sy htCap_S_P)) * ((sy temp_W) - (sy temp_PCM)))

iMod2Eqn4 = (deriv (sy temp_PCM) time $= (1 / (sy tau_S_P)) *
  ((sy temp_W) - (sy temp_PCM)))

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
  (dataContFooter quantity surArea vol thickness phsChgMtrl)

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

renameList1, renameList2 :: [CI]
renameList1  = [thModel, genDefn, Doc.dataDefn, inModel, likelyChg, assumption]
renameList2  = [inModel, requirement, dataConst]

traceTrailing1, traceTrailing2, traceTrailing3 :: Sentence

traceTrailing1 = foldlSent [foldlList Comma List $ map plural (take 4 renameList1), 
  S "with each other"]

traceTrailing2 = foldlSent [foldlList Comma List $ map plural renameList2, 
  S "on each other"]

traceTrailing3 = foldlSent_ [foldlList Comma List $ map plural (take 5 renameList1),
  S "on the", plural assumption]

traceTable1 :: LabelledContent
traceTable1 = llcc (makeTabRef "Tracey2") $ Table
  (EmptyS:traceMRowHeader1)
  (makeTMatrix (traceMRowHeader1) (traceMColumns1) (traceMRow1))
  (showingCxnBw traceyMatrix
  (titleize' item +:+ S "of Different" +:+ titleize' section_)) True

traceTable3 :: LabelledContent
traceTable3 = llcc (makeTabRef "Tracey3") $ Table
  (EmptyS:traceMRowHeader3)
  (makeTMatrix traceMColHeader3 traceMColumns3 traceMRow3)
  (showingCxnBw traceyMatrix (titleize' assumption `sAnd` S "Other" +:+
  titleize' item)) True

-- These matrices can probably be generated automatically when enough info is
-- abstracted out.

------------------------
-- Traceabilty Graphs --
------------------------

traceIntro2 :: [UnlabelledContent]
traceIntro2 = traceGIntro [traceFig1, traceFig2]

  [foldlSent [foldlList Comma List $ map plural renameList1, S "on each other"],

  foldlSent_ [foldlList Comma List $ map plural renameList2, S "on each other"]]

traceFig1 :: LabelledContent
traceFig1 = llcc (makeFigRef "TraceyA") $ fig (showingCxnBw traceyGraph (titleize' item +:+
  S "of Different" +:+ titleize' section_)) $ resourcePath ++ "ATrace.png"

traceFig2 :: LabelledContent
traceFig2 = llcc (makeFigRef "TraceyR") $ fig (showingCxnBw traceyGraph (foldlList Comma List
  $ map titleize' renameList2)) $ resourcePath ++ "RTrace.png"

-------------------------------------------------
-- Section 8 :  Specification Parameter Values --
-------------------------------------------------
----------------------------
-- Section 9 : References --
----------------------------
