module Drasil.NoPCM.Body where

import Language.Drasil hiding (constraints, section, sec)
import Language.Drasil.Code (CodeSpec, codeSpec)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block(Parallel), ChunkDB, RefbyMap, ReferenceDB,
  SystemInformation(SI), TraceMap, ccss, cdb, collectUnits, generateRefbyMap,
  rdb, refdb, _authors, _concepts, _constants, _constraints, _datadefs,
  _definitions, _defSequence, _inputs, _kind, _outputs, _quants, _sys,
  _sysinfodb, _usedinfodb)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import Utils.Drasil

import Control.Lens ((^.))
import qualified Data.Map as Map
import Data.Drasil.People (thulasi)
import Data.Drasil.Utils (enumSimple, itemRefToSent, makeTMatrix, noRefs)

import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation as Doc (assumption, content,
  definition, doccon, doccon', document, goal, information, item,
  material_, model, physSyst, problem, property, purpose, reference,
  requirement, srsDomains, traceyMatrix)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.IdeaDicts as Doc (inModel, thModel)
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Math (mathcon, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (physicalcon)
import Data.Drasil.Concepts.Physics (physicCon, physicCon')
import Data.Drasil.Concepts.Software (program, softwarecon)
import Data.Drasil.Concepts.Thermodynamics (enerSrc, thermalAnalysis, temp,
  thermalEnergy, htTransTheo, htFlux, heatCapSpec, thermalConduction, thermocon,
  phaseChange)

import qualified Data.Drasil.Concepts.Math as M (ode, de)
import qualified Data.Drasil.Quantities.Thermodynamics as QT (temp,
  heatCapSpec, htFlux, sensHeat)

import Data.Drasil.Quantities.Math (gradient, pi_, surface, uNormalVect)
import Data.Drasil.Quantities.PhysicalProperties (vol, mass, density)
import Data.Drasil.Quantities.Physics (time, energy, physicscon)

import Data.Drasil.Phrase (for)
import Data.Drasil.SentenceStructures (showingCxnBw, foldlSent_, foldlSent, foldlSP)
import Data.Drasil.Software.Products (compPro, prodtcon)
import Data.Drasil.SI_Units (metre, kilogram, second, centigrade, joule, watt,
  fundamentals, derived)

import qualified Drasil.DocLang.SRS as SRS (probDesc, inModel)
import Drasil.DocLang (DocDesc, Fields, Field(..), Verbosity(Verbose), 
  InclUnits(IncludeUnits), SCSSub(..), DerivationDisplay(..), SSDSub(..),
  SolChSpec(..), SSDSec(..), DocSection(..),
  IntroSec(IntroProg), IntroSub(IOrgSec, IScope, IChar, IPurpose), Literature(Lit, Doc'),
  ReqrmntSec(..), ReqsSub(FReqsSub, NonFReqsSub), LCsSec(..), UCsSec(..),
  RefSec(RefProg), RefTab(TAandA, TUnits), TraceabilitySec(TraceabilityProg),
  TSIntro(SymbOrder, SymbConvention, TSPurpose), dataConstraintUncertainty,
  inDataConstTbl, intro, mkDoc, mkEnumSimpleD, outDataConstTbl, physSystDesc,
  termDefnF, tsymb, valsOfAuxConstantsF, getDocDesc, egetDocDesc, generateTraceMap,
  getTraceMapFromTM, getTraceMapFromGD, getTraceMapFromDD, getTraceMapFromIM, getSCSSub,
  generateTraceTable, goalStmtF, physSystDescriptionLabel, generateTraceMap')

-- Since NoPCM is a simplified version of SWHS, the file is to be built off
-- of the SWHS libraries.  If the source for something cannot be found in
-- NoPCM, check SWHS.
import Drasil.SWHS.Body (charReader1, charReader2, dataContMid, genSystDesc, 
  orgDocIntro, physSyst1, physSyst2, traceIntro2, traceTrailing)
import Drasil.SWHS.Changes (likeChgTCVOD, likeChgTCVOL, likeChgTLH)
import Drasil.SWHS.Concepts (acronyms, coil, progName, sWHT, tank, transient, water, con)
import Drasil.SWHS.DataDefs (dd1HtFluxC, dd1HtFluxCQD)
import Drasil.SWHS.IMods (heatEInWtr)
import Drasil.SWHS.References (incroperaEtAl2007, koothoor2013, lightstone2012, 
  parnasClements1986, smithLai2005)
import Drasil.SWHS.Requirements (propsDerivNoPCM, nfRequirements)
import Drasil.SWHS.TMods (consThermE, sensHtE_template, PhaseChange(Liquid))
import Drasil.SWHS.Tables (inputInitQuantsTblabled)
import Drasil.SWHS.Unitals (coil_HTC, coil_HTC_max, coil_HTC_min, coil_SA, 
  coil_SA_max, deltaT, diam, eta, ht_flux_C, ht_flux_in, ht_flux_out, htCap_L, 
  htCap_W, htCap_W_max, htCap_W_min, htTransCoeff, in_SA, out_SA, 
  tank_length, tank_length_max, tank_length_min, tank_vol, tau, tau_W, temp_C, 
  temp_env, temp_W, thFluxVect, time_final, time_final_max, timeStep, 
  vol_ht_gen, w_density, w_density_max, w_density_min, w_E, w_mass, w_vol, 
  specParamValList, unitalChuncks, abs_tol, rel_tol, cons_tol)

import Drasil.NoPCM.Assumptions
import Drasil.NoPCM.Changes (likelyChgs, unlikelyChgs)
import Drasil.NoPCM.DataDesc (inputMod)
import Drasil.NoPCM.Definitions (srs_swhs, ht_trans)
import Drasil.NoPCM.GenDefs (genDefs)
import Drasil.NoPCM.Goals (goals)
import Drasil.NoPCM.IMods (eBalanceOnWtr, instModIntro)
import qualified Drasil.NoPCM.IMods as NoPCM(iMods)
import Drasil.NoPCM.Requirements (funcReqsList, reqs, dataConstListIn)
import Drasil.NoPCM.Unitals (temp_init)

-- This defines the standard units used throughout the document
this_si :: [UnitDefn]
this_si = map unitWrapper [metre, kilogram, second] ++ map unitWrapper [centigrade, joule, watt]

checkSi :: [UnitDefn]
checkSi = collectUnits symbMap symbTT 

-- This contains the list of symbols used throughout the document
symbols :: [DefinedQuantityDict]
symbols = pi_ : (map dqdWr units) ++ (map dqdWr constraints)
 ++ map dqdWr [temp_W, w_E]
 ++ [gradient, uNormalVect] ++ map dqdWr [surface]

resourcePath :: String
resourcePath = "../../../datafiles/NoPCM/"
  
symbolsAll :: [QuantityDict] --FIXME: Why is PCM (swhsSymbolsAll) here?
                               --Can't generate without SWHS-specific symbols like pcm_HTC and pcm_SA
                               --FOUND LOC OF ERROR: Instance Models
symbolsAll = map qw symbols ++ (map qw specParamValList) ++ 
  (map qw [coil_SA_max]) ++ (map qw [tau_W]) ++ (map qw [eta]) ++
  (map qw [abs_tol, rel_tol, cons_tol])

units :: [UnitaryConceptDict]
units = map ucw [density, tau, in_SA, out_SA,
  htCap_L, QT.htFlux, ht_flux_in, ht_flux_out, vol_ht_gen,
  htTransCoeff, mass, tank_vol, QT.temp, QT.heatCapSpec,
  deltaT, temp_env, thFluxVect, time, ht_flux_C,
  vol, w_mass, w_vol, tau_W, QT.sensHeat]

constraints :: [UncertQ]
constraints =  [coil_SA, htCap_W, coil_HTC, temp_init,
  time_final, tank_length, temp_C, timeStep, w_density, diam]
  -- w_E, temp_W

probDescription, termAndDefn, physSystDescription, goalStates, specParamVal :: Section


-------------------
--INPUT INFORMATION
-------------------

--------------------------------
--Section 1 : REFERENCE MATERIAL
--------------------------------
  
mkSRS :: DocDesc
mkSRS = [RefSec $ RefProg intro
  [TUnits,
  tsymb [TSPurpose, SymbConvention [Lit $ nw ht_trans, Doc' $ nw progName], SymbOrder],
  TAandA],
  IntroSec $ IntroProg (introStart enerSrc energy progName)
    (introEnd progName program)
  [IPurpose $ purpDoc progName,
  IScope (scopeReqStart thermalAnalysis sWHT) (scopeReqEnd temp thermalEnergy
    water),
  IChar [] ((charReader1 htTransTheo) ++ (charReader2 M.de)) [],
  IOrgSec orgDocIntro inModel (SRS.inModel [] []) $ orgDocEnd inModel M.ode progName],
  Verbatim genSystDesc,
  SSDSec $
    SSDProg [SSDSubVerb probDescription
    , SSDSolChSpec $ SCSProg
      [ Assumptions
      , TMs [] (Label : stdFields) theoretical_models
      , GDs [] ([Label, Units] ++ stdFields) genDefs ShowDerivation
      , DDs [] ([Label, Symbol, Units] ++ stdFields) [dd1HtFluxC] ShowDerivation
      , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields)
        NoPCM.iMods ShowDerivation
      , Constraints EmptyS dataConstraintUncertainty dataContMid
        [dataConstTable1, dataConstTable2]
      , CorrSolnPpties propsDerivNoPCM
      ]
    ],
  ReqrmntSec $ ReqsProg [
    FReqsSub funcReqsList,
    NonFReqsSub nfRequirements
  ],
  LCsSec $ LCsProg likelyChgsList,
  UCsSec $ UCsProg unlikelyChgsList,
  TraceabilitySec $
    TraceabilityProg traceRefList traceTrailing (map LlC traceRefList ++
  (map UlC traceIntro2)) []] ++
  map Verbatim [specParamVal] ++ [Bibliography]

label :: TraceMap
label = Map.union (generateTraceMap mkSRS) $ generateTraceMap' concIns
 
refBy :: RefbyMap
refBy = generateRefbyMap label

dataDefn :: [DataDefinition]
dataDefn = getTraceMapFromDD $ getSCSSub mkSRS

iMods :: [InstanceModel]
iMods = getTraceMapFromIM $ getSCSSub mkSRS

genDef :: [GenDefn]
genDef = getTraceMapFromGD $ getSCSSub mkSRS

theory :: [TheoryModel]
theory = getTraceMapFromTM $ getSCSSub mkSRS

concIns :: [ConceptInstance]
concIns =
 reqs ++ [likeChgTCVOD, likeChgTCVOL] ++ assumptions ++ likelyChgs ++
 [likeChgTLH] ++ unlikelyChgs

section :: [Section]
section = sec

labCon :: [LabelledContent]
labCon = [inputInitQuantsTblabled, dataConstTable1]

sec :: [Section]
sec = extractSection srs

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

si :: SystemInformation
si = SI {
  _sys = srs_swhs,
  _kind = Doc.srs,
  _authors = [thulasi],
  _quants = symbTT,
  _concepts = symbols,
  _definitions = [dd1HtFluxCQD],          --dataDefs
  _datadefs = [dd1HtFluxC],
  _inputs = (map qw constraints ++ map qw [temp_W, w_E] ++ 
    map qw [abs_tol, rel_tol, cons_tol]), --inputs ++ outputs?
  _outputs = (map qw [temp_W, w_E]),     --outputs
  _defSequence = [Parallel dd1HtFluxCQD []],
  _constraints = (map cnstrw constraints ++ map cnstrw [temp_W, w_E]),        --constrained
  _constants = [],
  _sysinfodb = symbMap,
  _usedinfodb = usedDB,
   refdb = refDB
}

refDB :: ReferenceDB
refDB = rdb referencesRefList concIns

code :: CodeSpec
code = codeSpec si [inputMod]
-- Sub interpolation mod into list when possible              ^

srs :: Document
srs = mkDoc mkSRS (for) si

symbMap :: ChunkDB
symbMap = cdb (symbolsAll) (map nw symbols ++ map nw acronyms ++ map nw thermocon
  ++ map nw physicscon ++ map nw doccon ++ map nw softwarecon ++ map nw doccon' ++ map nw con
  ++ map nw prodtcon ++ map nw physicCon ++ map nw physicCon' ++ map nw mathcon ++ map nw mathcon'
  ++ map nw specParamValList ++ map nw fundamentals ++ map nw educon ++ map nw derived 
  ++ map nw physicalcon ++ map nw unitalChuncks ++ [nw srs_swhs, nw algorithm, nw ht_trans] ++ map nw checkSi
  ++ map nw [abs_tol, rel_tol, cons_tol])
  (map cw symbols ++ srsDomains)
  this_si label refBy dataDefn iMods genDef theory
  concIns section labCon

usedDB :: ChunkDB
usedDB = cdb (map qw symbTT) (map nw symbols ++ map nw acronyms ++ map nw checkSi)
 ([] :: [ConceptChunk]) checkSi label refBy
 dataDefn iMods genDef theory concIns
 section labCon

printSetting :: PrintingInformation
printSetting = PI symbMap defaultConfiguration

symbTT :: [DefinedQuantityDict]
symbTT = ccss (getDocDesc mkSRS) (egetDocDesc mkSRS) symbMap

--------------------------
--Section 2 : INTRODUCTION
--------------------------

introStart :: ConceptChunk -> UnitalChunk -> CI-> Sentence
introStart es en pro = foldlSent [S "Due to increasing cost, diminishing",
  S "availability, and negative environmental impact of",
  S "fossil fuels, there is a higher demand for renewable",
  plural es `sAnd` phrase en +:+. S "storage technology", 
  at_start' pro, S "provide a novel way of storing", phrase en]

introEnd :: CI -> ConceptChunk -> Sentence
introEnd pro pr = foldlSent_ [EmptyS +:+. plural pro, S "The developed",
  phrase pr, S "will be referred to as", titleize pro,
  sParen (short pro)]

-----------------------------------
--Section 2.1 : PURPOSE OF DOCUMENT
-----------------------------------

purpDoc :: CI -> Sentence
purpDoc pro = foldlSent [S "The main", phrase purpose, S "of this",
  phrase document, S "is to describe the modelling of" +:+.
  phrase pro, S "The", plural Doc.goal `sAnd` plural thModel,
  S "used in the", short pro, S "code are provided, with an emphasis",
  S "on explicitly identifying", plural assumption, S "and unambiguous" +:+.
  plural definition, S "This", phrase document,
  S "is intended to be used as a", phrase reference,
  S "to provide ad hoc access to all", phrase information,
  S "necessary to understand and verify the" +:+. phrase model, S "The",
  short Doc.srs, S "is abstract because the", plural content, S "say what",
  phrase problem, S "is being solved, but do not say how to solve it"]

-------------------------------------
--Section 2.2 : SCOPE OF REQUIREMENTS
-------------------------------------

scopeReqStart :: ConceptChunk -> ConceptChunk -> Sentence
scopeReqStart ta sw = foldlSent_ [phrase ta, S "of a single", phrase sw]

scopeReqEnd :: ConceptChunk -> ConceptChunk -> ConceptChunk -> Sentence
scopeReqEnd tem te wa = foldlSent_ [S "predicts the",
  phrase tem `sAnd` phrase te,
  S "histories for the", phrase wa]

--------------------------------------------------
--Section 2.3 : CHARACTERISTICS Of INTENDED READER
--------------------------------------------------
          
---------------------------------------
--Section 2.4: ORGANIZATION OF DOCUMENT
---------------------------------------

orgDocEnd :: CI -> CI -> CI -> Sentence
orgDocEnd im_ od pro = foldlSent_ [S "The", phrase im_,
  S "to be solved is referred to as" +:+. makeRef2S eBalanceOnWtr,
  S "The", phrase im_, S "provides the",
  titleize od, sParen (short od), S "that model the"
  +:+. phrase pro, short pro, S "solves this", short od]

----------------------------------------
--Section 3 : GENERAL SYSTEM DESCRIPTION
----------------------------------------

--ALL OF THIS SECTION IS NOW PULLED FROM SWHS

--TODO: If/when system constraints recieves any content, add s3_3_intro

------------------------------
--Section 3.1 : SYSTEM CONTEXT
------------------------------
  
------------------------------------
--Section 3.2 : USER CHARACTERISTICS
------------------------------------

----------------------------------
--Section 3.3 : SYSTEM CONSTRAINTS
----------------------------------

--s3_3_intro = Paragraph $ EmptyS

--TODO: Placeholder value until content can be added

-----------------------------------------
--Section 4 : SPECIFIC SYSTEM DESCRIPTION
-----------------------------------------

--TODO: finish filling in the subsections

-----------------------------------
--Section 4.1 : PROBLEM DESCRIPTION
-----------------------------------

probDescription = SRS.probDesc [probDescIntro progName compPro water sWHT]
  [termAndDefn, physSystDescription, goalStates]

probDescIntro :: CI -> NamedChunk -> ConceptChunk -> ConceptChunk -> Contents
probDescIntro pro cp wa sw = foldlSP [getAcc pro, S "is a",
  phrase cp, S "developed to investigate",
  S "the heating of", phrase wa, S "in a", phrase sw]

termAndDefn = termDefnF Nothing [termAndDefnBullets]

termAndDefnBullets :: Contents
termAndDefnBullets = UlC $ ulcc $ Enumeration $ Bullet $ noRefs $ 
  map (\x -> Flat $
  at_start x :+: S ":" +:+ (x ^. defn))
  [htFlux, heatCapSpec, thermalConduction, transient]
  
physSystDescription = physSystDesc (getAcc progName) fig_tank
  [physSystDescList, LlC fig_tank]

fig_tank :: LabelledContent
fig_tank = llcc (makeFigRef "Tank") $ fig (at_start sWHT `sC` S "with" +:+ phrase htFlux +:+
  S "from" +:+ phrase coil `sOf` ch ht_flux_C)
  $ resourcePath ++ "TankWaterOnly.png"

physSystDescList :: Contents
physSystDescList = LlC $ enumSimple physSystDescriptionLabel 1 (short physSyst) $ map foldlSent_
  [physSyst1 tank water, physSyst2 coil tank ht_flux_C]

goalStates = goalStmtF (goalStatesIntro temp coil temp_W) goalStatesList

goalStatesIntro :: NamedIdea c => ConceptChunk -> ConceptChunk -> c -> [Sentence]
goalStatesIntro te co temw = [phrase te `ofThe` phrase co,
  S "the initial" +:+ phrase temw,
  S "the material" +:+ plural property]

goalStatesList :: [Contents]
goalStatesList = mkEnumSimpleD goals


------------------------------------------------------
--Section 4.2 : SOLUTION CHARACTERISTICS SPECIFICATION
------------------------------------------------------

theoretical_models :: [TheoryModel]
theoretical_models = [consThermE, sensHtE]

sensHtE :: TheoryModel
sensHtE = sensHtE_template Liquid sensHtEdesc

sensHtEdesc :: Sentence
sensHtEdesc = foldlSent [ch QT.sensHeat, S "occurs as long as the", phrase material_, S "does not reach a",
  phrase temp, S "where a", phrase phaseChange, S "occurs" `sC` S "as assumed in", makeRef2S assumpWAL]

--TODO: Implement physical properties of a substance

dataConstTable1 :: LabelledContent
dataConstTable1 = inDataConstTbl dataConstListIn
-- s4_2_6_table1 = Table [S "Var", titleize' physicalConstraint, titleize software +:+
  -- titleize' constraint, S "Typical" +:+ titleize value, titleize uncertainty]
  -- (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2), (\x -> x!!3), (\x -> x!!4)]
  -- data_constraint_conListIn) (titleize input_ +:+ titleize' variable) True

dataConstTable2 :: LabelledContent
dataConstTable2 = outDataConstTbl dataConstListOut
-- s4_2_6_table2 = Table [S "Var", titleize' physicalConstraint]
  -- (mkTable [(\x -> x!!0), (\x -> x!!1)] s4_2_6_conListOut)
  -- (titleize output_ +:+ titleize' variable) True

dataConstListOut :: [ConstrConcept]
dataConstListOut = [temp_W, w_E]

--------------------------
--Section 5 : REQUIREMENTS
--------------------------

-- in Requirements.hs

---------------------------------------
--Section 5.1 : FUNCTIONAL REQUIREMENTS
---------------------------------------

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

----------------------------
--Section 6 : LIKELY CHANGES
----------------------------
likelyChgsList :: [Contents]
likelyChgsList = mkEnumSimpleD $ [likeChgTCVOD, likeChgTCVOL] ++ likelyChgs ++ [likeChgTLH]

-------------------------------
--Section 6b : UNLIKELY CHANGES
-------------------------------
unlikelyChgsList :: [Contents]
unlikelyChgsList = mkEnumSimpleD unlikelyChgs

----------------------------------------------
--Section 7:  TRACEABILITY MATRICES AND GRAPHS
----------------------------------------------
traceTableAll :: LabelledContent
traceTableAll = generateTraceTable si

traceRefList :: [LabelledContent]
traceRefList = [traceTableAll, traceTable1, traceTable2, traceTable3]

traceInstaModel, traceData, traceFuncReq, traceLikelyChg, traceDataDefs, traceGenDefs,
  traceAssump, traceTheories :: [String]
traceDataRef, traceFuncReqRef, traceInstaModelRef, traceAssumpRef, traceTheoriesRef,
  traceDataDefRef, traceLikelyChgRef, traceGenDefRef :: [Sentence]

traceInstaModel = ["IM1", "IM2"]
traceInstaModelRef = map makeRef2S [eBalanceOnWtr, heatEInWtr]

traceFuncReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
traceFuncReqRef = map makeRef2S reqs

traceData = ["Data Constraints"]
traceDataRef = [makeRef2S dataConstTable1] --FIXME: Reference section?

traceAssump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
  "A11", "A12", "A13", "A14"]
traceAssumpRef = map makeRef2S assumptions

traceTheories = ["T1", "T2"]
traceTheoriesRef = map makeRef2S theoretical_models

traceGenDefs = ["GD1", "GD2"]
traceGenDefRef = map makeRef2S genDefs

traceDataDefs = ["DD1"]
traceDataDefRef = map makeRef2S [dd1HtFluxC]

traceLikelyChg = ["LC1", "LC2", "LC3", "LC4"]
traceLikelyChgRef = map makeRef2S $ [likeChgTCVOD, likeChgTCVOL] ++ likelyChgs ++ [likeChgTLH]

{-Traceability Matrix 1-}

traceRow1 :: [String]
traceRow1 = traceTheories ++ traceGenDefs ++ traceDataDefs ++ traceInstaModel

traceRowHeader1 :: [Sentence]
traceRowHeader1 = zipWith itemRefToSent traceRow1
  (traceTheoriesRef ++ traceGenDefRef ++ traceDataDefRef ++ traceInstaModelRef)

traceColumns1 :: [[String]]
traceColumns1 = [trace1T1, trace1GD1, trace1GD2, trace1DD1, trace1IM1,
  trace1IM2]

trace1T1, trace1GD1, trace1GD2, trace1DD1, trace1IM1, trace1IM2 :: [String]

--list of each item that "X" item requires for traceability matrix
trace1T1 = []
trace1GD1 = []
trace1GD2 = ["T1"]
trace1DD1 = ["GD1"]
trace1IM1 = ["GD2", "DD1"]
trace1IM2 = []

traceTable1 :: LabelledContent
traceTable1 = llcc (makeTabRef "TraceyRI") $
  Table (EmptyS : traceRowHeader1)
  (makeTMatrix (traceRowHeader1) (traceColumns1) (traceRow1))
  (showingCxnBw traceyMatrix
  (titleize' requirement `sAnd` titleize' inModel)) True

{-Traceability Matrix 2-}

traceRow2 :: [String]
traceRow2 = traceInstaModel ++ traceData ++ traceFuncReq

--column header
traceRowHeader2 :: [Sentence]
traceRowHeader2 = zipWith itemRefToSent traceRow2
  (traceInstaModelRef ++ traceDataRef ++ traceFuncReqRef)

--row header
traceColHeader2 :: [Sentence]
traceColHeader2 = zipWith itemRefToSent (traceInstaModel ++ traceFuncReq)
  (traceInstaModelRef ++ traceFuncReqRef)

traceColumns2 :: [[String]]
traceColumns2 = [trace2IM1, trace2IM2, trace2R1,
  trace2R2, trace2R3, trace2R4, trace2R5, trace2R6]

trace2IM1, trace2IM2, trace2R1, trace2R2,
  trace2R3, trace2R4, trace2R5, trace2R6 :: [String]

--list of each item that "X" item requires for traceability matrix
trace2IM1 = []
trace2IM2 = []
trace2R1 = []
trace2R2 = ["R1","IM1"]
trace2R3 = ["Data Constraints"]
trace2R4 = ["R1", "R2", "IM1"]
trace2R5 = ["IM1"]
trace2R6 = ["IM2"]

traceTable2 :: LabelledContent
traceTable2 = llcc (makeTabRef "TraceyRIs") $ Table
  (EmptyS : traceRowHeader2)
  (makeTMatrix (traceColHeader2) (traceColumns2) (traceRow2))
  (showingCxnBw traceyMatrix
  (titleize' requirement `sAnd` titleize' inModel)) True

{-Traceability Matrix 3-}

traceRowHeader3, traceColHeader3 :: [Sentence]
traceRowHeader3 = zipWith itemRefToSent traceAssump traceAssumpRef

traceColHeader3 = zipWith itemRefToSent
  (traceTheories ++ traceGenDefs ++ traceDataDefs ++ traceInstaModel ++ traceLikelyChg)
  (traceTheoriesRef ++ traceGenDefRef ++ traceDataDefRef ++ traceInstaModelRef ++
  traceLikelyChgRef)

traceColumns3 :: [[String]]
traceColumns3 = [trace3T1, trace3GD1, trace3GD2, trace3DD1,
  trace3IM1, trace3IM2, trace3LC1, trace3LC2, trace3LC3, trace3LC4]

trace3T1, trace3GD1, trace3GD2, trace3DD1,
  trace3IM1, trace3IM2, trace3LC1, trace3LC2, trace3LC3, trace3LC4 :: [String]

trace3T1  = ["A1"]
trace3GD1 = ["A2"]
trace3GD2 = ["A3", "A4", "A5"]
trace3DD1 = ["A6", "A7", "A8"]
trace3IM1 = ["A9", "A10"]
trace3IM2 = ["A10"]
trace3LC1 = ["A7"]
trace3LC2 = ["A8"]
trace3LC3 = ["A9"]
trace3LC4 = ["A11"]

traceTable3 :: LabelledContent
traceTable3 = llcc (makeTabRef "TraceyAI") $ Table
  (EmptyS : traceRowHeader3)
  (makeTMatrix traceColHeader3 traceColumns3 traceAssump)
  (showingCxnBw traceyMatrix (titleize' assumption `sAnd` S "Other" +:+
  titleize' item)) True

-- These matrices can probably be generated automatically when enough info is
-- abstracted out.

------------------------
-- Traceabilty Graphs --
------------------------

-- Using the SWHS graphs as place holders until ones can be generated for NoPCM 

------------------------------------------
--Section 8: SPECIFICATION PARAMETER VALUE
------------------------------------------

specParamValListb :: [QDefinition]
specParamValListb = [tank_length_min, tank_length_max,
  w_density_min, w_density_max, htCap_W_min, htCap_W_max, coil_HTC_min,
  coil_HTC_max, time_final_max]

specParamVal = valsOfAuxConstantsF progName specParamValListb

------------
--REFERENCES
------------

referencesRefList :: BibRef
referencesRefList = [incroperaEtAl2007, koothoor2013, lightstone2012, parnasClements1986, smithLai2005]
