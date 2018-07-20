module Drasil.SWHS.Body where

import Language.Drasil hiding (organization)
import Language.Drasil.Code (CodeSpec, codeSpec)
import Data.Drasil.SI_Units (metre, kilogram, second, centigrade, joule, watt)
import Control.Lens ((^.))

import Drasil.DocLang (AuxConstntSec (AuxConsProg), DocDesc, 
  DocSection (SSDSec, AuxConstntSec, Bibliography, IntroSec, RefSec, Verbatim), 
  LFunc (TermExcept), Literature (Doc', Lit), IntroSec (IntroProg), 
  IntroSub(IChar, IOrgSec, IPurpose, IScope), RefSec (RefProg), 
  RefTab (TAandA, TUnits), TSIntro (SymbConvention, SymbOrder, TSPurpose),
  Fields, Field(..), SSDSub(..), SolChSpec( SCSProg ), SSDSec(..), 
  Verbosity(..), InclUnits(..), DerivationDisplay(..), SCSSub(..),
  assumpF, dataConstraintUncertainty, genSysF, inDataConstTbl, inModelF, intro, 
  mkDoc, outDataConstTbl, physSystDesc, reqF, solChSpecF, specSysDesF, 
  termDefnF, traceGIntro, traceMGF, tsymb'')
import qualified Drasil.DocLang.SRS as SRS (inModel, missingP, likeChg,
  funcReq, propCorSol, genDefn, dataDefn, thModel, probDesc, goalStmt,
  sysCont, reference)

import Data.Drasil.People (thulasi, brooks, spencerSmith)
import Data.Drasil.Phrase (for)
import Data.Drasil.Concepts.Documentation (section_, traceyGraph, item,
  assumption, traceyMatrix, thModel, genDefn, dataDefn, inModel, likelyChg,
  dataConst, requirement, input_, solution, output_, corSol, constraint,
  value, software, column, model, goalStmt, quantity, property, condition, 
  physics, user, physical, datum, system, variable, sysCont, environment, 
  srs, softwareSys, organization, document, problem, content, information, 
  reference, definition, purpose, description, symbol_, physSyst, typUnc, 
  unlikelyChg)

import Data.Drasil.Concepts.PhysicalProperties (liquid, solid)
import qualified Data.Drasil.Concepts.Thermodynamics as CT (boiling,
  law_cons_energy, heat_trans, phase_change, melting, thermal_conduction,
  ht_flux, heat_cap_spec, thermal_energy, ht_trans_theo, thermal_analysis,
  ener_src)
import Data.Drasil.Concepts.Math (ode, de, unit_, rOfChng, equation)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Software.Products (sciCompS, compPro)

import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Quantities.Math (gradient, surface, uNormalVect, surArea)
import Data.Drasil.Quantities.Thermodynamics (temp, heat_cap_spec,
  latent_heat, sens_heat)
import Data.Drasil.Quantities.PhysicalProperties (density, mass, vol)

import Drasil.SWHS.Unitals (pcm_SA, temp_W, temp_PCM, pcm_HTC, pcm_E,
  temp_C, coil_SA, w_E, coil_HTC, sim_time, tau_S_P, htCap_S_P, pcm_mass,
  ht_flux_P, eta, tau_W, htCap_W, w_mass, ht_flux_C, vol_ht_gen, thickness,
  out_SA, ht_flux_out, ht_flux_in, in_SA, thFluxVect, time_final,
  specParamValList, w_density, temp_init, htCap_L_P, htFusion, pcm_density,
  temp_melt_P, pcm_vol, diam, tau_L_P, tank_length,
  w_vol, swhsConstrained, swhsOutputs, swhsInputs, swhsSymbols, swhsSymbolsAll)
import Drasil.SWHS.Concepts (progName, sWHT, water, rightSide, phsChgMtrl,
  coil, perfect_insul, tank, transient, gauss_div, swhs_pcm,
  phase_change_material, tank_pcm)
import Drasil.SWHS.TMods (tModels, t1ConsThermE, t1ConsThermE_new,
 t2SensHtE_new, t3LatHtE_new, swhsTMods)
import Drasil.SWHS.IMods (heatEInWtr_new, eBalanceOnWtr_new,
  heatEInPCM_new, eBalanceOnPCM_new, swhsIMods)
import Drasil.SWHS.DataDefs (swhsDataDefs,dd1HtFluxC, dd2HtFluxP, swhsDDefs, dataDefns)
import Drasil.SWHS.GenDefs (swhsGenDefs, generalDefinitions)
import Drasil.SWHS.References (ref_swhs_citations)
import Drasil.SWHS.Assumptions (assump3, assump4, assump5, assump6, assump13, 
  assump15, assump16, assump17, assump18, newAssumptions, swhsAssumptions)
import Drasil.SWHS.Requirements (req1, req2, reqEqn1, reqEqn2,
  req3, req4, req5, req6, req7, req8, req9, req10, req11, nonFuncReqs)
import Drasil.SWHS.Changes (likeChg1, likeChg2, likeChg3, likeChg4,
  likeChg5, likeChg6, unlikelyChgs)
import Drasil.SWHS.DataDesc (swhsInputMod)

import Data.Drasil.Utils (enumSimple, weave, itemRefToSent, makeListRef,
  makeTMatrix, refFromType, eqUnR)
import Data.Drasil.SentenceStructures (acroIM, acroGD, acroGS, showingCxnBw,
  foldlSent, foldlSent_, foldlSP, foldlSP_, foldlSPCol, foldlsC, isThe, ofThe,
  ofThe', sAnd, sOf, foldlList)

-------------------------------------------------------------------------------

acronyms :: [CI]
acronyms = [assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, ode,
  phsChgMtrl, physSyst, requirement, rightSide, srs, progName, thModel, typUnc, unlikelyChg]

this_si :: [UnitDefn]
this_si = map unitWrapper [metre, kilogram, second] ++ 
  map unitWrapper [centigrade, joule, watt]

--Will there be a table of contents?

swhsAuthors :: Sentence
swhsAuthors = S $ manyNames swhsPeople

swhs_si :: SystemInformation
swhs_si = SI {
  _sys = swhs_pcm,
  _kind = srs, 
  _authors = swhsPeople,
  _units = this_si,
  _quants = swhsSymbols,
  _concepts = symbT,
  _definitions = swhsDataDefs,
  _datadefs = ([] :: [DataDefinition]),
  _inputs = map qw swhsInputs,
  _outputs = map qw swhsOutputs,
  _defSequence = ([] :: [Block QDefinition]),
  _constraints = (swhsConstrained),
  _constants = [],
  _sysinfodb = swhsSymMap,
  _refdb = swhsRefDB
}
swhsRefDB :: ReferenceDB
swhsRefDB = rdb [] [] newAssumptions [] [] ref_swhs_citations []

swhsSymMap :: ChunkDB
swhsSymMap = cdb swhsSymbolsAll (map nw swhsSymbols ++ map nw acronyms) swhsSymbols
  this_si

  --Note: The second swhsSymbols here is
    -- Redundant b/c the unitals are not really concepts (yet). There
    -- Will still likely be a better way to do this.
  --FIXME: Should be all Named, not just acronyms at the end.

symbT :: [DefinedQuantityDict]
symbT =  ccss (getDoc swhs_srs') (egetDoc swhs_srs') swhsSymMap

swhsPeople :: [Person]
swhsPeople = [thulasi, brooks, spencerSmith]

mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro [
    TUnits, tsymb'' tsymb_intro (TermExcept [uNormalVect]), TAandA]):
  IntroSec (
    IntroProg (introP1 CT.ener_src energy swhs_pcm phsChgMtrl 
    progName CT.thermal_energy latent_heat unit_) (introP2 swhs_pcm program
    progName) 
    [IPurpose (purpDoc swhs_pcm progName),
     IScope (scopeReqs1 CT.thermal_analysis tank_pcm) 
       (scopeReqs2 temp CT.thermal_energy water phsChgMtrl sWHT),
     IChar (charReader1 CT.ht_trans_theo) (charReader2 de) (EmptyS),
     IOrgSec orgDocIntro inModel (SRS.inModel SRS.missingP [])
       (orgDocEnd swhs_pcm progName)]):
  Verbatim genSystDesc:
  SSDSec 
    (SSDProg [SSDSubVerb probDescription
      , SSDSolChSpec 
        (SCSProg 
          [ Assumptions
          , TMs ([Label] ++ stdFields) [t1ConsThermE_new, t2SensHtE_new, t3LatHtE_new]
          , GDs ([Label, Units] ++ stdFields) generalDefinitions ShowDerivation
          , DDs' ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
          , IMs ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields)
           [eBalanceOnWtr_new, eBalanceOnPCM_new, heatEInWtr_new, heatEInPCM_new ] ShowDerivation
          , Constraints  EmptyS dataConstraintUncertainty dataConTail
           [dataConTable1, dataConTable3]
          , CorrSolnPpties propsDeriv
          ]
        )
      ]
    ):  
  (map Verbatim [reqS, likelyChgs, unlikelyChgs, traceMAndG]) ++
    AuxConstntSec (AuxConsProg progName specParamValList) :
    Bibliography : []

swhsCode :: CodeSpec
swhsCode = codeSpec swhs_si [swhsInputMod]

tsymb_intro :: [TSIntro]
tsymb_intro = [TSPurpose, SymbConvention
  [Lit (nw CT.heat_trans), Doc' (nw progName)], SymbOrder]

--- The document starts here
swhs_srs' :: Document
swhs_srs' = mkDoc mkSRS for swhs_si

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]
-- It is sometimes hard to remember to add new sections both here and above.

-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --

------------------------------
-- Section 2 : INTRODUCTION --
------------------------------

-- In Concepts.hs "swhs_pcm" gives "s for program name, and there is a
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

genSystDesc :: Section
genSystDesc = genSysF [systCont] (userCharContents progName) [] []
-- First empty list is the list of constraints

--------------------------
-- 3.1 : System Context --
--------------------------

systCont :: Section
systCont = SRS.sysCont [systCContents progName, sys_context_fig, systCIntro 
  progName user, systContRespBullets] []

systContRespBullets :: Contents
systContRespBullets = Enumeration $ Bullet $ [userResp input_ datum,
  swhsResp]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

------------------------------
-- 3.3 : System Constraints --
------------------------------

---------------------------------------------
-- Section 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

specSystDesc :: Section
specSystDesc = specSysDesF (specSystDescIntroEnd swhs_pcm) [probDescription, solCharSpec]

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
termAndDefnBullets = Enumeration (Bullet $ map tAndDMap
  [CT.ht_flux, phase_change_material, CT.heat_cap_spec,
  CT.thermal_conduction, transient])

tAndDMap :: Concept c => c -> ItemType
tAndDMap c = Flat $ foldlSent [at_start c +: EmptyS, (c ^. defn)]

-- Structure of this list is same in all examples, probably can be automated.

-- Included heat flux and specific heat in NamedChunks even though they are
-- already in SWHSUnits

-----------------------------------------
-- 4.1.2 : Physical System Description --
-----------------------------------------

physSystDescription :: Section
physSystDescription = physSystDesc (short progName) fig_tank [physSystDescList, fig_tank]

-- Above paragraph is general except for progName and figure. However, not
-- every example has a physical system. Also, the SSP example is different, so
-- this paragraph can not be abstracted out as is.

physSystDescList :: Contents
physSystDescList = enumSimple 1 (short physSyst) $
  map foldlSent_ systDescList

systDescList :: [[Sentence]]
systDescList = [physSyst1 tank water, physSyst2 coil tank ht_flux_C,
  physSyst3 phsChgMtrl tank ht_flux_P]

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

goalStates :: Section
goalStates = SRS.goalStmt [goalStateIntro temp_C temp_W temp_PCM, goalStateList] []

goalStateList :: Contents
goalStateList = enumSimple 1 (short goalStmt) $
  map goalState outputConstraints

-- List structure is repeated between examples. (For all of these lists I am
-- imagining the potential for something like what was done with the lists in
-- MG, where you define goals, assumptions, physical system components, etc. in
-- separate files, import them and pass them as arguments to some "makeSRS"
-- function and the rest is automated.)

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

solCharSpec :: Section
solCharSpec = solChSpecF progName (probDescription, likelyChgs, unlikelyChgs) dataDefIntroEnd
  (dataContMid, dataConstraintUncertainty, dataContFooter quantity surArea
  vol thickness phsChgMtrl) (swhsAssumptions, 
  swhsTMods, genDefs ++ genDefsDeriv,
  swhsDDefs, iModsWithDerivs, dataConTables) [{-propsCorrSol-}]

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

assumps :: Section
assumps = assumpF
  (SRS.thModel SRS.missingP [])
  (SRS.genDefn SRS.missingP [])
  (SRS.dataDefn SRS.missingP [])
  iMods likelyChgs unlikelyChgs swhsAssumptions

-- Again, list structure is same between all examples.

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
-- General Definitions is automatically generated in solChSpecF
genDefs :: [Contents]
genDefs = map reldefn swhsGenDefs

genDefsDeriv :: [Contents]
genDefsDeriv = [genDefDeriv1 rOfChng temp,
  genDefDeriv2 t1ConsThermE vol,
  genDefDeriv3,
  genDefDeriv4 gauss_div surface vol thFluxVect uNormalVect unit_,
  genDefDeriv5,
  genDefDeriv6 vol vol_ht_gen,
  genDefDeriv7,
  genDefDeriv8 ht_flux_in ht_flux_out in_SA out_SA density heat_cap_spec
    temp vol assumption assump3 assump4 assump5 assump6,
  genDefDeriv9,
  genDefDeriv10 density mass vol,
  genDefDeriv11]


------------------------------
-- 4.2.4 : Data Definitions --
------------------------------
-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

iMods :: Section
iMods = inModelF probDescription
  (SRS.dataDefn SRS.missingP [])
  (SRS.thModel SRS.missingP [])
  (SRS.genDefn SRS.missingP [])
  iModsWithDerivs

iModsWithDerivs :: [Contents]
iModsWithDerivs = concat $ weave [iModsDerivations,
  map (\x -> [reldefn x]) swhsIMods]

iModsDerivations :: [[Contents]]
iModsDerivations = [iModSubpar solution temp_W temp_PCM pcm_E 
  CT.phase_change, iModDeriv1, iModDeriv2]
  
iModDeriv1 :: [Contents]
iModDeriv1 = (iMod1Para energy water) ++
  (weave [iMod1SentList, iMod1EqnList])

iMod1EqnList = map eqUnR [iMod1Eqn1, iMod1Eqn2,
  iMod1Eqn3, iMod1Eqn4, iMod1Eqn5, iMod1Eqn6, iMod1Eqn7]

iMod1SentList = map foldlSPCol
  [iMod1Sent1 rOfChng temp_W energy water vol w_vol w_mass htCap_W 
    ht_flux_C ht_flux_P coil_SA pcm_SA CT.heat_trans tank perfect_insul 
    vol_ht_gen assump15 assump16,
  iMod1Sent2 dd1HtFluxC dd2HtFluxP ht_flux_C ht_flux_P,
  iMod1Sent3 w_mass htCap_W,
  iMod1Sent4 rightSide coil_HTC coil_SA,
  iMod1Sent5, iMod1Sent6, iMod1Sent7]

iModDeriv2 :: [Contents]
iModDeriv2 =
  (iMod2StartPara energy phsChgMtrl sens_heat rOfChng temp_PCM vol pcm_vol
    pcm_mass htCap_S_P ht_flux_P pcm_SA ht_flux_out vol_ht_gen assump16 ++
  (weave [iMod2EqnList, iMod2SentList]) ++ (iMod2EndPara 
  phsChgMtrl htCap_S_P htCap_L_P tau_S_P tau_L_P surface CT.melting vol 
  temp_PCM temp_melt_P CT.boiling solid liquid))

iMod2SentList = map foldlSPCol [iMod2Sent1 dd2HtFluxP ht_flux_P,
  iMod2Sent2, iMod2Sent3]

iMod2EqnList = map eqUnR [iMod2Eqn1, iMod2Eqn2,
  iMod2Eqn3, iMod2Eqn4]

----------------------------
-- 4.2.6 Data Constraints --
----------------------------
------------------------------
-- Data Constraint: Table 1 --
------------------------------

dataConTables :: [Contents]
dataConTables = [dataConTable1, dataConTable3]

dataConTable1 :: Contents
dataConTable1 = inDataConstTbl inputConstraints

inputConstraints :: [UncertQ]
inputConstraints = [tank_length, diam, pcm_vol, pcm_SA, pcm_density,
  temp_melt_P, htCap_S_P, htCap_L_P, htFusion, coil_SA,
  temp_C, w_density, htCap_W, coil_HTC, pcm_HTC, temp_init, time_final]

------------------------------
-- Data Constraint: Table 2 --
------------------------------

------------------------------
-- Data Constraint: Table 3 --
------------------------------

dataConTable3 :: Contents
dataConTable3 = outDataConstTbl outputConstraints
--FIXME: add "(by A11)" in Physical Constraints of `temp_W` and `temp_PCM`?

outputConstraints :: [UncertQ]
outputConstraints = [temp_W, temp_PCM, w_E, pcm_E]

-- Other Notes:
---- Will there be a way to have asterisks for certain pieces of the table?

----------------------------------------------
-- 4.2.7 : Properties of A Correct Solution --
----------------------------------------------

propsDeriv :: [Contents]
propsDeriv =
  [propCorSolDeriv1 CT.law_cons_energy w_E energy coil phsChgMtrl dd1HtFluxC
    dd2HtFluxP surface CT.heat_trans,
  propCorSolDeriv2,
  propCorSolDeriv3 pcm_E energy phsChgMtrl water,
  propCorSolDeriv4,
  propCorSolDeriv5 equation progName rightSide]

-- Above section only occurs in this example (although maybe it SHOULD be in
-- the others).

-- Remember to insert references in above derivation when available

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------

reqS :: Section
reqS = reqF [funcReqs, nonFuncReqs]

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

funcReqs :: Section
funcReqs = SRS.funcReq funcReqsList []

funcReqsList :: [Contents]
funcReqsList = ([req1]) ++ [funcReqsTable] ++ ([req2]) ++
  [reqEqn1, reqEqn2] ++ (reqs) 

funcReqsTable :: Contents
funcReqsTable = (Table [titleize symbol_, titleize unit_, titleize description]
  (mkTable
  [ch,
  --(\ch -> Sy (unit_symb ch)),
  unitToSentence,
  phrase] (map qw inputConstraints))
  (titleize input_ +:+ titleize variable +:+ titleize requirement) False)
  "InConstraints"

reqs :: [Contents]
reqs = [req3, req4, req5, req6, req7, req8, req9, req10, req11]

---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------
--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

likelyChgs :: Section
likelyChgs = SRS.likeChg likelyChgsList []

likelyChgsList :: [Contents]
likelyChgsList = likeChgList 

likeChgList :: [Contents]
likeChgList = [likeChg1, likeChg2, likeChg3, likeChg4, likeChg5, likeChg6]

--------------------------------
-- Section 6b : UNLIKELY CHANGES --
--------------------------------
{-
unlikelyChgs :: Section
unlikelyChgs = SRS.unlikeChg unlikelyChgsList []

unlikelyChgsList :: [Contents]
unlikelyChgsList = unlikeChgList 

unlikeChgList :: [Contents]
unlikeChgList = []
-}
--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

traceMAndG :: Section
traceMAndG = traceMGF traceRefList traceTrailing
  ([traceTable1, traceTable2, traceTable3] ++
  (traceIntro2) ++ [traceFig1, traceFig2]) []

traceRefList :: [Contents]
traceRefList = [traceTable1, traceTable2, traceTable3]

traceTrailing :: [Sentence]
traceTrailing = [traceTrailing1, traceTrailing2, traceTrailing3]

traceInstaModel, traceData, traceFuncReq, traceLikelyChg, traceDataDefs, traceGenDefs,
  traceAssump, traceTheories :: [String]
  
traceDataRef, traceFuncReqRef, traceInstaModelRef, traceAssumpRef, traceTheoriesRef,
  traceDataDefRef, traceLikelyChgRef, traceGenDefRef :: [Sentence]

traceInstaModel = ["IM1", "IM2", "IM3", "IM4"]
traceInstaModelRef = map (refFromType Theory) swhsIMods

traceFuncReq = ["R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
  "R11"]
traceFuncReqRef = makeListRef traceFuncReq funcReqs

traceData = ["Data Constraints"]
traceDataRef = [makeRef dataConTable1] --FIXME: Reference section?

traceAssump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
  "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19"]
traceAssumpRef = makeListRef traceAssump assumps

traceTheories = ["T1", "T2", "T3"]
traceTheoriesRef = map (refFromType Theory) tModels

traceGenDefs = ["GD1", "GD2"]
traceGenDefRef = map (refFromType Theory) swhsGenDefs

traceDataDefs = ["DD1", "DD2", "DD3", "DD4"]
traceDataDefRef = map (refFromType Data) swhsDataDefs

traceLikelyChg = ["LC1", "LC2", "LC3", "LC4", "LC5", "LC6"]
traceLikelyChgRef = makeListRef traceLikelyChg likelyChgs

{-Traceability Matrix 1-}

traceMRow1 :: [String]
traceMRow1 = traceTheories ++ traceGenDefs ++ traceDataDefs ++ traceInstaModel

traceMRowHeader1 :: [Sentence]
traceMRowHeader1 = zipWith itemRefToSent traceMRow1 
  (traceTheoriesRef ++ traceGenDefRef ++ traceDataDefRef ++ traceInstaModelRef)

traceMColumns1 :: [[String]]
traceMColumns1 = [trace1T1, trace1T2, trace1T3, trace1GD1, trace1GD2, trace1DD1,
  trace1DD2, trace1DD3, trace1DD4, trace1IM1, trace1IM2, trace1IM3, trace1IM4]

trace1T1, trace1T2, trace1T3, trace1GD1, trace1GD2, trace1DD1, trace1DD2,
  trace1DD3, trace1DD4, trace1IM1, trace1IM2, trace1IM3, trace1IM4 :: [String]

--list of each item that "X" item requires for traceability matrix
trace1T1 = []
trace1T2 = ["T3"]
trace1T3 = []
trace1GD1 = []
trace1GD2 = ["T1"]
trace1DD1 = ["GD1"]
trace1DD2 = ["GD1"]
trace1DD3 = []
trace1DD4 = ["DD3"]
trace1IM1 = ["GD2", "DD1", "DD2", "IM2"]
trace1IM2 = ["GD2", "DD2", "DD4", "IM1", "IM4"]
trace1IM3 = ["T2"]
trace1IM4 = ["T2", "T3", "DD2", "DD3", "DD4", "IM2"]

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

traceTable2 :: Contents
traceTable2 = Table (EmptyS:traceMRowHeader2)
  (makeTMatrix (traceMColHeader2) (traceMColumns2) (traceMRow2))
  (showingCxnBw traceyMatrix
  (titleize' requirement `sAnd` titleize' inModel)) True "Tracey1"

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
  trace3DD2, trace3DD3, trace3DD4, trace3IM1, trace3IM2, trace3IM3, trace3IM4,
  trace3LC1, trace3LC2, trace3LC3, trace3LC4, trace3LC5, trace3LC6]

trace3T1, trace3T2, trace3T3, trace3GD1, trace3GD2, trace3DD1, trace3DD2, 
  trace3DD3, trace3DD4, trace3IM1, trace3IM2, trace3IM3, trace3IM4, trace3LC1,
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
  S "Due to", foldlList (map S ["increasing cost", "diminishing availability",
    "negative environmental impact of fossil fuels"]) `sC`
  S "there is a higher demand for renewable", plural es `sAnd` phrase en +:+.
  S "storage technology", sp ^. defn, sParen (short pcmat), S "use renewable",
  plural es `sAnd` S "provide a novel way of storing" +:+. phrase en,
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

-- In Concepts.hs "swhs_pcm" gives "s for program name, and there is a
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

charReader1 :: ConceptChunk -> Sentence
charReader1 htt = foldlSent_ [EmptyS +:+. phrase htt,
  S "A third or fourth year Mechanical Engineering course on this topic",
  S "is recommended"]

charReader2 :: CI -> Sentence
charReader2 diffeq = foldlSent_ [(plural diffeq) `sC`
  S "as typically covered in first and second year Calculus courses"]

------------------------------------
-- 2.4 : Organization of Document --
------------------------------------

orgDocIntro :: Sentence
orgDocIntro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the template for an", short srs,
  S "for", phrase sciCompS, S "proposed by", (sSqBrNum 3) `sAnd`
  (sSqBrNum 6), sParen (makeRef (SRS.reference SRS.missingP []))]

orgDocEnd :: NamedIdea ni => ni -> CI -> Sentence
orgDocEnd sp pro = foldlSent_ [S "The", plural inModel,
  sParen (makeRef (SRS.inModel SRS.missingP [])),
  S "to be solved are referred to as", acroIM 1,
  S "to" +:+. acroIM 4, S "The", plural inModel,
  S "provide the", phrase ode, sParen (short ode :+: S "s")
  `sAnd` S "algebraic", plural equation, S "that",
  phrase model, S "the" +:+. phrase sp,
  short pro, S "solves these", short ode :+: S "s"]

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

systCContents :: CI -> Contents
systCContents pro = foldlSP [makeRef sys_context_fig, S "shows the" +:+.
  phrase sysCont, S "A circle represents an external entity outside the",
  phrase software `sC` S "the", phrase user, S "in this case. A",
  S "rectangle represents the", phrase softwareSys, S "itself" +:+.
  sParen (short pro), S "Arrows are used to show the",
  plural datum, S "flow between the", phrase system `sAnd`
  S "its", phrase environment]

sys_context_fig :: Contents
sys_context_fig = fig (foldlSent_
  [makeRef sys_context_fig +: EmptyS, titleize sysCont])
  "SystemContextFigure.png" "SysCon"

systCIntro :: CI -> NamedChunk -> Contents
systCIntro pro us = foldlSPCol [short pro +:+. S "is mostly self-contained",
  S "The only external interaction is through the", phrase us +:+.
  S "interface", S "responsibilities" `ofThe'` phrase us `sAnd`
  S "the", phrase system, S "are as follows"]

-- User Responsibilities --
userResp :: NamedChunk -> NamedChunk -> ItemType
userResp inp dat = Nested (titleize user +: S "Responsibilities")
  $ Bullet $ map Flat [

  foldlSent_ [S "Provide the", phrase inp, plural dat, S "to the",
  phrase system `sC` S "ensuring no errors in the", plural dat, S "entry"],

  foldlSent_ [S "Take care that consistent", plural unit_,
  S "are used for", phrase inp, plural variable]

  ]

-- SWHS Responsibilities --
swhsResp :: ItemType
swhsResp = Nested (short progName +: S "Responsibilities")
  $ Bullet $ map Flat [

  foldlSent_ [S "Detect", plural datum, S "type mismatch, such as a string of",
  S "characters instead of a floating point number"],

  foldlSent_ [S "Determine if the", plural input_, S "satisfy the required",
  phrase physical `sAnd` phrase software, plural constraint],

  foldlSent_ [S "Calculate the required", plural output_]

  ]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

userCharContents :: CI -> Contents
userCharContents pro = foldlSP [S "The end", phrase user, S "of",
  short pro, S "should have an understanding of undergraduate",
  S "Level 1 Calculus and", titleize physics]

-- Some of these course names are repeated between examples, could potentially
-- be abstracted out.

------------------------------
-- 3.3 : System Constraints --
------------------------------

---------------------------------------------
-- Section 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

specSystDescIntroEnd :: NamedIdea ni => ni -> Sentence
specSystDescIntroEnd sw = foldlSent_ [foldlsC (map plural (take 3 renameList1))
  `sC` S "and finally the", plural inModel, sParen (short ode :+: S "s"),
  S "that", phrase model, S "the", phrase sw]

-- Completely general except for solar water heating tank (object of analysis)
-- and similar between all examples; can be abstracted out.

-- The swhs_pcm reference at the end would be better if singular, but concept
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

fig_tank :: Contents
fig_tank = fig (
  foldlSent_ [at_start sWHT `sC` S "with", phrase ht_flux_C, S "of",
  ch ht_flux_C `sAnd` phrase ht_flux_P, S "of", ch ht_flux_P])
  "Tank.png" "Tank"

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

goalStateIntro :: UncertQ -> UncertQ -> UncertQ -> Contents
goalStateIntro temc temw tempcm = foldlSPCol [S "Given the", phrase temc `sC`
  S "initial", plural condition, S "for the", phrase temw
  `sAnd` S "the", phrase tempcm `sC` S "and material",
  plural property `sC` S "the", plural goalStmt, S "are"]

-- 2 examples include this paragraph, 2 don't. The "givens" would need to be
-- abstracted out if this paragraph were to be abstracted out.

goalState :: NamedIdea varTerm => varTerm -> Sentence
goalState varTerm = foldlSent [S "Predict the", phrase varTerm,
  S "over", phrase time]

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

--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

-- Theory has to be RelationChunk....
-- No way to include "Source" or "Ref. By" sections?

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

genDefDeriv3, genDefDeriv5, genDefDeriv7, genDefDeriv9,
  genDefDeriv11 :: Contents

genDefDeriv1 :: ConceptChunk -> UnitalChunk -> Contents
genDefDeriv1 roc tem = foldlSPCol [S "Detailed derivation of simplified",
  phrase roc, S "of", phrase tem]

genDefDeriv2 :: RelationConcept -> UnitalChunk -> Contents
genDefDeriv2 t1ct vo = foldlSPCol [S "Integrating", makeRef $ reldefn t1ct,
  S "over a", phrase vo, sParen (ch vo) `sC` S "we have"]

genDefDeriv3 = eqUnR
  ((negate (int_all (eqSymb vol) ((sy gradient) $. (sy thFluxVect)))) +
  (int_all (eqSymb vol) (sy vol_ht_gen)) $=
  (int_all (eqSymb vol) ((sy density) * (sy heat_cap_spec) * pderiv (sy temp) time)))

genDefDeriv4 :: ConceptChunk -> DefinedQuantityDict -> UnitalChunk -> UnitalChunk ->
  DefinedQuantityDict -> ConceptChunk -> Contents
genDefDeriv4 gaussdiv su vo tfv unv un = foldlSPCol [S "Applying", titleize gaussdiv,
  S "to the first term over", (phrase su +:+ ch su `ofThe` phrase vo) `sC`
  S "with", ch tfv, S "as the", phrase tfv, S "for the",
  phrase surface `sAnd` ch unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

genDefDeriv5 = eqUnR
  ((negate (int_all (eqSymb surface) ((sy thFluxVect) $. (sy uNormalVect)))) +
  (int_all (eqSymb vol) (sy vol_ht_gen)) $= 
  (int_all (eqSymb vol) ((sy density) * (sy heat_cap_spec) * pderiv (sy temp) time)))

genDefDeriv6 :: UnitalChunk -> UnitalChunk -> Contents
genDefDeriv6 vo vhg = foldlSPCol [S "We consider an arbitrary" +:+.
  phrase vo, S "The", phrase vhg, S "is assumed constant. Then",
  sParen $ S $ show (1 :: Integer), S "can be written as"]

genDefDeriv7 = eqUnR
  ((sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out) *
  (sy out_SA) + (sy vol_ht_gen) * (sy vol) $= 
  (int_all (eqSymb vol) ((sy density) * (sy heat_cap_spec) * pderiv (sy temp) time)))

genDefDeriv8 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk -> CI -> Contents ->
  Contents -> Contents -> Contents -> Contents
genDefDeriv8 hfi hfo isa osa den hcs tem vo assu a3 a4 a5 a6 = foldlSPCol 
  [S "Where", foldlList (map ch [hfi, hfo, isa, osa]), 
  S "are explained in" +:+. acroGD 2, S "Assuming", ch den `sC` ch hcs
  `sAnd` ch tem, S "are constant over the", phrase vo `sC` S "which is true",
  S "in our case by", titleize' assu, 
  foldlList (map (\c -> sParen (makeRef c)) [a3, a4, a5, a6]) `sC` S "we have"]

genDefDeriv9 = eqUnR
  ((sy density) * (sy heat_cap_spec) * (sy vol) * deriv (sy temp)
  time $= (sy ht_flux_in) * (sy in_SA) - (sy ht_flux_out) *
  (sy out_SA) + (sy vol_ht_gen) * (sy vol))

genDefDeriv10 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> Contents
genDefDeriv10 den ma vo = foldlSPCol [S "Using the fact that", ch den :+:
  S "=" :+: ch ma :+: S "/" :+: ch vo `sC` S "(2) can be written as"]

genDefDeriv11 = eqUnR
  ((sy mass) * (sy heat_cap_spec) * deriv (sy temp)
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
iModSubpar sol temw tempcm epcm pc = [foldlSP [S "The goals", acroGS 1,
  S "to", acroGS 4, S "are solved by", acroIM 1, S "to" +:+. acroIM 4,
  S "The", plural sol, S "for", acroIM 1 `sAnd` acroIM 2, 
  S "are coupled since the", phrase sol, S "for", ch temw `sAnd` ch tempcm
  +:+. S "depend on one another", acroIM 3, S "can be solved once", acroIM 1, 
  S "has been solved. The", phrase sol `sOf` acroIM 2 `sAnd` acroIM 4, 
  S "are also coupled, since the", phrase tempcm `sAnd` phrase epcm, 
  S "depend on the", phrase pc]]

iMod1Para :: UnitalChunk -> ConceptChunk -> [Contents]
iMod1Para en wa = [foldlSPCol [S "Derivation of the",
  phrase en, S "balance on", phrase wa]]

iMod1Sent1 :: ConceptChunk -> UncertQ -> UnitalChunk -> ConceptChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UncertQ -> UnitalChunk -> 
  UnitalChunk -> UncertQ -> UncertQ -> ConceptChunk -> ConceptChunk -> 
  ConceptChunk -> UnitalChunk -> Contents -> Contents -> [Sentence]
iMod1Sent1 roc temw en wa vo wvo wma hcw hfc hfp csa psa ht ta purin vhg
  a15 a16 = [S "To find the", phrase roc, S "of", ch temw `sC`
  S "we look at the", phrase en, S "balance on" +:+.
  phrase wa, S "The", phrase vo, S "being considered" `isThe`
  phrase wvo, ch wvo `sC` S "which has", phrase wma +:+.
  (ch wma `sAnd` phrase hcw `sC` ch hcw),
  ch hfc, S "represents the", phrase hfc `sAnd`
  ch hfp, S "represents the", phrase hfp `sC`
  S "over", phrase csa `sAnd` phrase psa, S "of",
  ch csa `sAnd` ch psa `sC` S "respectively. No",
  phrase ht, S "occurs to", (S "outside" `ofThe`
  phrase ta) `sC` S "since it has been assumed to be",
  phrase purin +:+. sParen (makeRef a15), S "Assuming no",
  phrase vhg +:+. (sParen (makeRef a16) `sC`
  (E $ sy vhg $= 0)), S "Therefore, the", phrase equation, S "for",
  acroGD 2, S "can be written as"]

iMod1Sent2 :: QDefinition -> QDefinition -> UnitalChunk ->
  UnitalChunk -> [Sentence]
iMod1Sent2 d1hf d2hf hfc hfp = [S "Using", (makeRef $ datadefn d1hf) `sAnd`
  (makeRef $ datadefn d2hf), S "for", ch hfc `sAnd`
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
  S "we are left with the governing", short ode, S "for", acroIM 1]

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

iMod2Sent1 :: QDefinition -> UnitalChunk -> [Sentence]
iMod2Sent1 d2hfp hfp = [S "Using", makeRef $ datadefn d2hfp, S "for", 
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

iMod1EqnList, iMod1SentList, iMod2EqnList, 
  iMod2SentList :: [Contents]

iMod2StartPara :: UnitalChunk -> CI -> UnitalChunk -> ConceptChunk ->
  UncertQ -> UnitalChunk -> UncertQ -> UnitalChunk -> UncertQ -> 
  UnitalChunk -> UncertQ -> UnitalChunk -> UnitalChunk ->
  Contents -> [Contents]
iMod2StartPara en pcmat sh roc ptem vo pvo pma hcsp hfp psa hfo vhg a16 =
  map foldlSPCol [

  [S "Detailed derivation of the", phrase en, S "balance on the",
  short pcmat, S "during", phrase sh :+: S "ing phase"],

  [S "To find the", phrase roc, S "of", ch ptem `sC`
  S "we look at the", phrase en, S "balance on the" +:+.
  short pcmat, S "The", phrase vo,
  S "being considered is the" +:+. (phrase pvo `sC` ch pvo),
  S "The derivation that follows is initially for the",
  phrase solid +:+. short pcmat, S "The", phrase pma, S "is",
  ch pma `sAnd` S "the", phrase hcsp, S "is" +:+. ch hcsp,
  S "The", phrase hfp, S "is", ch hfp, S "over",
  phrase psa +:+. ch psa, S "There is no" +:+. phrase hfo,
  S "Assuming no", phrase vhg, sParen (makeRef a16) `sC`
  ch vhg :+: S "=0, the", phrase equation, S "for", acroGD 2,
  S "can be written as"]

  ]

iMod2EndPara :: CI -> UncertQ -> UncertQ -> UnitalChunk -> UnitalChunk -> 
  DefinedQuantityDict -> ConceptChunk -> UnitalChunk -> UncertQ -> UncertQ -> 
  ConceptChunk -> ConceptChunk -> ConceptChunk -> [Contents]
iMod2EndPara pcmat hcsp hclp tsp tlp sur mel vo ptem tmp boi so li = map 
  foldlSP [

  [titleize equation,
  S "(6) applies for the", phrase solid +:+. short pcmat,
  S "In the case where all of the", short pcmat,
  S "is melted, the same derivation applies, except that",
  ch hcsp, S "is replaced by", ch hclp `sC`
  S "and thus", ch tsp, S "is replaced by" +:+.
  ch tlp, S "Although a small change in", phrase sur,
  S "area would be expected with", phrase mel `sC`
  S "this is not included, since",
  (phrase vo +:+ S "change" `ofThe` short pcmat),
  S "with", phrase mel,
  S "is assumed to be negligible", sParen (makeRef assump17)],

  [S "In the case where", ch ptem :+: S "=" :+:
  ch tmp `sAnd` S "not all of the", short pcmat,
  S "is melted, the", phrase ptem +:+. S "does not change",
  S "Therefore, in this case d" :+: ch ptem :+: S "/d" :+:
  ch time :+: S "=0"],

  [S "This derivation does not consider",
  (phrase boi `ofThe` short pcmat) `sC` S "as the", short pcmat,
  S "is assumed to either be in a", (so ^. defn),
  S "or a", (li ^. defn), sParen (makeRef assump18)]

  ]

-- Add GD, A, and EqnBlock references when available
-- Replace derivs with regular derivative when available
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
  S "or there would be freezing", sParen (makeRef assump13)],

  [sParen (Sp Hash), S "The", plural constraint, S "on the", phrase sa,
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

propCorSolDeriv1 :: ConceptChunk -> UncertQ -> UnitalChunk -> ConceptChunk ->
  CI -> QDefinition -> QDefinition -> DefinedQuantityDict -> ConceptChunk -> Contents
propCorSolDeriv1 lce ewat en co pcmat d1hfc d2hfp su ht  =
  foldlSPCol [S "A", phrase corSol, S "must exhibit the" +:+.
  phrase lce, S "This means that the", phrase ewat,
  S "should equal the difference between the total", phrase en,
  phrase input_, S "from the", phrase co `sAnd` S "the",
  phrase en, phrase output_, S "to the" +:+. short pcmat,
  S "This can be shown as an", phrase equation, S "by taking",
  (makeRef $ datadefn d1hfc) `sAnd` (makeRef $ datadefn d2hfp) `sC`
  S "multiplying each by their respective", phrase su,
  S "area of", phrase ht `sC` S "and integrating each",
  S "over the", phrase sim_time `sC` S "as follows"]

propCorSolDeriv2 :: Contents
propCorSolDeriv2 = eqUnR
  ((sy w_E) $= (defint (eqSymb time) 0 (sy time)
  ((sy coil_HTC) * (sy coil_SA) * ((sy temp_C) - apply1 temp_W time)))
  - (defint (eqSymb time) 0 (sy time)
  ((sy pcm_HTC) * (sy pcm_SA) * ((apply1 temp_W time) -
  (apply1 temp_PCM time)))))

propCorSolDeriv3 :: UncertQ -> UnitalChunk -> CI -> ConceptChunk -> Contents
propCorSolDeriv3 epcm en pcmat wa =
  foldlSP_ [S "In addition, the", phrase epcm, S "should equal the",
  phrase en, phrase input_, S "to the", short pcmat,
  S "from the" +:+. phrase wa, S "This can be expressed as"]

propCorSolDeriv4 :: Contents
propCorSolDeriv4 = eqUnR
  ((sy pcm_E) $= (defint (eqSymb time) 0 (sy time)
  ((sy pcm_HTC) * (sy pcm_SA) * ((apply1 temp_W time) - 
  (apply1 temp_PCM time)))))

propCorSolDeriv5 :: ConceptChunk -> CI -> CI -> Contents
propCorSolDeriv5 eq pro rs = foldlSP [titleize' eq, S "(FIXME: Equation 7)" 
  `sAnd` S "(FIXME: Equation 8) can be used as", Quote (S "sanity") :+:
  S "checks to gain confidence in any", phrase solution,
  S "computed by" +:+. short pro, S "The relative",
  S "error between the results computed by", short pro `sAnd`
  S "the results calculated from the", short rs, S "of these",
  plural eq, S "should be less than 0.001%", makeRef req9]

-- Above section only occurs in this example (although maybe it SHOULD be in
-- the others).

-- Remember to insert references in above derivation when available

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
renameList1  = [thModel, genDefn, dataDefn, inModel, likelyChg, assumption]
renameList2  = [inModel, requirement, dataConst]

traceTrailing1, traceTrailing2, traceTrailing3 :: Sentence

traceTrailing1 = foldlSent [foldlList $ map plural (take 4 renameList1), 
  S "with each other"]

traceTrailing2 = foldlSent [foldlList $ map plural renameList2, 
  S "on each other"]

traceTrailing3 = foldlSent_ [foldlList $ map plural (take 5 renameList1),
  S "on the", plural assumption]

traceTable1 :: Contents
traceTable1 = Table (EmptyS:traceMRowHeader1)
  (makeTMatrix (traceMRowHeader1) (traceMColumns1) (traceMRow1))
  (showingCxnBw traceyMatrix
  (titleize' item +:+ S "of Different" +:+ titleize' section_)) True "Tracey2"

traceTable3 :: Contents
traceTable3 = Table (EmptyS:traceMRowHeader3)
  (makeTMatrix traceMColHeader3 traceMColumns3 traceMRow3)
  (showingCxnBw traceyMatrix (titleize' assumption `sAnd` S "Other" +:+
  titleize' item)) True "Tracey3"

-- These matrices can probably be generated automatically when enough info is
-- abstracted out.

------------------------
-- Traceabilty Graphs --
------------------------

traceIntro2 :: [Contents]
traceIntro2 = traceGIntro [traceFig1, traceFig2]

  [foldlSent [foldlList $ map plural renameList1, S "on each other"],

  foldlSent_ [foldlList $ map plural renameList2, S "on each other"]]

traceFig1 :: Contents
traceFig1 = fig (showingCxnBw traceyGraph (titleize' item +:+
  S "of Different" +:+ titleize' section_)) "ATrace.png" "TraceyA"

traceFig2 :: Contents
traceFig2 = fig (showingCxnBw traceyGraph (foldlList $ map titleize' 
  renameList2)) "RTrace.png" "TraceyR"

-------------------------------------------------
-- Section 8 :  Specification Parameter Values --
-------------------------------------------------
----------------------------
-- Section 9 : References --
----------------------------
