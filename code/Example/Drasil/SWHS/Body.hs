module Drasil.SWHS.Body where

import Language.Drasil
import Data.Drasil.SI_Units
import Control.Lens ((^.))

import Data.Drasil.People (thulasi, brooks, spencerSmith)

import Data.Drasil.Concepts.Documentation (section_, traceyGraph, item,
  assumption, traceyMatrix, thModel, genDefn, dataDefn, inModel, likelyChg,
  dataConst, requirement, input_, solution, output_, corSol, constraint,
  value, software, column, model, goalStmt, quantity, property, condition, 
  physics, user, physical, datum, system, variable, sysCont, environment, 
  srs, softwareSys, organization, document, problem, content, information, 
  reference, definition, purpose, description, acroNumGen, symbol_, physSyst,
  typUnc)

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
  ht_flux_P, eta, tau_W, htCap_W, w_mass, ht_flux_C, vol_ht_gen,
  out_SA, ht_flux_out, ht_flux_in, in_SA, thFluxVect, time_final,
  specParamValList, w_density, temp_init, htCap_L_P, htFusion, pcm_density,
  temp_melt_P, pcm_vol, diam, tau_L_P, tank_length, htTransCoeff_min,
  w_vol, swhsConstrained, swhsOutputs, swhsInputs, swhsSymbols, swhsSymbolsAll)
import Drasil.SWHS.Concepts (progName, sWHT, water, rightSide, phsChgMtrl,
  coil, perfect_insul, tank, transient, gauss_div, swhs_pcm,
  phase_change_material, tank_pcm)
import Drasil.SWHS.TMods (tModels, t1ConsThermE, s4_2_2_swhsTMods)
import Drasil.SWHS.IMods (s4_2_5_IMods)
import Drasil.SWHS.DataDefs (swhsSymbMapDRef, swhsSymbMapTRef, swhsDataDefs,
  dd1HtFluxC, dd2HtFluxP, swhsSymbMapT, s4_2_4_swhsDataDefs)
import Drasil.SWHS.GenDefs (swhsGenDefs)
import Drasil.SWHS.References (s9_swhs_citations)
import Drasil.SWHS.Assumptions (s4_2_1_list, assump3, assump4, assump5,
  assump6, assump13, assump15, assump16, assump17, assump18)
import Drasil.SWHS.Requirements (req1, req2, s5_1_2_Eqn1, s5_1_2_Eqn2,
  req3, req4, req5, req6, req7, req8, req9, req10, req11, s5_2)
import Drasil.SWHS.LikelyChanges (likeChg1, likeChg2, likeChg3, likeChg4,
  likeChg5, likeChg6)
import Drasil.SWHS.DataDesc (swhsInputMod)

import qualified Drasil.SRS as SRS (inModel, missingP, likeChg,
  funcReq, propCorSol, genDefn, dataDefn, thModel, probDesc, goalStmt,
  sysCont, reference)

import Drasil.DocumentLanguage (DocDesc, mkDoc, tsymb'',
  LFunc (TermExcept),
  Literature (Lit, Doc'),
  TSIntro (SymbOrder, SymbConvention, TSPurpose),
  DocSection (Verbatim, IntroSec, RefSec, Bibliography, AuxConstntSec), 
  IntroSub(IOrgSec, IChar, IScope, IPurpose),
  IntroSec (IntroProg),
  RefTab (TAandA, TUnits),
  RefSec (RefProg),
  AuxConstntSec (AuxConsProg))

import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.SpecificSystemDescription (inModelF, assumpF,
  inDataConstTbl, outDataConstTbl, dataConstraintUncertainty, solChSpecF,
  termDefnF, specSysDesF, physSystDesc)
import Drasil.Sections.TraceabilityMandGs (traceMGF, traceGIntro)
import Drasil.Sections.Requirements (reqF)
import Drasil.Sections.GeneralSystDesc (genSysF)

import Data.Drasil.Utils (enumSimple, weave, getES, itemRefToSent, makeListRef,
  makeTMatrix, refFromType)
import Data.Drasil.SentenceStructures (acroIM, acroGD, acroGS, showingCxnBw,
  foldlSent, foldlSent_, foldlSP, foldlSP_, foldlSPCol, foldlsC, isThe, ofThe,
  ofThe', sAnd, sOf, foldlList)

-------------------------------------------------------------------------------

acronyms :: [CI]
acronyms = [assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, ode,
  phsChgMtrl, physSyst, requirement, rightSide, srs, progName, thModel, typUnc]

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ 
  map UU [centigrade, joule, watt]

--Will there be a table of contents?

swhsAuthors :: Sentence
swhsAuthors = manyNames swhsPeople

swhs_si :: SystemInformation
swhs_si = SI {
  _sys = swhs_pcm,
  _kind = srs, 
  _authors = swhsPeople,
  _units = this_si,
  _quants = swhsSymbols,
  _concepts = (swhsSymbols),
  _definitions = swhsDataDefs,
  _inputs = map qw swhsInputs,
  _outputs = map qw swhsOutputs,
  _defSequence = ([] :: [Block QDefinition]),
  _constraints = (swhsConstrained),
  _constants = [],
  _sysinfodb = swhsSymMap
}

swhsSymMap :: ChunkDB
swhsSymMap = cdb swhsSymbolsAll (map nw swhsSymbols ++ map nw acronyms) ([] :: [CWrapper] ) -- FIXME: Fill in Concepts
  this_si

  --Note: The second swhsSymbols here is
    -- Redundant b/c the unitals are not really concepts (yet). There
    -- Will still likely be a better way to do this.
  --FIXME: Should be all Named, not just acronyms at the end.

swhsPeople :: [Person]
swhsPeople = [thulasi, brooks, spencerSmith]

mkSRS :: DocDesc
mkSRS = [RefSec (RefProg intro
  [TUnits, tsymb'' tsymb_intro (TermExcept [uNormalVect]), TAandA])] ++

  [IntroSec (IntroProg (s2_intro CT.ener_src energy swhs_pcm phsChgMtrl 
    progName CT.thermal_energy latent_heat unit_) (s2_kSent swhs_pcm program
    progName) [
   
  IPurpose (s2_1_par1 swhs_pcm progName),
  
  IScope (s2_2_contents CT.thermal_analysis tank_pcm)
  (s2_2_end temp CT.thermal_energy water phsChgMtrl sWHT),
   
  IChar (s2_3_knowlegde CT.ht_trans_theo) (s2_3_understanding de) (EmptyS),
  
  IOrgSec (s2_4_intro) (inModel) (SRS.inModel SRS.missingP [])
  (s2_4_trail swhs_pcm progName)])] ++
  
  map Verbatim [s3, s4, s5, s6, s7] ++ 
  [AuxConstntSec (AuxConsProg progName specParamValList)] ++
  [Bibliography s9_swhs_citations]

swhsCode :: CodeSpec
swhsCode = codeSpec' swhs_si [swhsInputMod]

tsymb_intro :: [TSIntro]
tsymb_intro = [TSPurpose, SymbConvention
  [Lit (nw CT.heat_trans), Doc' (nw progName)], SymbOrder]

swhs_srs' :: Document
swhs_srs' = mkDoc mkSRS (for) swhs_si

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

s3 :: Section
s3 = genSysF [s3_1] (s3_2_contents progName) [] []
-- First empty list is the list of constraints

--------------------------
-- 3.1 : System Context --
--------------------------

s3_1 :: Section
s3_1 = SRS.sysCont [s3_1_contents progName, sys_context_fig, s3_1_2_intro 
  progName user, s3_1_2_respBullets] []

s3_1_2_respBullets :: Contents
s3_1_2_respBullets = Enumeration $ Bullet $ [s3_1_2_userResp input_ datum,
  s3_1_2_swhsResp]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

------------------------------
-- 3.3 : System Constraints --
------------------------------

---------------------------------------------
-- Section 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

s4 :: Section
s4 = specSysDesF (s4_intro_end swhs_pcm) [s4_1, s4_2]

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

s4_1 :: Section
s4_1 = SRS.probDesc [s4_1_intro progName phsChgMtrl sWHT]
  [s4_1_1, s4_1_2, s4_1_3]

-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

s4_1_1 :: Section
s4_1_1 = termDefnF Nothing [s4_1_1_bullets]

-- Above paragraph is repeated in all examples, can be abstracted out. (Note:
-- GlassBR has an additional sentence with a reference at the end.)

s4_1_1_bullets :: Contents
s4_1_1_bullets = Enumeration (Bullet $ map s4_1_1_bullet_map_f
  [CT.ht_flux, phase_change_material, CT.heat_cap_spec,
  CT.thermal_conduction, transient])

s4_1_1_bullet_map_f :: Concept c => c -> ItemType
s4_1_1_bullet_map_f c = Flat $ foldlSent [at_start c +: EmptyS, (c ^. defn)]

-- Structure of this list is same in all examples, probably can be automated.

-- Included heat flux and specific heat in NamedChunks even though they are
-- already in SWHSUnits

-----------------------------------------
-- 4.1.2 : Physical System Description --
-----------------------------------------

s4_1_2 :: Section
s4_1_2 = physSystDesc (short progName) (fig_tank) [s4_1_2_list, fig_tank]

-- Above paragraph is general except for progName and figure. However, not
-- every example has a physical system. Also, the SSP example is different, so
-- this paragraph can not be abstracted out as is.

s4_1_2_list :: Contents
s4_1_2_list = enumSimple 1 (short physSyst) $
  map foldlSent_ s4_1_2_physSystList

s4_1_2_physSystList :: [[Sentence]]
s4_1_2_physSystList = [physSyst1 tank water, physSyst2 coil tank ht_flux_C,
  physSyst3 phsChgMtrl tank ht_flux_P]

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

s4_1_3 :: Section
s4_1_3 = SRS.goalStmt [s4_1_3_intro temp_C temp_W temp_PCM, s4_1_3_list] []

s4_1_3_list :: Contents
s4_1_3_list = enumSimple 1 (short goalStmt) $
  map (goalState) outputConstraints

-- List structure is repeated between examples. (For all of these lists I am
-- imagining the potential for something like what was done with the lists in
-- MG, where you define goals, assumptions, physical system components, etc. in
-- separate files, import them and pass them as arguments to some "makeSRS"
-- function and the rest is automated.)

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

s4_2 :: Section
s4_2 = solChSpecF progName (s4_1, s6) s4_2_4_intro_end
  (s4_2_6_mid, dataConstraintUncertainty, s4_2_6_T1footer quantity surArea
  vol htTransCoeff_min phsChgMtrl) (s4_2_1_list, 
  s4_2_2_swhsTMods, s4_2_3_genDefs ++ s4_2_3_deriv,
  s4_2_4_swhsDataDefs, s4_2_5_IModsWithDerivs, s4_2_6_DataConTables) [s4_2_7]

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

s4_2_1 :: Section
s4_2_1 = assumpF
  (SRS.thModel SRS.missingP [])
  (SRS.genDefn SRS.missingP [])
  (SRS.dataDefn SRS.missingP [])
  s4_2_5 s6 s4_2_1_list

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
s4_2_3_genDefs :: [Contents]
s4_2_3_genDefs = map swhsSymbMapT swhsGenDefs

s4_2_3_deriv :: [Contents]
s4_2_3_deriv = [s4_2_3_deriv_1 rOfChng temp,
  s4_2_3_deriv_2 t1ConsThermE vol,
  s4_2_3_deriv_3,
  s4_2_3_deriv_4 gauss_div surface vol thFluxVect uNormalVect unit_,
  s4_2_3_deriv_5,
  s4_2_3_deriv_6 vol vol_ht_gen,
  s4_2_3_deriv_7,
  s4_2_3_deriv_8 ht_flux_in ht_flux_out in_SA out_SA density heat_cap_spec
    temp vol assumption assump3 assump4 assump5 assump6,
  s4_2_3_deriv_9,
  s4_2_3_deriv_10 density mass vol,
  s4_2_3_deriv_11]


------------------------------
-- 4.2.4 : Data Definitions --
------------------------------
-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

s4_2_5 :: Section
s4_2_5 = inModelF s4_1
  (SRS.dataDefn SRS.missingP [])
  (SRS.thModel SRS.missingP [])
  (SRS.genDefn SRS.missingP [])
  s4_2_5_IModsWithDerivs

s4_2_5_IModsWithDerivs :: [Contents]
s4_2_5_IModsWithDerivs = concat $ weave [s4_2_5_derivations,
  map (\x -> [swhsSymbMapT x]) s4_2_5_IMods]

s4_2_5_derivations :: [[Contents]]
s4_2_5_derivations = [s4_2_5_subpar solution temp_W temp_PCM pcm_E 
  CT.phase_change, s4_2_5_deriv1, s4_2_5_deriv2]
  
s4_2_5_deriv1 :: [Contents]
s4_2_5_deriv1 = (s4_2_5_d1startPara energy water) ++
  (weave [s4_2_5_d1sent_list, s4_2_5_d1eqn_list])

s4_2_5_d1eqn_list = map EqnBlock [s4_2_5_d_eqn1, s4_2_5_d_eqn2,
  s4_2_5_d_eqn3, s4_2_5_d_eqn4, s4_2_5_d_eqn5, s4_2_5_d_eqn6, s4_2_5_d_eqn7]

s4_2_5_d1sent_list = map foldlSPCol
  [s4_2_5_d1sent_1 rOfChng temp_W energy water vol w_vol w_mass htCap_W 
    ht_flux_C ht_flux_P coil_SA pcm_SA CT.heat_trans tank perfect_insul 
    vol_ht_gen assump15 assump16,
  s4_2_5_d1sent_2 dd1HtFluxC dd2HtFluxP ht_flux_C ht_flux_P,
  s4_2_5_d1sent_3 w_mass htCap_W,
  s4_2_5_d1sent_4 rightSide coil_HTC coil_SA,
  s4_2_5_d1sent_5, s4_2_5_d1sent_6, s4_2_5_d1sent_7]

s4_2_5_deriv2 :: [Contents]
s4_2_5_deriv2 =
  (s4_2_5_d2startPara energy phsChgMtrl sens_heat rOfChng temp_PCM vol pcm_vol
    pcm_mass htCap_S_P ht_flux_P pcm_SA ht_flux_out vol_ht_gen assump16) ++
  (weave [s4_2_5_d2eqn_list, s4_2_5_d2sent_list]) ++ (s4_2_5_d2endPara 
  phsChgMtrl htCap_S_P htCap_L_P tau_S_P tau_L_P surface CT.melting vol 
  temp_PCM temp_melt_P CT.boiling solid liquid)

s4_2_5_d2sent_list = map foldlSPCol [s4_2_5_d2sent_1 dd2HtFluxP ht_flux_P,
  s4_2_5_d2sent_2, s4_2_5_d2sent_3]

s4_2_5_d2eqn_list = map (EqnBlock) [s4_2_5_d2eqn1, s4_2_5_d2eqn2,
  s4_2_5_d2eqn3, s4_2_5_d2eqn4]

----------------------------
-- 4.2.6 Data Constraints --
----------------------------
------------------------------
-- Data Constraint: Table 1 --
------------------------------

s4_2_6_DataConTables :: [Contents]
s4_2_6_DataConTables = [s4_2_6_table1, s4_2_6_table3]

s4_2_6_table1 :: Contents
s4_2_6_table1 = inDataConstTbl inputConstraints

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

s4_2_6_table3 :: Contents
s4_2_6_table3 = outDataConstTbl outputConstraints
--FIXME: add "(by A11)" in Physical Constraints of `temp_W` and `temp_PCM`?

outputConstraints :: [UncertQ]
outputConstraints = [temp_W, temp_PCM, w_E, pcm_E]

-- Other Notes:
---- Will there be a way to have asterisks for certain pieces of the table?

----------------------------------------------
-- 4.2.7 : Properties of A Correct Solution --
----------------------------------------------

s4_2_7 :: Section
s4_2_7 = SRS.propCorSol (s4_2_7_deriv) []

s4_2_7_deriv :: [Contents]
s4_2_7_deriv =
  [s4_2_7_deriv_1 CT.law_cons_energy w_E energy coil phsChgMtrl dd1HtFluxC
    dd2HtFluxP surface CT.heat_trans,
  s4_2_7_deriv_2,
  s4_2_7_deriv_3 pcm_E energy phsChgMtrl water,
  s4_2_7_deriv_4,
  s4_2_7_deriv_5 equation progName rightSide]

-- Above section only occurs in this example (although maybe it SHOULD be in
-- the others).

-- Remember to insert references in above derivation when available

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------

s5 :: Section
s5 = reqF [s5_1, s5_2]

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

s5_1 :: Section
s5_1 = SRS.funcReq s5_1_list []

s5_1_list :: [Contents]
s5_1_list = (acroNumGen [req1] 1) ++ [s5_1_1_Table] ++ (acroNumGen [req2] 2) ++
  [s5_1_2_Eqn1, s5_1_2_Eqn2] ++ (acroNumGen s5_1_Reqs 3) 

s5_1_1_Table :: Contents
s5_1_1_Table = (Table [titleize symbol_, titleize unit_, titleize description]
  (mkTable
  [getES,
  --(\ch -> Sy (unit_symb ch)),
  unit'2Contents,
  phrase] (map qw inputConstraints))
  (titleize input_ +:+ titleize variable +:+ titleize requirement) False)

s5_1_Reqs :: [Contents]
s5_1_Reqs = [req3, req4, req5, req6, req7, req8, req9, req10, req11]

---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------
--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

s6 :: Section
s6 = SRS.likeChg s6_list []

s6_list :: [Contents]
s6_list = acroNumGen s6_likeChg_list 1

s6_likeChg_list :: [Contents]
s6_likeChg_list = [likeChg1, likeChg2, likeChg3, likeChg4, likeChg5, likeChg6]

--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

s7 :: Section
s7 = traceMGF s7_refList s7_trailing
  ([s7_table1, s7_table2, s7_table3] ++
  (s7_intro2) ++ [s7_fig1, s7_fig2]) []

s7_refList :: [Contents]
s7_refList = [s7_table1, s7_table2, s7_table3]

s7_trailing :: [Sentence]
s7_trailing = [s7_trailing_1, s7_trailing_2, s7_trailing_3]

s7_instaModel, s7_data, s7_funcReq, s7_likelyChg, s7_dataDefs, s7_genDefs,
  s7_assump, s7_theories :: [String]
  
s7_dataRef, s7_funcReqRef, s7_instaModelRef, s7_assumpRef, s7_theoriesRef,
  s7_dataDefRef, s7_likelyChgRef, s7_genDefRef :: [Sentence]

s7_instaModel = ["IM1", "IM2", "IM3", "IM4"]
s7_instaModelRef = map (refFromType Theory) s4_2_5_IMods

s7_funcReq = ["R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
  "R11"]
s7_funcReqRef = makeListRef s7_funcReq s5_1

s7_data = ["Data Constraints"]
s7_dataRef = [makeRef s4_2_6_table1] --FIXME: Reference section?

s7_assump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
  "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19"]
s7_assumpRef = makeListRef s7_assump s4_2_1

s7_theories = ["T1", "T2", "T3"]
s7_theoriesRef = map (refFromType Theory) tModels

s7_genDefs = ["GD1", "GD2"]
s7_genDefRef = map (refFromType Theory) swhsGenDefs

s7_dataDefs = ["DD1", "DD2", "DD3", "DD4"]
s7_dataDefRef = map (refFromType Data) swhsDataDefs

s7_likelyChg = ["LC1", "LC2", "LC3", "LC4", "LC5", "LC6"]
s7_likelyChgRef = makeListRef s7_likelyChg s6

{-Traceability Matrix 1-}

s7_row_t1 :: [String]
s7_row_t1 = s7_theories ++ s7_genDefs ++ s7_dataDefs ++ s7_instaModel

s7_row_header_t1 :: [Sentence]
s7_row_header_t1 = zipWith itemRefToSent s7_row_t1 
  (s7_theoriesRef ++ s7_genDefRef ++ s7_dataDefRef ++ s7_instaModelRef)

s7_columns_t1 :: [[String]]
s7_columns_t1 = [s7_t1_T1, s7_t1_T2, s7_t1_T3, s7_t1_GD1, s7_t1_GD2, s7_t1_DD1,
  s7_t1_DD2, s7_t1_DD3, s7_t1_DD4, s7_t1_IM1, s7_t1_IM2, s7_t1_IM3, s7_t1_IM4]

s7_t1_T1, s7_t1_T2, s7_t1_T3, s7_t1_GD1, s7_t1_GD2, s7_t1_DD1, s7_t1_DD2,
  s7_t1_DD3, s7_t1_DD4, s7_t1_IM1, s7_t1_IM2, s7_t1_IM3, s7_t1_IM4 :: [String]

--list of each item that "X" item requires for traceability matrix
s7_t1_T1 = []
s7_t1_T2 = ["T3"]
s7_t1_T3 = []
s7_t1_GD1 = []
s7_t1_GD2 = ["T1"]
s7_t1_DD1 = ["GD1"]
s7_t1_DD2 = ["GD1"]
s7_t1_DD3 = []
s7_t1_DD4 = ["DD3"]
s7_t1_IM1 = ["GD2", "DD1", "DD2", "IM2"]
s7_t1_IM2 = ["GD2", "DD2", "DD4", "IM1", "IM4"]
s7_t1_IM3 = ["T2"]
s7_t1_IM4 = ["T2", "T3", "DD2", "DD3", "DD4", "IM2"]

{-Traceability Matrix 2-}

s7_row_t2 :: [String]
s7_row_t2 = s7_instaModel ++ s7_data ++ s7_funcReq

--column header
s7_row_header_t2 :: [Sentence]
s7_row_header_t2 = zipWith itemRefToSent s7_row_t2 
  (s7_instaModelRef ++ s7_dataRef ++ s7_funcReqRef)

--row header
s7_col_header_t2 :: [Sentence]
s7_col_header_t2 = zipWith itemRefToSent (s7_instaModel ++ s7_funcReq)
  (s7_instaModelRef ++ s7_funcReqRef)

s7_columns_t2 :: [[String]]
s7_columns_t2 = [s7_t2_IM1, s7_t2_IM2, s7_t2_IM3, s7_t2_IM4, s7_t2_R1, 
  s7_t2_R2, s7_t2_R3, s7_t2_R4, s7_t2_R5, s7_t2_R6, s7_t2_R7, s7_t2_R8, 
  s7_t2_R9, s7_t2_R10, s7_t2_R11]

s7_t2_IM1, s7_t2_IM2, s7_t2_IM3, s7_t2_IM4, s7_t2_R1, s7_t2_R2,
  s7_t2_R3, s7_t2_R4, s7_t2_R5, s7_t2_R6, s7_t2_R7, s7_t2_R8, 
  s7_t2_R9, s7_t2_R10, s7_t2_R11 :: [String]

--list of each item that "X" item requires for traceability matrix
s7_t2_IM1 = ["IM2", "R1", "R2"]
s7_t2_IM2 = ["IM1", "IM4", "R1", "R2"]
s7_t2_IM3 = ["R1", "R2"]
s7_t2_IM4 = ["IM2", "R1", "R2"]
s7_t2_R1 = []
s7_t2_R2 = ["R1"]
s7_t2_R3 = ["Data Constraints"]
s7_t2_R4 = ["IM1", "IM2", "R1", "R2"]
s7_t2_R5 = ["IM1"]
s7_t2_R6 = ["IM2"]
s7_t2_R7 = ["IM3"]
s7_t2_R8 = ["IM4"]
s7_t2_R9 = ["IM3", "IM4"]
s7_t2_R10 = ["IM2"]
s7_t2_R11 = ["IM2"]

s7_table2 :: Contents
s7_table2 = Table (EmptyS:s7_row_header_t2)
  (makeTMatrix (s7_col_header_t2) (s7_columns_t2) (s7_row_t2))
  (showingCxnBw traceyMatrix
  (titleize' requirement `sAnd` titleize' inModel)) True

{-Traceability Matrix 3-}

s7_row_t3 :: [String]
s7_row_t3 = s7_assump

s7_row_header_t3, s7_col_header_t3 :: [Sentence]
s7_row_header_t3 = zipWith itemRefToSent s7_assump s7_assumpRef

s7_col_header_t3 = zipWith itemRefToSent
  (s7_theories ++ s7_genDefs ++ s7_dataDefs ++ s7_instaModel ++ s7_likelyChg)
  (s7_theoriesRef ++ s7_genDefRef ++ s7_dataDefRef ++ s7_instaModelRef ++ 
    s7_likelyChgRef)

s7_columns_t3 :: [[String]]
s7_columns_t3 = [s7_t3_T1, s7_t3_T2, s7_t3_T3, s7_t3_GD1, s7_t3_GD2, s7_t3_DD1,
  s7_t3_DD2, s7_t3_DD3, s7_t3_DD4, s7_t3_IM1, s7_t3_IM2, s7_t3_IM3, s7_t3_IM4,
  s7_t3_LC1, s7_t3_LC2, s7_t3_LC3, s7_t3_LC4, s7_t3_LC5, s7_t3_LC6]

s7_t3_T1, s7_t3_T2, s7_t3_T3, s7_t3_GD1, s7_t3_GD2, s7_t3_DD1, s7_t3_DD2, 
  s7_t3_DD3, s7_t3_DD4, s7_t3_IM1, s7_t3_IM2, s7_t3_IM3, s7_t3_IM4, s7_t3_LC1,
  s7_t3_LC2, s7_t3_LC3, s7_t3_LC4, s7_t3_LC5, s7_t3_LC6 :: [String]

s7_t3_T1  = ["A1"]
s7_t3_T2  = []
s7_t3_T3  = []
s7_t3_GD1 = ["A2"]
s7_t3_GD2 = ["A3", "A4", "A5", "A6"]
s7_t3_DD1 = ["A7", "A8", "A9"]
s7_t3_DD2 = ["A3", "A4", "A10"]
s7_t3_DD3 = []
s7_t3_DD4 = []
s7_t3_IM1 = ["A11", "A12", "A14", "A15", "A16", "A19"]
s7_t3_IM2 = ["A12", "A13", "A16", "A17", "A18"]
s7_t3_IM3 = ["A14", "A19"]
s7_t3_IM4 = ["A13", "A18"]
s7_t3_LC1 = ["A4"]
s7_t3_LC2 = ["A8"]
s7_t3_LC3 = ["A9"]
s7_t3_LC4 = ["A11"]
s7_t3_LC5 = ["A12"]
s7_t3_LC6 = ["A15"]


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

s2_intro :: ConceptChunk -> UnitalChunk -> ConceptChunk -> CI -> CI ->
  ConceptChunk -> UnitalChunk -> ConceptChunk -> Sentence
s2_intro es en sp pcmat pro te lh un = foldlSent [
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

s2_kSent :: ConceptChunk -> ConceptChunk -> CI -> Sentence
s2_kSent sp pr pro = foldlSent_ [EmptyS +:+. phrase sp, S "The developed",
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

s2_1_par1 :: ConceptChunk -> CI -> Sentence
s2_1_par1 sp pro = foldlSent [S "The main", phrase purpose, S "of this",
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

s2_2_contents :: ConceptChunk -> ConceptChunk -> Sentence
s2_2_contents ta tp = foldlSent_ [phrase ta,
  S "of a single", phrase tp]

s2_2_end :: UnitalChunk -> ConceptChunk -> ConceptChunk -> CI ->
  ConceptChunk -> Sentence
s2_2_end t te wa pcmat sw = foldlSent_ [S "predict the",
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

s2_3_knowlegde :: ConceptChunk -> Sentence
s2_3_knowlegde htt = foldlSent_ [EmptyS +:+. phrase htt,
  S "A third or fourth year Mechanical Engineering course on this topic",
  S "is recommended"]

s2_3_understanding :: CI -> Sentence
s2_3_understanding diffeq = foldlSent_ [(plural diffeq) `sC`
  S "as typically covered in first and second year Calculus courses"]

------------------------------------
-- 2.4 : Organization of Document --
------------------------------------

s2_4_intro :: Sentence
s2_4_intro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the template for an", short srs,
  S "for", phrase sciCompS, S "proposed by", (sSqBrNum 3) `sAnd`
  (sSqBrNum 6), sParen (makeRef (SRS.reference SRS.missingP []))]

s2_4_trail :: ConceptChunk -> CI -> Sentence
s2_4_trail sp pro = foldlSent_ [S "The", plural inModel,
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

s3_1_contents :: CI -> Contents
s3_1_contents pro = foldlSP [makeRef sys_context_fig, S "shows the" +:+.
  phrase sysCont, S "A circle represents an external entity outside the",
  phrase software `sC` S "the", phrase user, S "in this case. A",
  S "rectangle represents the", phrase softwareSys, S "itself" +:+.
  sParen (short pro), S "Arrows are used to show the",
  plural datum, S "flow between the", phrase system `sAnd`
  S "its", phrase environment]

sys_context_fig :: Contents
sys_context_fig = fig (foldlSent_
  [makeRef sys_context_fig +: EmptyS, titleize sysCont])
  "SystemContextFigure.png"

s3_1_2_intro :: CI -> NamedChunk -> Contents
s3_1_2_intro pro us = foldlSPCol [short pro +:+. S "is mostly self-contained",
  S "The only external interaction is through the", phrase us +:+.
  S "interface", S "responsibilities" `ofThe'` phrase us `sAnd`
  S "the", phrase system, S "are as follows"]

-- User Responsibilities --
s3_1_2_userResp :: NamedChunk -> NamedChunk -> ItemType
s3_1_2_userResp inp dat = Nested (titleize user +: S "Responsibilities")
  $ Bullet $ map (\c -> Flat c) [

  foldlSent_ [S "Provide the", phrase inp, plural dat, S "to the",
  phrase system `sC` S "ensuring no errors in the", plural dat, S "entry"],

  foldlSent_ [S "Take care that consistent", plural unit_,
  S "are used for", phrase inp, plural variable]

  ]

-- SWHS Responsibilities --
s3_1_2_swhsResp :: ItemType
s3_1_2_swhsResp = Nested (short progName +: S "Responsibilities")
  $ Bullet $ map (\c -> Flat c) [

  foldlSent_ [S "Detect", plural datum, S "type mismatch, such as a string of",
  S "characters instead of a floating point number"],

  foldlSent_ [S "Determine if the", plural input_, S "satisfy the required",
  phrase physical `sAnd` phrase software, plural constraint],

  foldlSent_ [S "Calculate the required", plural output_]

  ]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

s3_2_contents :: CI -> Contents
s3_2_contents pro = foldlSP [S "The end", phrase user, S "of",
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

s4_intro_end :: ConceptChunk -> Sentence
s4_intro_end sw = foldlSent_ [foldlsC (map plural (take 3 renameList1))
  `sC` S "and finally the", plural inModel, sParen (short ode :+: S "s"),
  S "that", phrase model, S "the", phrase sw]

-- Completely general except for solar water heating tank (object of analysis)
-- and similar between all examples; can be abstracted out.

-- The swhs_pcm reference at the end would be better if singular, but concept
-- is plural.

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

s4_1_intro :: CI -> CI -> ConceptChunk -> Contents
s4_1_intro pro pcmat sw = foldlSP [short pro, S "is a", phrase compPro,
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
  sParen (getES hfc +:+ S "represents the" +:+. phrase hfc)]
--
physSyst3 :: CI -> ConceptChunk -> UnitalChunk -> [Sentence]
physSyst3 pcmat ta hfp = [short pcmat, S "suspended in" +:+. phrase ta,
  sParen (getES hfp +:+ S "represents the" +:+. phrase hfp)]

-- Structure of list would be same between examples but content is completely
-- different

fig_tank :: Contents
fig_tank = fig (
  foldlSent_ [at_start sWHT `sC` S "with", phrase ht_flux_C, S "of",
  getES ht_flux_C `sAnd` phrase ht_flux_P, S "of", getES ht_flux_P])
  "Tank.png"

-----------------------------
-- 4.1.3 : Goal Statements --
-----------------------------

s4_1_3_intro :: UncertQ -> UncertQ -> UncertQ -> Contents
s4_1_3_intro temc temw tempcm = foldlSPCol [S "Given the", phrase temc `sC`
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

s4_2_3_deriv_3, s4_2_3_deriv_5, s4_2_3_deriv_7, s4_2_3_deriv_9,
  s4_2_3_deriv_11 :: Contents

s4_2_3_deriv_1 :: ConceptChunk -> UnitalChunk -> Contents
s4_2_3_deriv_1 roc tem = foldlSPCol [S "Detailed derivation of simplified",
  phrase roc, S "of", phrase tem]

s4_2_3_deriv_2 :: RelationConcept -> UnitalChunk -> Contents
s4_2_3_deriv_2 t1ct vo = foldlSPCol [S "Integrating", swhsSymbMapTRef t1ct,
  S "over a", phrase vo, sParen (getES vo) `sC` S "we have"]

s4_2_3_deriv_3 = EqnBlock
  ((negate (int_all (eqSymb vol) ((C gradient) $. (C thFluxVect)))) +
  (int_all (eqSymb vol) (C vol_ht_gen)) $=
  (int_all (eqSymb vol) ((C density) * (C heat_cap_spec) * Deriv Part (C temp) time)))

s4_2_3_deriv_4 :: ConceptChunk -> ConVar -> UnitalChunk -> UnitalChunk ->
  ConVar -> ConceptChunk -> Contents
s4_2_3_deriv_4 gd su vo tfv unv un = foldlSPCol [S "Applying", titleize gd,
  S "to the first term over", (phrase su +:+ getES su `ofThe` phrase vo) `sC`
  S "with", getES tfv, S "as the", phrase tfv, S "for the",
  phrase surface `sAnd` getES unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

s4_2_3_deriv_5 = EqnBlock
  ((negate (int_all (eqSymb surface) ((C thFluxVect) $. (C uNormalVect)))) +
  (int_all (eqSymb vol) (C vol_ht_gen)) $= 
  (int_all (eqSymb vol) ((C density) * (C heat_cap_spec) * Deriv Part (C temp) time)))

s4_2_3_deriv_6 :: UnitalChunk -> UnitalChunk -> Contents
s4_2_3_deriv_6 vo vhg = foldlSPCol [S "We consider an arbitrary" +:+.
  phrase vo, S "The", phrase vhg, S "is assumed constant. Then",
  sParen $ S $ show (1 :: Integer), S "can be written as"]

s4_2_3_deriv_7 = EqnBlock
  ((C ht_flux_in) * (C in_SA) - (C ht_flux_out) *
  (C out_SA) + (C vol_ht_gen) * (C vol) $= 
  (int_all (eqSymb vol) ((C density) * (C heat_cap_spec) * Deriv Part (C temp) time)))

s4_2_3_deriv_8 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk -> CI -> Contents ->
  Contents -> Contents -> Contents -> Contents
s4_2_3_deriv_8 hfi hfo isa osa den hcs tem vo assu a3 a4 a5 a6 = foldlSPCol 
  [S "Where", foldlList (map getES [hfi, hfo, isa, osa]), 
  S "are explained in" +:+. acroGD 2, S "Assuming", getES den `sC` getES hcs
  `sAnd` getES tem, S "are constant over the", phrase vo `sC` S "which is true",
  S "in our case by", titleize' assu, 
  foldlList (map (\c -> sParen (makeRef c)) [a3, a4, a5, a6]) `sC` S "we have"]

s4_2_3_deriv_9 = EqnBlock
  ((C density) * (C heat_cap_spec) * (C vol) * Deriv Total (C temp)
  time $= (C ht_flux_in) * (C in_SA) - (C ht_flux_out) *
  (C out_SA) + (C vol_ht_gen) * (C vol))

s4_2_3_deriv_10 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> Contents
s4_2_3_deriv_10 den ma vo = foldlSPCol [S "Using the fact that", getES den :+:
  S "=" :+: getES ma :+: S "/" :+: getES vo `sC` S "(2) can be written as"]

s4_2_3_deriv_11 = EqnBlock
  ((C mass) * (C heat_cap_spec) * Deriv Total (C temp)
  time $= (C ht_flux_in) * (C in_SA) - (C ht_flux_out)
  * (C out_SA) + (C vol_ht_gen) * (C vol))

-- Created a unitalChunk for "S"... should I add it to table of symbols?
-- Add references to above when available (assumptions, GDs)
-- Replace relevant Derivs with the regular derivative when it is available

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------

s4_2_4_intro_end :: Sentence
s4_2_4_intro_end = foldlSent [S "The dimension of each",
  phrase quantity, S "is also given"]

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

s4_2_5_subpar :: NamedChunk -> UncertQ -> UncertQ -> UncertQ -> ConceptChunk
  -> [Contents]
s4_2_5_subpar sol temw tempcm epcm pc = [foldlSP [S "The goals", acroGS 1,
  S "to", acroGS 4, S "are solved by", acroIM 1, S "to" +:+. acroIM 4,
  S "The", plural sol, S "for", acroIM 1 `sAnd` acroIM 2, 
  S "are coupled since the", phrase sol, S "for", getES temw `sAnd` getES tempcm
  +:+. S "depend on one another", acroIM 3, S "can be solved once", acroIM 1, 
  S "has been solved. The", phrase sol `sOf` acroIM 2 `sAnd` acroIM 4, 
  S "are also coupled, since the", phrase tempcm `sAnd` phrase epcm, 
  S "depend on the", phrase pc]]

s4_2_5_d1startPara :: UnitalChunk -> ConceptChunk -> [Contents]
s4_2_5_d1startPara en wa = [foldlSPCol [S "Derivation of the",
  phrase en, S "balance on", phrase wa]]

s4_2_5_d1sent_1 :: ConceptChunk -> UncertQ -> UnitalChunk -> ConceptChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UncertQ -> UnitalChunk -> 
  UnitalChunk -> UncertQ -> UncertQ -> ConceptChunk -> ConceptChunk -> 
  ConceptChunk -> UnitalChunk -> Contents -> Contents -> [Sentence]
s4_2_5_d1sent_1 roc temw en wa vo wvo wma hcw hfc hfp csa psa ht ta purin vhg
  a15 a16 = [S "To find the", phrase roc, S "of", getES temw `sC`
  S "we look at the", phrase en, S "balance on" +:+.
  phrase wa, S "The", phrase vo, S "being considered" `isThe`
  phrase wvo, getES wvo `sC` S "which has", phrase wma +:+.
  (getES wma `sAnd` phrase hcw `sC` getES hcw),
  getES hfc, S "represents the", phrase hfc `sAnd`
  getES hfp, S "represents the", phrase hfp `sC`
  S "over", phrase csa `sAnd` phrase psa, S "of",
  getES csa `sAnd` getES psa `sC` S "respectively. No",
  phrase ht, S "occurs to", (S "outside" `ofThe`
  phrase ta) `sC` S "since it has been assumed to be",
  phrase purin +:+. sParen (makeRef a15), S "Assuming no",
  phrase vhg +:+. (sParen (makeRef a16) `sC`
  (E $ C vhg $= 0)), S "Therefore, the", phrase equation, S "for",
  acroGD 2, S "can be written as"]

s4_2_5_d1sent_2 :: QDefinition -> QDefinition -> UnitalChunk ->
  UnitalChunk -> [Sentence]
s4_2_5_d1sent_2 d1hf d2hf hfc hfp = [S "Using", swhsSymbMapDRef d1hf `sAnd`
  swhsSymbMapDRef d2hf, S "for", getES hfc `sAnd`
  getES hfp, S "respectively, this can be written as"]

s4_2_5_d1sent_3 :: UnitalChunk -> UncertQ -> [Sentence]
s4_2_5_d1sent_3 wm hcw = [S "Dividing (3) by", getES wm :+: getES hcw `sC`
  S "we obtain"]

s4_2_5_d1sent_4 :: CI -> UncertQ -> UncertQ -> [Sentence]
s4_2_5_d1sent_4 rs chtc csa = [S "Factoring the negative sign out of",
  S "second term" `ofThe` short rs,
  S "of", titleize equation,
  S "(4) and multiplying it by",
  getES chtc :+: getES csa :+: S "/" :+:
  getES chtc :+: getES csa, S "yields"]

s4_2_5_d1sent_5 :: [Sentence]
s4_2_5_d1sent_5 = [S "Which simplifies to"]

s4_2_5_d1sent_6 :: [Sentence]
s4_2_5_d1sent_6 = [S "Setting",
  (E $ C tau_W $= (C w_mass * C htCap_W) / (C coil_HTC * C coil_SA)) `sAnd`
  (E $ C eta $= (C pcm_HTC * C pcm_SA) / (C coil_HTC * C coil_SA)) `sC`
  titleize equation, S "(5) can be written as"]

s4_2_5_d1sent_7 :: [Sentence]
s4_2_5_d1sent_7 = [S "Finally, factoring out", (E $ 1 / C tau_W) `sC` 
  S "we are left with the governing", short ode, S "for", acroIM 1]

s4_2_5_d_eqn1, s4_2_5_d_eqn2, s4_2_5_d_eqn3, s4_2_5_d_eqn4, s4_2_5_d_eqn5,
  s4_2_5_d_eqn6, s4_2_5_d_eqn7 :: Expr

s4_2_5_d_eqn1 = ((C w_mass) * (C htCap_W) * Deriv Total (C temp_W) time $=
  (C ht_flux_C) * (C coil_SA) - (C ht_flux_P) * (C pcm_SA))

s4_2_5_d_eqn2 = ((C w_mass) * (C htCap_W) * Deriv Total (C temp_W) time $=
  (C coil_HTC) * (C coil_SA) * ((C temp_C) - (C temp_W)) -
  (C pcm_HTC) * (C pcm_SA) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d_eqn3 = (Deriv Total (C temp_W) time $= ((C coil_HTC) *
  (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) -
  (C temp_W)) - ((C pcm_mass) * (C pcm_SA)) / ((C w_mass) *
  (C htCap_W)) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d_eqn4 = (Deriv Total (C temp_W) time $= ((C coil_HTC) *
  (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) - (C temp_W)) +
  (((C coil_HTC) * (C coil_SA)) / ((C coil_HTC) * (C coil_SA))) *
  (((C pcm_HTC) * (C pcm_SA)) / ((C w_mass) * (C htCap_W))) *
  ((C temp_PCM) - (C temp_W)))

s4_2_5_d_eqn5 = (Deriv Total (C temp_W) time $= ((C coil_HTC) *
  (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) - (C temp_W)) +
  (((C pcm_HTC) * (C pcm_SA)) / ((C coil_HTC) * (C coil_SA))) *
  (((C coil_HTC) * (C coil_SA)) / ((C w_mass) * (C htCap_W))) *
  ((C temp_PCM) - (C temp_W)))

s4_2_5_d_eqn6 = (Deriv Total (C temp_W) time $= (1 / (C tau_W)) *
  ((C temp_C) - (C temp_W)) + ((C eta) / (C tau_W)) *
  ((C temp_PCM) - (C temp_W)))

s4_2_5_d_eqn7 = (Deriv Total (C temp_W) time $= (1 / (C tau_W)) *
  (((C temp_C) - (C temp_W)) + (C eta) * ((C temp_PCM) -
  (C temp_W))))

-- Should "energy balance" be a concept?
-- Add IM, GD, A, and EqnBlock references when available
-- Replace Derivs with regular derivative when available
-- Fractions in paragraph?

s4_2_5_d2sent_1 :: QDefinition -> UnitalChunk -> [Sentence]
s4_2_5_d2sent_1 d2hfp hfp = [S "Using", swhsSymbMapDRef d2hfp, S "for", 
  getES hfp `sC` S "this", phrase equation, S "can be written as"]

s4_2_5_d2sent_2 :: [Sentence]
s4_2_5_d2sent_2 = [S "Dividing by", getES pcm_mass :+: getES htCap_S_P,
  S "we obtain"]

s4_2_5_d2sent_3 :: [Sentence]
s4_2_5_d2sent_3 = [S "Setting", getES tau_S_P :+: S "=" :+: getES pcm_mass :+: 
  getES htCap_S_P :+: S "/" :+: getES pcm_HTC :+: getES pcm_SA `sC`
  S "this can be written as"]

s4_2_5_d2eqn1, s4_2_5_d2eqn2, s4_2_5_d2eqn3, s4_2_5_d2eqn4 :: Expr

s4_2_5_d2eqn1 = ((C pcm_mass) * (C htCap_S_P) * Deriv Total (C temp_PCM)
  time $= (C ht_flux_P) * (C pcm_SA))

s4_2_5_d2eqn2 = ((C pcm_mass) * (C htCap_S_P) * Deriv Total (C temp_PCM)
  time $= (C pcm_HTC) * (C pcm_SA) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d2eqn3 = (Deriv Total (C temp_PCM) time $= ((C pcm_HTC) *
  (C pcm_SA)) / ((C pcm_mass) * (C htCap_S_P)) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d2eqn4 = (Deriv Total (C temp_PCM) time $= (1 / (C tau_S_P)) *
  ((C temp_W) - (C temp_PCM)))

s4_2_5_d1eqn_list, s4_2_5_d1sent_list, s4_2_5_d2eqn_list, 
  s4_2_5_d2sent_list :: [Contents]

s4_2_5_d2startPara :: UnitalChunk -> CI -> UnitalChunk -> ConceptChunk ->
  UncertQ -> UnitalChunk -> UncertQ -> UnitalChunk -> UncertQ -> 
  UnitalChunk -> UncertQ -> UnitalChunk -> UnitalChunk ->
  Contents -> [Contents]
s4_2_5_d2startPara en pcmat sh roc ptem vo pvo pma hcsp hfp psa hfo vhg a16 =
  map foldlSPCol [

  [S "Detailed derivation of the", phrase en, S "balance on the",
  short pcmat, S "during", phrase sh :+: S "ing phase"],

  [S "To find the", phrase roc, S "of", getES ptem `sC`
  S "we look at the", phrase en, S "balance on the" +:+.
  short pcmat, S "The", phrase vo,
  S "being considered is the" +:+. (phrase pvo `sC` getES pvo),
  S "The derivation that follows is initially for the",
  phrase solid +:+. short pcmat, S "The", phrase pma, S "is",
  getES pma `sAnd` S "the", phrase hcsp, S "is" +:+. getES hcsp,
  S "The", phrase hfp, S "is", getES hfp, S "over",
  phrase psa +:+. getES psa, S "There is no" +:+. phrase hfo,
  S "Assuming no", phrase vhg, sParen (makeRef a16) `sC`
  getES vhg :+: S "=0, the", phrase equation, S "for", acroGD 2,
  S "can be written as"]

  ]

s4_2_5_d2endPara :: CI -> UncertQ -> UncertQ -> UnitalChunk -> UnitalChunk -> 
  ConVar -> ConceptChunk -> UnitalChunk -> UncertQ -> UncertQ -> 
  ConceptChunk -> ConceptChunk -> ConceptChunk -> [Contents]
s4_2_5_d2endPara pcmat hcsp hclp tsp tlp sur mel vo ptem tmp boi so li = map 
  foldlSP [

  [titleize equation,
  S "(6) applies for the", phrase solid +:+. short pcmat,
  S "In the case where all of the", short pcmat,
  S "is melted, the same derivation applies, except that",
  getES hcsp, S "is replaced by", getES hclp `sC`
  S "and thus", getES tsp, S "is replaced by" +:+.
  getES tlp, S "Although a small change in", phrase sur,
  S "area would be expected with", phrase mel `sC`
  S "this is not included, since",
  (phrase vo +:+ S "change" `ofThe` short pcmat),
  S "with", phrase mel,
  S "is assumed to be negligible", sParen (makeRef assump17)],

  [S "In the case where", getES ptem :+: S "=" :+:
  getES tmp `sAnd` S "not all of the", short pcmat,
  S "is melted, the", phrase ptem +:+. S "does not change",
  S "Therefore, in this case d" :+: getES ptem :+: S "/d" :+:
  getES time :+: S "=0"],

  [S "This derivation does not consider",
  (phrase boi `ofThe` short pcmat) `sC` S "as the", short pcmat,
  S "is assumed to either be in a", (so ^. defn),
  S "or a", (li ^. defn), sParen (makeRef assump18)]

  ]

-- Add GD, A, and EqnBlock references when available
-- Replace Derivs with regular derivative when available
-- Derivative notation in paragraph?


----------------------------
-- 4.2.6 Data Constraints --
----------------------------

-- I do not think Table 2 will end up being necessary for the Drasil version
---- The info from table 2 will likely end up in table 1.
s4_2_6_mid :: Sentence
s4_2_6_mid = foldlSent [S "The", phrase column, S "for", phrase software,
  plural constraint, S "restricts the range of",
  plural input_, S "to reasonable", plural value]

------------------------------
-- Data Constraint: Table 1 --
------------------------------

s4_2_6_T1footer :: NamedChunk -> UnitalChunk -> UnitalChunk -> QDefinition ->
  CI -> Sentence
s4_2_6_T1footer qua sa vo htcm pcmat = foldlSent_ $ map foldlSent [

  [sParen (S "*"), S "These", plural qua, S "cannot be equal to zero" `sC`
  S "or there will be a divide by zero in the", phrase model],

  [sParen (S "+"), S "These", plural qua, S "cannot be zero" `sC`
  S "or there would be freezing", sParen (makeRef assump13)],

  [sParen (Sp Hash), S "The", plural constraint, S "on the", phrase sa,
  S "are calculated by considering the", phrase sa, S "to", phrase vo +:+.
  S "ratio", S "The", phrase assumption, S "is that the lowest ratio is",
  (S $ show (1 :: Integer)) `sAnd`
  S "the highest possible is", E (2 / C htcm) `sC` S "where",
  E $ C htcm, S "is the thickness of a", Quote (S "sheet"), S "of" +:+.
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

s4_2_7_deriv_1 :: ConceptChunk -> UncertQ -> UnitalChunk -> ConceptChunk ->
  CI -> QDefinition -> QDefinition -> ConVar -> ConceptChunk -> Contents
s4_2_7_deriv_1 lce ewat en co pcmat d1hfc d2hfp su ht  =
  foldlSPCol [S "A", phrase corSol, S "must exhibit the" +:+.
  phrase lce, S "This means that the", phrase ewat,
  S "should equal the difference between the total", phrase en,
  phrase input_, S "from the", phrase co `sAnd` S "the",
  phrase en, phrase output_, S "to the" +:+. short pcmat,
  S "This can be shown as an", phrase equation, S "by taking",
  swhsSymbMapDRef d1hfc `sAnd` swhsSymbMapDRef d2hfp `sC`
  S "multiplying each by their respective", phrase su,
  S "area of", phrase ht `sC` S "and integrating each",
  S "over the", phrase sim_time `sC` S "as follows"]

s4_2_7_deriv_2 :: Contents
s4_2_7_deriv_2 = EqnBlock
  ((C w_E) $= (defint (eqSymb time) 0 (C time)
  ((C coil_HTC) * (C coil_SA) * ((C temp_C) - FCall (C temp_W)
  [C time]))) - (defint (eqSymb time) 0 (C time)
  ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) -
  (FCall (C temp_PCM) [C time])))))

s4_2_7_deriv_3 :: UncertQ -> UnitalChunk -> CI -> ConceptChunk -> Contents
s4_2_7_deriv_3 epcm en pcmat wa =
  foldlSP_ [S "In addition, the", phrase epcm, S "should equal the",
  phrase en, phrase input_, S "to the", short pcmat,
  S "from the" +:+. phrase wa, S "This can be expressed as"]

s4_2_7_deriv_4 :: Contents
s4_2_7_deriv_4 = EqnBlock
  ((C pcm_E) $= (defint (eqSymb time) 0 (C time)
  ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) - (FCall
  (C temp_PCM) [C time])))))

s4_2_7_deriv_5 :: ConceptChunk -> CI -> CI -> Contents
s4_2_7_deriv_5 eq pro rs = foldlSP [titleize' eq, S "(FIXME: Equation 7)" 
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

s7_trailing_1, s7_trailing_2, s7_trailing_3 :: Sentence

s7_trailing_1 = foldlSent [foldlList $ map plural (take 4 renameList1), 
  S "with each other"]

s7_trailing_2 = foldlSent [foldlList $ map plural renameList2, 
  S "on each other"]

s7_trailing_3 = foldlSent_ [foldlList $ map plural (take 5 renameList1),
  S "on the", plural assumption]

s7_table1 :: Contents
s7_table1 = Table (EmptyS:s7_row_header_t1)
  (makeTMatrix (s7_row_header_t1) (s7_columns_t1) (s7_row_t1))
  (showingCxnBw traceyMatrix
  (titleize' item +:+ S "of Different" +:+ titleize' section_)) True

s7_table3 :: Contents
s7_table3 = Table (EmptyS:s7_row_header_t3)
  (makeTMatrix s7_col_header_t3 s7_columns_t3 s7_row_t3)
  (showingCxnBw traceyMatrix (titleize' assumption `sAnd` S "Other" +:+
  titleize' item)) True

-- These matrices can probably be generated automatically when enough info is
-- abstracted out.

------------------------
-- Traceabilty Graphs --
------------------------

s7_intro2 :: [Contents]
s7_intro2 = traceGIntro [s7_fig1, s7_fig2]

  [foldlSent [foldlList $ map plural renameList1, S "on each other"],

  foldlSent_ [foldlList $ map plural renameList2, S "on each other"]]

s7_fig1 :: Contents
s7_fig1 = fig (showingCxnBw traceyGraph (titleize' item +:+
  S "of Different" +:+ titleize' section_)) "ATrace.png"

s7_fig2 :: Contents
s7_fig2 = fig (showingCxnBw traceyGraph (foldlList $ map titleize' 
  renameList2)) "RTrace.png"

-------------------------------------------------
-- Section 8 :  Specification Parameter Values --
-------------------------------------------------
----------------------------
-- Section 9 : References --
----------------------------
