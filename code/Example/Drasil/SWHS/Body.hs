module Drasil.SWHS.Body where

import Control.Lens ((^.))
import Prelude hiding (id)

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Authors

import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.PhysicalProperties hiding (density, mass, vol)
import qualified Data.Drasil.Concepts.Thermodynamics as CT
import Data.Drasil.Concepts.Physics (mech_energy)
import Data.Drasil.Concepts.Math (ode, de, unit_, rOfChng, equation, change, 
  parameter)

import Data.Drasil.Concepts.Software (program, performance)
import Data.Drasil.Software.Products
import Data.Drasil.Utils (enumSimple, weave, getS, itemRefToSent, makeListRef,
  makeTMatrix, mkRefsList, refFromType)

import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Quantities.Math (gradient, surface, uNormalVect, surArea)
import Data.Drasil.Quantities.Thermodynamics
import Data.Drasil.Quantities.PhysicalProperties (density, mass, vol)

import Data.Drasil.Software.Products (compPro)

import Drasil.SWHS.Unitals
import Drasil.SWHS.Concepts
import Drasil.SWHS.TMods
import Drasil.SWHS.IMods
import Drasil.SWHS.DataDefs
import Drasil.SWHS.GenDefs
import Drasil.SWHS.Modules
import Drasil.SWHS.Changes
import Drasil.SWHS.Reqs

import qualified Drasil.SRS as SRS
import Drasil.Template.MG
import Drasil.Template.DD
import Drasil.DocumentLanguage

import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.SpecificSystemDescription
import Drasil.Sections.TraceabilityMandGs
import Drasil.Sections.Requirements
import Drasil.Sections.GeneralSystDesc
import Drasil.Sections.AuxiliaryConstants (valsOfAuxConstantsF)

import Data.Drasil.Concepts.Software(correctness, verifiability,
  understandability, reusability, maintainability)

import Data.Drasil.SentenceStructures (showingCxnBw, foldlSent, foldlSent_,
  foldlSP, foldlSP_, foldlSPCol, foldlsC, isThe, ofThe, ofThe',
  sAnd)

acronyms :: [CI]
acronyms = [assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, ode,
  phsChgMtrl, physSyst, requirement, rightSide, srs, progName, thModel]

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

--Will there be a table of contents?

authors :: Sentence
authors = manyNames swhsPeople

swhs_si :: SystemInformation
swhs_si = SI swhs_pcm srs swhsPeople
  this_si swhsSymbols (swhsSymbols) acronyms
  (swhsDataDefs :: [QDefinition])
  ((map qs swhsInputs) :: [QSWrapper])
  ((map qs swhsOutputs) :: [QSWrapper])
  ([] :: [Block QDefinition])
  (swhsConstrained)
  --Note: The second swhsSymbols here is
    -- Redundant b/c the unitals are not really concepts (yet). There
    -- Will still likely be a better way to do this.
  --FIXME: Should be all Named, not just acronyms at the end.

swhsPeople :: [Person]
swhsPeople = [thulasi, brooks, spencerSmith]

mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro
  [TUnits, tsymb'' tsymb_intro (TermExcept [uNormalVect]), TAandA]):
  IntroSec (IntroProg (s2_intro CT.ener_src energy swhs_pcm phsChgMtrl progName
    CT.thermal_energy latent_heat unit_) (s2_kSent swhs_pcm program progName) [
  IPurpose (s2_1_par1 swhs_pcm progName),
  IScope (s2_2_contents CT.thermal_analysis tank_pcm) (s2_2_end temp CT.thermal_energy water phsChgMtrl sWHT),
  IChar (s2_3_knowlegde CT.ht_trans_theo) (s2_3_understanding de) (EmptyS),
  IOrgSec (s2_4_intro) (inModel) (SRS.inModel SRS.missingP []) (s2_4_trail swhs_pcm progName)]) :
  map Verbatim [s3, s4, s5, s6, s7, s8, s9]

tsymb_intro :: [TSIntro]
tsymb_intro = [TSPurpose, SymbConvention
  [Lit (nw CT.heat_trans), Doc' (nw progName)], SymbOrder]

swhs_srs' :: Document
swhs_srs' = mkDoc mkSRS swhs_si

-- It is sometimes hard to remember to add new sections both here and above.

mgBod :: [Section]
(mgBod, _) = makeDD likelyChanges unlikelyChanges reqs modules

swhs_mg :: Document
swhs_mg = mgDoc swhsFull authors mgBod


-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --

------------------------------
-- Section 2 : INTRODUCTION --
------------------------------

-- In Concepts.hs "swhs_pcm" gives "s for program name, and there is a
-- similar paragraph in each of the other eolar water heating systems
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
s3_1 = SRS.sysCont [s3_1_contents progName, sys_context_fig, s3_1_2_intro progName user,
  s3_1_2_respBullets] []

s3_1_2_respBullets :: Contents
s3_1_2_respBullets = Enumeration $ Bullet $ [s3_1_2_userResp input_ datum, s3_1_2_swhsResp]

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
s4_1_1 = termDefnF EmptyS [s4_1_1_bullets]

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
s4_1_2_list = enumSimple 1 (short physSyst) $ map foldlSent_ s4_1_2_physSystList

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
  (s4_2_6_mid, dataConstraintUncertainty, s4_2_6_T1footer) (s4_2_1_list, s4_2_2_T1 ++
  s4_2_2_T2 ++ s4_2_2_T3, s4_2_3_genDefs ++ s4_2_3_deriv,
  s4_2_4_DD1 ++ s4_2_4_DD2 ++ s4_2_4_DD3 ++ s4_2_4_DD4, (s4_2_5_IMods),
  s4_2_6_DataConTables) [s4_2_7]

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

s4_2_1 :: Section
s4_2_1 = assumpF
  (SRS.thModel SRS.missingP [])
  (SRS.genDefn SRS.missingP [])
  (SRS.dataDefn SRS.missingP [])
  s4_2_5 s6 s4_2_1_list

s4_2_1_list :: [Contents]
s4_2_1_list = acroNumGen s4_2_1_assump_list 1

s4_2_1_assump_list :: [Contents]
s4_2_1_assump_list = [assump1, assump2, assump3, assump4, assump5, assump6,
  assump7, assump8, assump9, assump10, assump11, assump12, assump13, assump14,
  assump15, assump16, assump17, assump18, assump19, assump20]

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
s4_2_3_deriv = [s4_2_3_deriv_1 rOfChng temp, s4_2_3_deriv_2 t1ConsThermE vol, s4_2_3_deriv_3,
  s4_2_3_deriv_4 gauss_div surface vol thFluxVect uNormalVect unit_, s4_2_3_deriv_5, s4_2_3_deriv_6, s4_2_3_deriv_7,
  s4_2_3_deriv_8, s4_2_3_deriv_9, s4_2_3_deriv_10, s4_2_3_deriv_11]


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
  s4_2_5_IMods

s4_2_5_IMods :: [Contents]
s4_2_5_IMods = concat $ weave [s4_2_5_derivations,
  map (\x -> [swhsSymbMapT x]) swhsInModels]

s4_2_5_derivations :: [[Contents]]
s4_2_5_derivations = [s4_2_5_subpar, s4_2_5_deriv1, s4_2_5_deriv2]
  
s4_2_5_deriv1 :: [Contents]
s4_2_5_deriv1 = s4_2_5_d1startPara ++
  (weave [s4_2_5_d1sent_list, s4_2_5_d1eqn_list])

s4_2_5_d1eqn_list = map EqnBlock [s4_2_5_d_eqn1, s4_2_5_d_eqn2,
  s4_2_5_d_eqn3, s4_2_5_d_eqn4, s4_2_5_d_eqn5, s4_2_5_d_eqn6, s4_2_5_d_eqn7]

s4_2_5_d1sent_list = map foldlSPCol [s4_2_5_d1sent_1, s4_2_5_d1sent_2,
  s4_2_5_d1sent_3, s4_2_5_d1sent_4, s4_2_5_d1sent_5, s4_2_5_d1sent_6, s4_2_5_d1sent_7]

s4_2_5_deriv2 :: [Contents]
s4_2_5_deriv2 = s4_2_5_d2startPara ++ (weave [s4_2_5_d2eqn_list,
  s4_2_5_d2sent_list]) ++ s4_2_5_d2endPara

s4_2_5_d2sent_list = map foldlSPCol [s4_2_5_d2sent_1, s4_2_5_d2sent_2, s4_2_5_d2sent_3]

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
s4_2_7_deriv = [s4_2_7_deriv_1, s4_2_7_deriv_2, s4_2_7_deriv_3,
  s4_2_7_deriv_4, s4_2_7_deriv_5]

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
s5_1_list = [req1, s5_1_1_Table, req2, s5_1_2_Eqn1, s5_1_2_Eqn2, s5_1_Reqs]

s5_1_1_Table = (Table [titleize symbol_, titleize unit_, titleize description]
  (mkTable
  [getS,
  --(\ch -> Sy (unit_symb ch)),
  unit'2Contents,
  phrase] (map qs inputConstraints))
  (titleize input_ +:+ titleize variable +:+ titleize requirement) False)

s5_1_Reqs :: Contents
s5_1_Reqs = enumSimple 3 (short requirement) $ map foldlSent reqList
-- Want to add req1 and req2 but they include a table and another enumeration
-- so not sure how to implement yet

reqList :: [[Sentence]]
reqList = [req3, req4, req5, req6, req7, req8, req9, req10, req11]

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

s6_start :: Contents -> Sentence
s6_start assump = makeRef assump +:+ S "-"

-- List structure same in all examples.

--add referencing to assumptions?

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
s7_instaModelRef = map (refFromType Theory swhsSymMap) swhsInModels

s7_funcReq = ["R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
  "R11"]
s7_funcReqRef = makeListRef s7_funcReq s5_1

s7_data = ["Data Constraints"]
s7_dataRef = [makeRef s4_2_6_table1] --FIXME: Reference section?

s7_assump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
  "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19"]
s7_assumpRef = makeListRef s7_assump s4_2_1

s7_theories = ["T1", "T2", "T3"]
s7_theoriesRef = map (refFromType Theory swhsSymMap) tModels

s7_genDefs = ["GD1", "GD2"]
s7_genDefRef = map (refFromType Theory swhsSymMap) swhsGenDefs

s7_dataDefs = ["DD1", "DD2", "DD3", "DD4"]
s7_dataDefRef = map (refFromType Data swhsSymMap) swhsDataDefs

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
  (s7_theoriesRef ++ s7_genDefRef ++ s7_dataDefRef ++ s7_instaModelRef ++ s7_likelyChgRef)

s7_columns_t3 :: [[String]]
s7_columns_t3 = [s7_t3_T1, s7_t3_T2, s7_t3_T3, s7_t3_GD1, s7_t3_GD2, s7_t3_DD1, 
  s7_t3_DD2, s7_t3_DD3, s7_t3_DD4, s7_t3_IM1, s7_t3_IM2, s7_t3_IM3, s7_t3_IM4,
  s7_t3_LC1, s7_t3_LC2, s7_t3_LC3, s7_t3_LC4, s7_t3_LC5, s7_t3_LC6]

s7_t3_T1, s7_t3_T2, s7_t3_T3, s7_t3_GD1, s7_t3_GD2, s7_t3_DD1, s7_t3_DD2, s7_t3_DD3,
  s7_t3_DD4, s7_t3_IM1, s7_t3_IM2, s7_t3_IM3, s7_t3_IM4, s7_t3_LC1, s7_t3_LC2,
  s7_t3_LC3, s7_t3_LC4, s7_t3_LC5, s7_t3_LC6 :: [String]

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
s8 :: Section
s8 = valsOfAuxConstantsF progName specParamValList

----------------------------
-- Section 9 : References --
----------------------------

s9 :: Section
s9 = SRS.reference [s9_refs] []

s9_refs :: Contents
s9_refs = mkRefsList 1 $ map foldlsC s9_refList

s9_refList :: [[Sentence]]
s9_refList = [ref1, ref2, ref3, ref4, ref5, ref6]

-- ============== --
-- Dead Knowledge --
-- ============== --


------------------------------
-- Section 2 : INTRODUCTION --
------------------------------

s2_intro :: ConceptChunk -> UnitalChunk -> ConceptChunk -> CI -> CI ->
  ConceptChunk -> UnitalChunk -> ConceptChunk -> Sentence
s2_intro es en sp pcmat pro te lh un = foldlSent [S "Due to increasing cost"
  `sC` S "diminishing availability, and negative environmental impact of",
  S "fossil fuels, there is a higher demand for renewable",
  plural es `sAnd` phrase en +:+.
  S "storage technology", phrase sp,
  sParen (short pcmat), S "use a renewable",
  plural es `sAnd` S "provide a novel way of",
  S "storing" +:+. phrase en,
  at_start sp, S "improve over the traditional",
  plural pro, S "because of their smaller size. The",
  S "smaller size is possible because of the ability of",
  short pcmat, S "to store", phrase te,
  S "as", phrase lh `sC`
  S "which allows higher", phrase te,
  S "storage capacity per", phrase un, S "weight"]

s2_kSent :: ConceptChunk -> ConceptChunk -> CI -> Sentence
s2_kSent sp pr pro = foldlSent_ [EmptyS +:+. phrase sp, S "The developed",
  phrase pr, S "will be referred to as", titleize pro,
  sParen (short pro)] -- SSP has same style sentence here

-- In Concepts.hs "swhs_pcm" gives "s for program name, and there is a
-- similar paragraph in each of the other eolar water heating systems
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

s2_2_end :: UnitalChunk -> ConceptChunk -> ConceptChunk -> CI -> ConceptChunk ->
  Sentence
s2_2_end t te wa pcmat sw = foldlSent_ [S "predict the",
  phrase t `sAnd` phrase te,
  S "histories for the", phrase wa `sAnd` S "the" +:+.
  short pcmat, S "This entire", phrase document,
  S "is written assuming that the substances inside the",
  phrase sw, S "are", phrase wa `sAnd` short pcmat]

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

s2_3_knowlegde :: ConceptChunk -> Sentence
s2_3_knowlegde htt = foldlSent_ [EmptyS +:+. phrase htt,
  S "A third or fourth year Mechanical Engineering course on this topic",
  S "is recommended"]

s2_3_understanding :: CI -> Sentence
s2_3_understanding diffeq = foldlSent_ [(phrase diffeq) `sC`
  S "as typically covered in first and second year Calculus courses"]

------------------------------------
-- 2.4 : Organization of Document --
------------------------------------

s2_4_intro :: Sentence
s2_4_intro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the template for an", short srs,
  S "for", phrase sciCompS, S "proposed by [citation] and",
  sSqBr (S "citation")]

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
sys_context_fig = Figure (foldlSent_
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
s4_intro_end sw = foldlSent_ [plural thModel `sC` plural genDefn `sC`
  plural dataDefn `sC` S "and finally the", plural inModel,
  sParen (short ode :+: S "s"), S "that", phrase model, S "the",
  phrase sw]

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
  sParen (getS hfc +:+ S "represents the" +:+. phrase hfc)]
--
physSyst3 :: CI -> ConceptChunk -> UnitalChunk -> [Sentence]
physSyst3 pcmat ta hfp = [short pcmat, S "suspended in" +:+. phrase ta,
  sParen (getS hfp +:+ S "represents the" +:+. phrase hfp)]

-- Structure of list would be same between examples but content is completely
-- different

fig_tank :: Contents
fig_tank = Figure (
  foldlSent_ [at_start sWHT `sC` S "with", phrase ht_flux_C, S "of",
  getS ht_flux_C `sAnd` phrase ht_flux_P, S "of", getS ht_flux_P])
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

assump1, assump2, assump3, assump4, assump5, assump6,
  assump7, assump8, assump9, assump10, assump11, assump12, assump13, assump14,
  assump15, assump16, assump17, assump18, assump19, assump20 :: Contents

assump1 = mkAssump "assump1"
  (S "The only form of" +:+ phrase energy +:+ S "that is" +:+
  S "relevant for this" +:+ phrase problem +:+ S "is" +:+. phrase CT.thermal_energy
  +:+ S "All other forms of" +:+ phrase energy `sC` S "such as" +:+
  phrase mech_energy `sC` S "are assumed to be negligible" +:+.
  sSqBr (swhsSymbMapTRef t1ConsThermE))
--
assump2 = mkAssump "assump2" 
  (S "All" +:+ phrase CT.heat_trans +:+ S "coefficients are constant over"
  +:+ phrase time +:+. sSqBr (acroGD 1))
--
assump3 = mkAssump "assump3"
  (S "The" +:+ phrase water +:+ S "in the" +:+ phrase tank +:+
  S "is fully mixed, so the" +:+ (phrase temp_W `isThe` S "same throughout the entire")
  +:+ phrase tank +:+. sSqBr (acroGD 2 `sC` swhsSymbMapDRef dd2HtFluxP))
--
assump4 = mkAssump "assump4"
  (S "The" +:+ phrase temp_PCM `isThe` S "same throughout the" +:+
  phrase pcm_vol +:+. sSqBr (acroGD 2 `sC` swhsSymbMapDRef dd2HtFluxP `sC`
  makeRef likeChg1))
--
assump5 = mkAssump "assump5"
  (S "The" +:+ phrase w_density `sAnd` phrase pcm_density +:+
  S "have no spatial variation; that is" `sC`
  S "they are each constant over their entire" +:+ phrase vol +:+.
  sSqBr (acroGD 2))
--
assump6 = mkAssump "assump6"
  (S "The" +:+ phrase htCap_W `sC` phrase htCap_S_P `sC` S "and" +:+
  phrase htCap_L_P +:+ S "have no spatial variation; that" +:+
  S "is, they are each constant over their entire" +:+
  phrase vol +:+. sSqBr (acroGD 2))
--
assump7 = mkAssump "assump7"
  ((CT.law_conv_cooling ^. defn) +:+
  S "applies between the" +:+ phrase coil `sAnd` S "the" +:+
  phrase water +:+. sSqBr (swhsSymbMapDRef dd1HtFluxC))
--
assump8 = mkAssump "assump8"
  (S "The" +:+ phrase temp_C +:+ S "is constant over" +:+
  phrase time +:+. sSqBr (swhsSymbMapDRef dd1HtFluxC `sC` acroLC 2))
--
assump9 = mkAssump "assump9"
  (S "The" +:+ phrase temp_C +:+ S "does not vary along its length" +:+.
  sSqBr (swhsSymbMapDRef dd1HtFluxC `sC` makeRef likeChg3))
--
assump10 = mkAssump "assump10"
  ((CT.law_conv_cooling ^. defn) +:+ S "applies between the" +:+
  phrase water `sAnd` S "the" +:+ short phsChgMtrl +:+.
  sSqBr (swhsSymbMapDRef dd2HtFluxP))
--
assump11 = mkAssump "assump11"
  (S "The" +:+ phrase model +:+
  S "only accounts for" +:+ (charging ^. defn) `sC` S "not" +:+.
  phrase discharging +:+ S "The" +:+ phrase temp_W `sAnd`
  phrase temp_PCM +:+ S "can only increase, or remain" +:+
  S "constant; they do not decrease. This implies that the" +:+
  phrase temp_init +:+ sParen (acroA 12) +:+ S "is less than (or equal)" +:+
  S "to the" +:+ phrase temp_C +:+. sSqBr ((acroIM 1) `sC` (makeRef likeChg4)))
--
assump12 = mkAssump "assump12"
  (phrase temp_init `ofThe'` phrase water
  `sAnd` S "the" +:+ short phsChgMtrl `isThe` S "same" +:+.
  sSqBr ((acroIM 1) `sC` (acroIM 2) `sC` (makeRef likeChg5)))
--
assump13 = mkAssump "assump13"
  (S "The" +:+ phrase simulation +:+ S "will start with the" +:+
  short phsChgMtrl +:+ S "in a" +:+ (solid ^. defn) +:+.
  sSqBr ((acroIM 2) `sC` (acroIM 4)))
--
assump14 = mkAssump "assump14"
  ((S "operating" +:+ phrase temp +:+ S "range" `ofThe'`
  phrase system) +:+ S "is such that the" +:+ phrase water +:+
  S "is always in" +:+. (liquid ^. defn) +:+ S "That is," +:+
  S "the" +:+ phrase temp +:+ S "will not drop below the" +:+
  phrase melt_pt +:+ S "of" +:+ phrase water `sC` S "or rise above its" +:+
  phrase boil_pt +:+. sSqBr ((acroIM 1) `sC` (acroIM 3)))
--
assump15 = mkAssump "assump15"
  (S "The" +:+ phrase tank +:+ S "is" +:+ phrase perfect_insul +:+
  S "so that there is no" +:+ phrase CT.heat +:+ S "loss from the" +:+
  phrase tank +:+. sSqBr ((acroIM 1) `sC` (makeRef likeChg6)))
--
assump16 = mkAssump "assump16"
  (S "No internal" +:+ phrase CT.heat +:+
  S "is generated by either the" +:+ phrase water +:+ S "or the" +:+
  short phsChgMtrl `semiCol` S "therefore, the" +:+ phrase vol_ht_gen +:+
  S "is zero" +:+. sSqBr ((acroIM 1) `sC` (acroIM 2)))
--
assump17 = mkAssump "assump17"
  ((phrase vol +:+ phrase change `ofThe'` short phsChgMtrl) +:+
  S "due to" +:+ phrase CT.melting +:+ S "is negligible" +:+. sSqBr (acroIM 2))
--
assump18 = mkAssump "assump18"
  (S "The" +:+ short phsChgMtrl +:+ S "is either in a" +:+ (liquid ^. defn) +:+
  S "or a" +:+ (solid ^. defn) +:+ S "but not a" +:+ (gaseous ^. defn) +:+.
  sSqBr ((acroIM 2) `sC` (acroIM 4)))
--
assump19 = mkAssump "assump19"
  (S "The pressure in the" +:+ phrase tank +:+
  S "is atmospheric, so the" +:+ phrase melt_pt `sAnd`
  phrase boil_pt +:+ S "are" +:+ S (show (0 :: Integer)) :+: Sy (unit_symb temp) `sAnd`
  S (show (100 :: Integer)) :+: Sy (unit_symb temp) `sC` S "respectively" +:+
  sSqBr ((acroIM 1) `sC` (acroIM 3)))
--
assump20 = mkAssump "assump20"
  (S "When considering the" +:+ phrase w_vol +:+ S "in the" +:+
  phrase tank `sC` (phrase vol `ofThe` phrase coil) +:+
  S "is assumed to be negligible" +:+. sSqBr (makeRef req2))

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

s4_2_3_deriv_3,
  s4_2_3_deriv_5, s4_2_3_deriv_6, s4_2_3_deriv_7,
  s4_2_3_deriv_8, s4_2_3_deriv_9, s4_2_3_deriv_10, s4_2_3_deriv_11 :: Contents

s4_2_3_deriv_1 :: ConceptChunk -> UnitalChunk -> Contents
s4_2_3_deriv_1 roc tem = foldlSPCol [S "Detailed derivation of simplified",
  phrase roc, S "of", phrase tem]

s4_2_3_deriv_2 :: RelationConcept -> UnitalChunk -> Contents
s4_2_3_deriv_2 t1ct vo = foldlSPCol [S "Integrating", swhsSymbMapTRef t1ct,
  S "over a", phrase vo, sParen (getS vo) `sC` S "we have"]

s4_2_3_deriv_3 = EqnBlock
  ((Neg (UnaryOp (Integral (Just (Low (C vol)), Nothing)
  ((C gradient) :. (C thFluxVect)) vol))) +
  UnaryOp (Integral (Just (Low (C vol)), Nothing)
  (C vol_ht_gen) vol) :=
  UnaryOp (Integral (Just (Low (C vol)), Nothing) ((C density)
  * (C heat_cap_spec) * Deriv Part (C temp) (C time)) vol))

s4_2_3_deriv_4 :: ConceptChunk -> ConVar -> UnitalChunk -> UnitalChunk ->
  ConVar -> ConceptChunk -> Contents
s4_2_3_deriv_4 gd su vo tfv unv un = foldlSPCol [S "Applying", titleize gd,
  S "to the first term over", (phrase su +:+ getS su `ofThe` phrase vo) `sC`
  S "with", getS tfv, S "as the", phrase tfv, S "for the",
  phrase surface `sAnd` getS unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

s4_2_3_deriv_5 = EqnBlock
  ((Neg (UnaryOp (Integral (Just (Low (C surface)),
  Nothing) ((C thFluxVect) :. (C uNormalVect)) surface))) +
  (UnaryOp (Integral (Just (Low (C vol)), Nothing) (C vol_ht_gen)
  vol)) := UnaryOp (Integral (Just (Low (C vol)), Nothing)
  ((C density) * (C heat_cap_spec) * Deriv Part (C temp) (C time)) vol))

s4_2_3_deriv_6 = foldlSPCol [S "We consider an arbitrary" +:+. phrase vol, S "The",
  phrase vol_ht_gen, S "is assumed constant. Then (1) can be written as"]

s4_2_3_deriv_7 = EqnBlock
  ((C ht_flux_in) * (C in_SA) - (C ht_flux_out) *
  (C out_SA) + (C vol_ht_gen) * (C vol) := UnaryOp (Integral
  (Just (Low (C vol)), Nothing) ((C density) * (C heat_cap_spec) *
  Deriv Part (C temp) (C time)) vol))

s4_2_3_deriv_8 = foldlSPCol [S "Where", getS ht_flux_in `sC` getS ht_flux_out `sC`
  getS in_SA `sC` S "and", getS out_SA, S "are explained in" +:+.
  acroGD 2, S "Assuming", getS density `sC` getS heat_cap_spec `sAnd`
  getS temp, S "are constant over the", phrase vol `sC`
  S "which is true in our case by", titleize' assumption,
  sParen (acroA 3) `sC` sParen (acroA 4) `sC`
  sParen (acroA 5) `sC` S "and", sParen (acroA 6) `sC` S "we have"]

s4_2_3_deriv_9 = EqnBlock
  ((C density) * (C heat_cap_spec) * (C vol) * Deriv Total (C temp)
  (C time) := (C ht_flux_in) * (C in_SA) - (C ht_flux_out) *
  (C out_SA) + (C vol_ht_gen) * (C vol))

s4_2_3_deriv_10 = foldlSPCol [S "Using the fact that", getS density :+: S "=" :+:
  getS mass :+: S "/" :+: getS vol `sC` S "(2) can be written as"]

s4_2_3_deriv_11 = EqnBlock
  ((C mass) * (C heat_cap_spec) * Deriv Total (C temp)
  (C time) := (C ht_flux_in) * (C in_SA) - (C ht_flux_out)
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

s4_2_5_subpar :: [Contents]
s4_2_5_subpar = [foldlSP [S "The goals",  acroGS 1, S "to",
  acroGS 4, S "are solved by", acroIM 1, S "to" +:+.
  acroIM 4, S "The", plural solution, S "for", acroIM 1 `sAnd` acroIM 2,
  S "are coupled since the", phrase solution,
  S "for", getS temp_W `sAnd` getS temp_PCM +:+.
  S "depend on one another", acroIM 3, S "can be solved once",
  acroIM 1, S "has been solved. The", phrase solution, S "of",
  acroIM 2 `sAnd` acroIM 4, S "are also coupled, since the",
  phrase temp_PCM `sAnd` phrase pcm_E, S "depend on the",
  phrase CT.phase_change]]

s4_2_5_d1startPara, s4_2_5_d2startPara, s4_2_5_d2endPara, s4_2_5_d1eqn_list,
  s4_2_5_d1sent_list, s4_2_5_d2eqn_list, s4_2_5_d2sent_list :: [Contents]

s4_2_5_d1startPara = [foldlSPCol [S "Derivation of the",
  phrase energy, S "balance on", phrase water]]
  
s4_2_5_d1sent_1, s4_2_5_d1sent_2, s4_2_5_d1sent_3, s4_2_5_d1sent_4,
  s4_2_5_d1sent_5, s4_2_5_d1sent_6, s4_2_5_d1sent_7 :: [Sentence]

s4_2_5_d1sent_1 = [S "To find the", phrase rOfChng, S "of", getS temp_W `sC`
  S "we look at the", phrase energy, S "balance on" +:+.
  phrase water, S "The", phrase vol, S "being considered" `isThe`
  phrase w_vol, getS w_vol `sC` S "which has", phrase w_mass +:+.
  (getS w_mass `sAnd` phrase htCap_W `sC` getS htCap_W),
  getS ht_flux_C, S "represents the", phrase ht_flux_C `sAnd`
  getS ht_flux_P, S "represents the", phrase ht_flux_P `sC`
  S "over", phrase coil_SA `sAnd` phrase pcm_SA, S "of",
  getS coil_SA `sAnd` getS pcm_SA `sC` S "respectively. No",
  phrase CT.heat_trans, S "occurs to", (S "outside" `ofThe`
  phrase tank) `sC` S "since it has been assumed to be",
  phrase perfect_insul +:+. sParen (acroA 15), S "Assuming no",
  phrase vol_ht_gen +:+. (sParen (acroA 16) `sC`
  (E $ C vol_ht_gen := Int 0)), S "Therefore, the", phrase equation, S "for",
  acroGD 2, S "can be written as"]

s4_2_5_d1sent_2 = [S "Using", swhsSymbMapDRef dd1HtFluxC `sAnd`
  swhsSymbMapDRef dd2HtFluxP, S "for", getS ht_flux_C `sAnd`
  getS ht_flux_P, S "respectively, this can be written as"]

s4_2_5_d1sent_3 = [S "Dividing (3) by", getS w_mass :+: getS htCap_W `sC` S "we obtain"]

s4_2_5_d1sent_4 = [S "Factoring the negative sign out of",
  S "second term" `ofThe` short rightSide,
  S "of", titleize equation,
  S "(4) and multiplying it by",
  getS coil_HTC :+: getS coil_SA :+: S "/" :+:
  getS coil_HTC :+: getS coil_SA, S "yields"]

s4_2_5_d1sent_5 = [S "Which simplifies to"]

s4_2_5_d1sent_6 = [S "Setting",
  (E $ C tau_W := (C w_mass :* C htCap_W) :/ (C coil_HTC :* C coil_SA)) `sAnd`
  (E $ C eta := (C pcm_HTC :* C pcm_SA) :/ (C coil_HTC :* C coil_SA)) `sC`
  titleize equation, S "(5) can be written as"]

s4_2_5_d1sent_7 = [S "Finally, factoring out", (E $ Int 1 :/ C tau_W) `sC` S "we are" +:+
  S "left with the governing", short ode, S "for", acroIM 1]

s4_2_5_d_eqn1, s4_2_5_d_eqn2, s4_2_5_d_eqn3, s4_2_5_d_eqn4, s4_2_5_d_eqn5,
  s4_2_5_d_eqn6, s4_2_5_d_eqn7 :: Expr

s4_2_5_d_eqn1 = ((C w_mass) * (C htCap_W) * Deriv Total (C temp_W) (C time) :=
  (C ht_flux_C) * (C coil_SA) - (C ht_flux_P) * (C pcm_SA))

s4_2_5_d_eqn2 = ((C w_mass) * (C htCap_W) * Deriv Total (C temp_W) (C time) :=
  (C coil_HTC) * (C coil_SA) * ((C temp_C) - (C temp_W)) -
  (C pcm_HTC) * (C pcm_SA) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d_eqn3 = (Deriv Total (C temp_W) (C time) := ((C coil_HTC) *
  (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) -
  (C temp_W)) - ((C pcm_mass) * (C pcm_SA)) / ((C w_mass) *
  (C htCap_W)) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d_eqn4 = (Deriv Total (C temp_W) (C time) := ((C coil_HTC) *
  (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) - (C temp_W)) +
  (((C coil_HTC) * (C coil_SA)) / ((C coil_HTC) * (C coil_SA))) *
  (((C pcm_HTC) * (C pcm_SA)) / ((C w_mass) * (C htCap_W))) *
  ((C temp_PCM) - (C temp_W)))

s4_2_5_d_eqn5 = (Deriv Total (C temp_W) (C time) := ((C coil_HTC) *
  (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) - (C temp_W)) +
  (((C pcm_HTC) * (C pcm_SA)) / ((C coil_HTC) * (C coil_SA))) *
  (((C coil_HTC) * (C coil_SA)) / ((C w_mass) * (C htCap_W))) *
  ((C temp_PCM) - (C temp_W)))

s4_2_5_d_eqn6 = (Deriv Total (C temp_W) (C time) := (1 / (C tau_W)) *
  ((C temp_C) - (C temp_W)) + ((C eta) / (C tau_W)) *
  ((C temp_PCM) - (C temp_W)))

s4_2_5_d_eqn7 = (Deriv Total (C temp_W) (C time) := (1 / (C tau_W)) *
  (((C temp_C) - (C temp_W)) + (C eta) * ((C temp_PCM) -
  (C temp_W))))

-- Should "energy balance" be a concept?
-- Add IM, GD, A, and EqnBlock references when available
-- Replace Derivs with regular derivative when available
-- Fractions in paragraph?

s4_2_5_d2sent_1, s4_2_5_d2sent_2, s4_2_5_d2sent_3 :: [Sentence]

s4_2_5_d2sent_1 = [S "Using", swhsSymbMapDRef dd2HtFluxP, S "for", getS ht_flux_P `sC`
  S "this", phrase equation, S "can be written as"]

s4_2_5_d2sent_2 = [S "Dividing by", getS pcm_mass :+: getS htCap_S_P, S "we obtain"]

s4_2_5_d2sent_3 = [S "Setting", getS tau_S_P :+: S "=" :+: getS pcm_mass :+: 
  getS htCap_S_P :+: S "/" :+: getS pcm_HTC :+: getS pcm_SA `sC`
  S "this can be written as"]

s4_2_5_d2eqn1, s4_2_5_d2eqn2, s4_2_5_d2eqn3, s4_2_5_d2eqn4 :: Expr

s4_2_5_d2eqn1 = ((C pcm_mass) * (C htCap_S_P) * Deriv Total (C temp_PCM)
  (C time) := (C ht_flux_P) * (C pcm_SA))

s4_2_5_d2eqn2 = ((C pcm_mass) * (C htCap_S_P) * Deriv Total (C temp_PCM)
  (C time) := (C pcm_HTC) * (C pcm_SA) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d2eqn3 = (Deriv Total (C temp_PCM) (C time) := ((C pcm_HTC) *
  (C pcm_SA)) / ((C pcm_mass) * (C htCap_S_P)) * ((C temp_W) - (C temp_PCM)))

s4_2_5_d2eqn4 = (Deriv Total (C temp_PCM) (C time) := (1 / (C tau_S_P)) *
  ((C temp_W) - (C temp_PCM)))

s4_2_5_d2startPara = map foldlSPCol [

  [S "Detailed derivation of the", phrase energy, S "balance on the",
  short phsChgMtrl, S "during", phrase sens_heat :+: S "ing phase"],

  [S "To find the", phrase rOfChng, S "of", getS temp_PCM `sC`
  S "we look at the", phrase energy, S "balance on the" +:+.
  short phsChgMtrl, S "The", phrase vol,
  S "being considered is the" +:+. (phrase pcm_vol `sC` getS pcm_vol),
  S "The derivation that follows is initially for the",
  phrase solid +:+. short phsChgMtrl, S "The", phrase pcm_mass, S "is",
  getS pcm_mass `sAnd` S "the", phrase htCap_S_P, S "is" +:+. getS htCap_S_P,
  S "The", phrase ht_flux_P, S "is", getS ht_flux_P, S "over",
  phrase pcm_SA +:+. getS pcm_SA, S "There is no" +:+. phrase ht_flux_out,
  S "Assuming no", phrase vol_ht_gen, sParen (acroA 16) `sC`
  getS vol_ht_gen :+: S "=0, the", phrase equation, S "for", acroGD 2,
  S "can be written as"]

  ]

s4_2_5_d2endPara = map foldlSP [

  [titleize equation,
  S "(6) applies for the", phrase solid +:+. short phsChgMtrl,
  S "In the case where all of the", short phsChgMtrl,
  S "is melted, the same derivation applies, except that",
  getS htCap_S_P, S "is replaced by", getS htCap_L_P `sC`
  S "and thus", getS tau_S_P, S "is replaced by" +:+.
  getS tau_L_P, S "Although a small change in", phrase surface,
  S "area would be expected with", phrase CT.melting `sC`
  S "this is not included, since",
  (phrase vol +:+ S "change" `ofThe` short phsChgMtrl),
  S "with", phrase CT.melting,
  S "is assumed to be negligible", sParen (acroA 17)],

  [S "In the case where", getS temp_PCM :+: S "=" :+:
  getS temp_melt_P `sAnd` S "not all of the", short phsChgMtrl,
  S "is melted, the", phrase temp_PCM +:+. S "does not change",
  S "Therefore, in this case d" :+: getS temp_PCM :+: S "/d" :+:
  getS time :+: S "=0"],

  [S "This derivation does not consider",
  (phrase CT.boiling `ofThe` short phsChgMtrl) `sC`
  S "as the", short phsChgMtrl,
  S "is assumed to either be in a", (solid ^. defn),
  S "or a", (liquid ^. defn), sParen (acroA 18)]

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

s4_2_6_T1footer :: Sentence
s4_2_6_T1footer = foldlSent_ $ map foldlSent [

  [sParen (S "*"), S "These", plural quantity, S "cannot be equal to zero" `sC`
  S "or there will be a divide by zero in the", phrase model],

  [sParen (S "+"), S "These", plural quantity, S "cannot be zero" `sC`
  S "or there would be freezing", sParen (acroA 13)],

  [sParen (S "#"), S "The", plural constraint, S "on the", phrase surArea,
  S "are calculated by considering the", phrase surArea, S "to", phrase vol +:+.
  S "ratio", S "The", phrase assumption, S "is that the lowest ratio is 1 and",
  S "the highest possible is", E (Int 2 :/ C htTransCoeff_min) `sC` S "where",
  E $ C htTransCoeff_min, S "is the thickness of a", Quote (S "sheet"), S "of" +:+.
  short phsChgMtrl, S "A thin sheet has the greatest", phrase surArea, S "to",
  phrase vol, S "ratio"],

  [sParen (S "**"), S "The", phrase constraint, S "on the maximum", phrase time,
  S "at the end of the simulation is the total number of secondsin one day"]
  
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

s4_2_7_deriv_1, s4_2_7_deriv_2, s4_2_7_deriv_3,
  s4_2_7_deriv_4, s4_2_7_deriv_5 :: Contents

s4_2_7_deriv_1 = foldlSPCol [S "A", phrase corSol, S "must exhibit the" +:+.
  phrase CT.law_cons_energy, S "This means that the", phrase w_E,
  S "should equal the difference between the total", phrase energy,
  phrase input_, S "from the", phrase coil `sAnd` S "the",
  phrase energy, phrase output_, S "to the" +:+. short phsChgMtrl,
  S "This can be shown as an", phrase equation, S "by taking",
  swhsSymbMapDRef dd1HtFluxC `sAnd` swhsSymbMapDRef dd2HtFluxP `sC`
  S "multiplying each by their respective", phrase surface,
  S "area of", phrase CT.heat_trans `sC` S "and integrating each",
  S "over the", phrase simulation, phrase time `sC` S "as follows"]

s4_2_7_deriv_2 = EqnBlock
  ((C w_E) := UnaryOp (Integral (Just (Low 0), Just (High (C time)))
  ((C coil_HTC) * (C coil_SA) * ((C temp_C) - FCall (C temp_W)
  [C time])) time) - UnaryOp (Integral (Just (Low 0), Just (High (C time)))
  ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) -
  (FCall (C temp_PCM) [C time]))) time))

s4_2_7_deriv_3 = foldlSP_ [S "In addition, the", phrase pcm_E, S "should equal the",
  phrase energy, phrase input_, S "to the", short phsChgMtrl,
  S "from the" +:+. phrase water, S "This can be expressed as"]

s4_2_7_deriv_4 = EqnBlock
  ((C pcm_E) := UnaryOp (Integral (Just (Low 0), Just (High (C time)))
  ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) - (FCall
  (C temp_PCM) [C time]))) time))

s4_2_7_deriv_5 = foldlSP [titleize' equation, S "(reference) and",
  S "(reference) can be used as", Quote (S "sanity") :+:
  S "checks to gain confidence in any", phrase solution,
  S "computed by" +:+. short progName, S "The relative",
  S "error between the results computed by", short progName `sAnd`
  S "the results calculated from the", short rightSide, S "of these",
  plural equation, S "should be less than 0.001%", sParen (acroR 9)]

-- Above section only occurs in this example (although maybe it SHOULD be in
-- the others).

-- Remember to insert references in above derivation when available

------------------------------
-- Section 5 : REQUIREMENTS --
------------------------------

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

req1, s5_1_1_Table, req2, s5_1_2_Eqn1, s5_1_2_Eqn2 :: Contents

req1 = Requirement (ReqChunk (nw $ npnc "req1" $
  nounPhraseSent (titleize input_ +:+ S "the following" +:+ plural quantity `sC`
  S "which define the" +:+ phrase tank +:+ plural parameter `sC` S "material" +:+
  plural property +:+ S "and initial" +: plural condition))
  []) EmptyS

req2 = Requirement (ReqChunk (nw $ npnc "req2" $
  nounPhraseSent (S "Use the" +:+ plural input_ +:+ S "in" +:+ makeRef req1 +:+
  S "to find the" +:+ phrase mass +:+ S "needed for" +:+ acroIM 1 +:+ S "to" +:+
  acroIM 4 `sC` S "as follows, where" +:+ getS w_vol `isThe` phrase w_vol +:+
  S "and" +: (getS tank_vol `isThe` phrase tank_vol)))
  []) EmptyS

s5_1_2_Eqn1 = EqnBlock ((C w_mass) := (C w_vol) * (C w_density) := ((C tank_vol) -
  (C pcm_vol)) * (C w_density) := (((C diam) / 2) * (C tank_length) -
  (C pcm_vol)) * (C w_density))

s5_1_2_Eqn2 = EqnBlock ((C pcm_mass) := (C pcm_vol) * (C pcm_density))

-- Want to add req1 and req2 but they include a table and another enumeration
-- so not sure how to implement yet

req3, req4, req5, req6, req7, req8, req9, req10, req11 :: [Sentence]

req3 = [S "Verify that the", plural input_, S "satisfy the required" +:+
  phrase physical, plural constraint, S "shown in", makeRef s7_table1]
--
req4 = [titleize output_, S "the", phrase input_, plural quantity `sAnd`
  S "derived", plural quantity +: S "in the following list",
  S "the", plural quantity, S "from", acroR 1 `sC` S "the",
  plural mass, S "from", acroR 2 `sC` getS tau_W,
  sParen (S "from" +:+ acroIM 1) `sC` getS eta,
  sParen (S "from" +:+ acroIM 1) `sC` getS tau_S_P,
  sParen (S "from" +:+ acroIM 2) `sAnd` getS tau_L_P,
  sParen (S "from" +:+ acroIM 2)]
--
req5 = [S "Calculate and", phrase output_, S "the", phrase temp_W,
  sParen(getS temp_W :+: sParen (getS time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 1)]
--
req6 = [S "Calculate and", phrase output_, S "the", phrase temp_PCM,
  sParen (getS temp_PCM :+: sParen (getS time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 2)]
--
req7 = [S "Calculate and", phrase output_, S "the", phrase w_E,
  sParen (getS w_E :+: sParen (getS time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 3)]
--
req8 = [S "Calculate and", phrase output_, S "the", phrase pcm_E,
  sParen (getS pcm_E :+: sParen (getS time)), S "over the",
  phrase simulation, phrase time, sParen (S "from" +:+ acroIM 4)]
--
req9 = [S "Verify that the", phrase energy, plural output_,
  sParen (getS w_E :+: sParen (getS time) `sAnd` getS pcm_E :+:
  sParen (getS time)), S "follow the", phrase CT.law_cons_energy `sC`
  S "as outlined in", makeRef s4_2_7 `sC` S "with relative error",
  S "no greater than 0.001%"]
--
req10 = [S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "begins to melt",
  getS t_init_melt, sParen (S "from" +:+ acroIM 2)]
--
req11 = [S "Calculate and", phrase output_, S "the", phrase time,
  S "at which the", short phsChgMtrl, S "stops", phrase CT.melting,
  getS t_final_melt, sParen (S "from" +:+ acroIM 2)]

-- List structure same between all examples

--How to include pi?
--How to add exponents?

---------------------------------------
-- 5.2 : Non-functional Requirements --
---------------------------------------

s5_2 :: Section
s5_2 = nonFuncReqF [performance] [correctness, verifiability,
  understandability, reusability, maintainability]
  (S "This problem is small in size and relatively simple")
  (S "Any reasonable implementation will be very quick and use minimal storage.")

-- The second sentence of the above paragraph is repeated in all examples (not
-- exactly, but the general idea is). The first sentence is not always
-- repeated, but it is always either stating that performance is a priority or
-- performance is not a priority. This is probably something that can be
-- abstracted out.

--------------------------------
-- Section 6 : LIKELY CHANGES --
--------------------------------

likeChg1, likeChg2, likeChg3, likeChg4, likeChg5, likeChg6 :: Contents

likeChg1 = LikelyChange (LCChunk (nw $ npnc "likeChg1" $
  nounPhraseSent (s6_start assump4 +:+ short phsChgMtrl +:+
  S "is actually a poor" +:+ phrase CT.thermal_conductor `sC` S "so" +:+
  S "the" +:+ phrase assumption +:+ S "of uniform" +:+ phrase temp_PCM +:+
  S "is not likely."))
  []) EmptyS
--
likeChg2 = LikelyChange (LCChunk (nw $ npnc "likeChg2" $
  nounPhraseSent (s6_start assump8 +:+ S "The" +:+ phrase temp_C +:+
  S "will change over" +:+ (S "course" `ofThe` S "day, depending") +:+
  S "on the" +:+ phrase energy +:+ S "received from the sun."))
  []) EmptyS
--
likeChg3 = LikelyChange (LCChunk (nw $ npnc "likeChg3" $
  nounPhraseSent (s6_start assump9 +:+ S "The" +:+ phrase temp_C +:+
  S "will actually change along its length as the" +:+ phrase water +:+
  S "within it cools."))
  []) EmptyS
--
likeChg4 = LikelyChange (LCChunk (nw $ npnc "likeChg4" $
  nounPhraseSent (s6_start assump11 +:+ S "The" +:+ phrase model +:+
  S "currently only accounts for charging of the tank. A more complete"
  +:+ phrase model +:+ S "would also account for discharging of the tank"))
  []) EmptyS
--
likeChg5 = LikelyChange (LCChunk (nw $ npnc "likeChg5" $
  nounPhraseSent (s6_start assump12 +:+ S "To add more" +:+
  S "flexibility to the" +:+ phrase simulation `sC`
  (phrase temp_init `ofThe` phrase water) `sAnd`
  S "the" +:+ short phsChgMtrl +:+ S "could be" +:+
  S "allowed to have different" +:+. plural value))
  []) EmptyS
--
likeChg6 = LikelyChange (LCChunk (nw $ npnc "likeChg6" $
  nounPhraseSent (s6_start assump15 +:+ S "Any real" +:+ phrase tank +:+
  S "cannot be perfectly insulated and will lose" +:+. phrase CT.heat))
  []) EmptyS
-- List structure same in all examples.

--add referencing to assumptions?

--------------------------------------------------
-- Section 7 : TRACEABILITY MATRICES AND GRAPHS --
--------------------------------------------------

s7_trailing_1, s7_trailing_2, s7_trailing_3 :: Sentence

s7_trailing_1 = foldlSent [plural thModel `sC` plural genDefn `sC` plural dataDefn `sC`
  S "and", plural inModel, S "with each other"]

s7_trailing_2 = foldlSent [plural inModel `sC` plural requirement `sC` S "and",
  plural datum, plural constraint, S "on each other"]

s7_trailing_3 = foldlSent_ [plural thModel `sC` plural genDefn `sC` plural dataDefn `sC`
  plural inModel `sC` S "and", plural likelyChg, S "on the",
  plural assumption]

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

  [foldlSent [plural thModel `sC` plural genDefn `sC` plural dataDefn
  `sC` plural inModel `sC` plural likelyChg `sC` S "and",
  plural assumption, S "on each other"],

  foldlSent_ [plural inModel `sC` plural requirement `sC`
  S "and", plural datumConstraint, S "on each other"]]

s7_fig1 :: Contents
s7_fig1 = Figure (showingCxnBw traceyGraph (titleize' item +:+
  S "of Different" +:+ titleize' section_)) "ATrace.png"

s7_fig2 :: Contents
s7_fig2 = Figure (showingCxnBw traceyGraph (titleize' requirement `sC`
  titleize' inModel `sC` S "and" +:+ titleize' datumConstraint)) "RTrace.png"

-------------------------------------------------
-- Section 8 :  Specification Parameter Values --
-------------------------------------------------
----------------------------
-- Section 9 : References --
----------------------------

ref1, ref2, ref3, ref4, ref5, ref6 :: [Sentence]

ref1 = [S "J. Frederick Bueche. Introduction to Physics for Scientists. McGraw Hill",
  S "United States", S "fourth edition edition", S "1986."]

ref2 = [S "F. P. Incropera", S "D. P. Dewitt", S "T. L. Bergman",
  S "and A. S. Lavine. Fundamentals of Heat and Mass Transfer. John Wiley" +:+
  S "and Sons", S "United States", S "sixth edition edition", S "2007."]

ref3 = [S "Nirmitha Koothoor. A document drive approach to certifying" +:+
  S "scientific computing software. Master's thesis", S "McMaster University",
  S "Hamilton", S "Ontario", S "Canada", S "2013."]

ref4 = [S "Marilyn Lightstone. Derivation of tank/pcm model. Personal Notes",
  S "2012."]

ref5 = [S "David L. Parnas and P.C. Clements. A rational design process:" +:+
  S "How and why to fake it. IEEE Transactions on Software Engineering",
  S "12" :+: Quote (S "2") :+: S ":251-257", S "February 1986."]

ref6 = [S "W. Spencer Smith and Lei Lai. A new requirements template for" +:+
  S "scientific computing. In J. Ralyt" :+: (F Acute 'e'), S "P. Agerfalk",
  S "and N. Kraiem", S "editors", S "Proceedings of the First" +:+
  S "International Workshop on Situational Requirements Engineering" +:+
  S "Processes - Methods, Techniques and Tools to Support" +:+
  S "Situation-Specific Requirements Engineering Processes, SREP'05",
  S "pages 107-121", S "Paris", S "France", S "2005. In conjunction with" +:+
  S "13th IEEE International Requirements Engineering Conference."]