module Drasil.NoPCM.Body where

import Control.Lens ((^.))
import Prelude hiding (id)
import Drasil.NoPCM.Definitions (ht_trans, srs_swhs)
--import Drasil.NoPCM.Unitals hiding (coil_SA, htCap_W, temp_init, time_final)

import Drasil.SWHS.Body (s2_3_knowlegde, s2_3_understanding, s2_4_intro,
  s3, physSyst1, physSyst2, s4_2_4_intro_end, s4_2_5_d1startPara, assump1,
  assump2, assump7, s5_2, s6_start, s7_trailing, ref2, ref3, ref4, ref5, ref6)
import Drasil.SWHS.Concepts (progName, water, gauss_div, sWHT, tank, coil,
  transient, perfect_insul)
import Drasil.SWHS.Unitals (w_vol, tank_length, tank_vol, tau_W, temp_W, w_mass,
  diam, coil_SA, temp_C, w_density, htCap_W, temp_init, time_final,
  in_SA, out_SA, vol_ht_gen, thFluxVect, ht_flux_in, ht_flux_out, tau, htCap_L,
  htTransCoeff, temp_env, diam, tank_length, w_vol, ht_flux_C, coil_HTC, temp_diff,
  w_E, tank_length_min, tank_length_max, htTransCoeff_min, w_density_min,
  w_density_max, htCap_W_min, htCap_W_max, coil_HTC_min, coil_HTC_max,
  time_final_max)
import Drasil.SWHS.DataDefs(swhsSymbMapDRef, swhsSymbMapTRef, dd1HtFluxC,
  s4_2_4_DD1, swhsSymbMapT)
import Drasil.SWHS.TMods (s4_2_2_T1, t1ConsThermE)
import Drasil.SWHS.GenDefs (swhsGenDefs)
import Drasil.SWHS.IMods (eBalanceOnWtr, heatEInWtr)

import Language.Drasil

import Data.Drasil.SI_Units
import Data.Drasil.Authors
import Data.Drasil.Utils (enumSimple, getS, mkRefsList, makeListRef, refFromType,
  itemRefToSent, makeTMatrix, itemRefToSent, mkEnumAbbrevList, weave)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (ode, unit_, rOfChng, equation)
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.PhysicalProperties (liquid)
import Data.Drasil.Concepts.Physics (energy)
import Data.Drasil.Concepts.Thermodynamics
import qualified Data.Drasil.Quantities.Thermodynamics as QT
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.PhysicalProperties (vol, mass, density)
import Data.Drasil.Quantities.Math (uNormalVect, surface, gradient)

import Drasil.Sections.ReferenceMaterial (intro)
import qualified Drasil.SRS as SRS
import Drasil.DocumentLanguage
import Drasil.Sections.SpecificSystemDescription
import Drasil.Sections.Requirements
import Drasil.Sections.TraceabilityMandGs
import Drasil.Sections.AuxiliaryConstants

import Data.Drasil.SentenceStructures

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

acronyms :: [CI]
acronyms = [assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, ode,
            physSyst, requirement, srs, progName, thModel]

pcmSymbols :: [CQSWrapper]
pcmSymbols = (map cqs pcmUnits) ++ (map cqs pcmConstraints)

pcmUnits :: [UCWrapper]
pcmUnits = map ucw [density, tau, in_SA, out_SA,
  htCap_L, QT.ht_flux, ht_flux_in, ht_flux_out, vol_ht_gen,
  htTransCoeff, mass, tank_vol, QT.temp, QT.heat_cap_spec,
  temp_diff, temp_env, thFluxVect, time, ht_flux_C,
  vol, w_mass, w_vol]

pcmConstraints :: [UncertQ]
pcmConstraints =  [coil_SA, htCap_W, coil_HTC, temp_init,
  time_final, tank_length, temp_C, w_density, diam, temp_W]

s4, s4_1, s4_1_1, s4_1_2, s4_1_3, s4_2, {-s3, s3_1, -}
  s5, s5_1, s6, s7, s8, s9 :: Section -- s5_2,

s4_1_intro, s4_1_1_bullets, {-s3_1_intro, sys_context_fig, s3_2_intro, s3_3_intro, -}
  s4_1_2_list, s4_1_3_intro, s4_1_3_list, fig_tank,
  s4_2_6_table1, s4_2_6_table2, s6_list, s9_refs :: Contents

--------------------------------
--Section 1 : REFERENCE MATERIAL
--------------------------------
  
mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro
  [TUnits, tsymb [TSPurpose, SymbConvention
  [Lit (nw ht_trans), Doc' (nw progName)], SymbOrder], TAandA]) :
  IntroSec (IntroProg s2_start s2_end
  [IPurpose s2_1,
  IScope s2_2_start s2_2_end,
  IChar s2_3_knowlegde s2_3_understanding EmptyS,
  IOrgSec s2_4_intro inModel (SRS.inModel SRS.missingP []) s2_4_end]) :
  map Verbatim [s3, s4, s5, s6, s7, s8, s9]

pcm_si :: SystemInformation
pcm_si = SI srs_swhs srs [thulasi] this_si pcmSymbols (pcmSymbols)
  acronyms ([dd1HtFluxC] :: [QDefinition]) (map qs pcmConstraints) 
  ([] :: [QSWrapper]) ([] :: [Block QDefinition])
  ([] :: [ConstrainedChunk])-- Place Holder until Data Definitions can be created

pcm_srs :: Document
pcm_srs = mkDoc mkSRS pcm_si

nopcmSymbMap :: SymbolMap
nopcmSymbMap = symbolMap pcmSymbols



--------------------------
--Section 2 : INTRODUCTION
--------------------------

s2_start, s2_end, s2_1, s2_2_start, s2_2_end, {-s2_3kn, s2_3un, s2_4s, -}s2_4_end :: Sentence

s2_start = foldlSent [S "Due to increasing cost, diminishing",
  S "availability, and negative environmental impact of",
  S "fossil fuels, there is a higher demand for renewable",
  phrase energy, plural source, S "and",
  phrase energy +:+. S "storage technology", at_start' progName,
  S "provide a novel way of storing", phrase energy]

s2_end = foldlSent_ [EmptyS +:+. plural progName, S "The developed",
  phrase program, S "will be referred to as", titleize progName,
  sParen (short progName)]

-----------------------------------
--Section 2.1 : PURPOSE OF DOCUMENT
-----------------------------------

s2_1 = foldlSent [S "The main", phrase purpose, S "of this",
  phrase document, S "is to describe the modelling of" +:+.
  phrase progName, S "The", plural goal, S "and", plural thModel,
  S "used in the", short progName, S "code are provided, with an emphasis",
  S "on explicitly identifying", plural assumption, S "and unambiguous" +:+.
  plural definition, S "This", phrase document,
  S "is intended to be used as a", phrase reference,
  S "to provide ad hoc access to all", phrase information,
  S "necessary to understand and verify the" +:+. phrase model, S "The",
  short srs, S "is abstract because the", plural content, S "say what",
  phrase problem, S "is being solved, but do not say how to solve it"]

-------------------------------------
--Section 2.2 : SCOPE OF REQUIREMENTS
-------------------------------------

s2_2_start = foldlSent_ [phrase thermal_analysis, S "of a single",
  phrase sWHT]

s2_2_end = foldlSent_ [S "predict the",
  phrase temp, S "and", phrase thermal_energy,
  S "histories for the", phrase water]

--------------------------------------------------
--Section 2.3 : CHARACTERISTICS Of INTENDED READER
--------------------------------------------------

{-s2_3kn = foldlSent_ [phrase heat, S "transfer" +:+. phrase theory,
  S "A third or fourth year Mechanical Engineering course on this topic",
  S "is recommended"]

s2_3un = foldlSent_ [S "differential", plural equation `sC`
  S "as typically covered in first and second year Calculus courses"]

s2_3 = charIntRdrF knowledge understanding sWHS EmptyS
  (SRS.userChar SRS.missingP [])
  -- FIXME: referencing this for now until we figure out how to reference
  -- auto-generated section (section 3.2)
  where knowledge = phrase heat +:+ S "transfer" +:+. phrase theory +:+
          S "A third or fourth year Mechanical Engineering" +:+
          S "course on this topic is recommended"

        understanding = S "differential" +:+ plural equation `sC`
          S "as typically covered in first and second year" +:+
          S "Calculus courses"-}
          
---------------------------------------
--Section 2.4: ORGANIZATION OF DOCUMENT
---------------------------------------

{-s2_4s = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the template for an", short srs,
  S "for", phrase sciCompS, S "proposed by [2] and",
  sSqBr (S "5")]-}

s2_4_end = foldlSent_ [S "The", phrase inModel,
  sParen (makeRef (SRS.inModel SRS.missingP [])),
  S "to be solved is referred to as" +:+. acroIM 1,
  S "The", phrase inModel, S "provides the",
  titleize ode, sParen (short ode), S "that model the" +:+. phrase progName,
  short progName, S "solves this", short ode]



----------------------------------------
--Section 3 : GENERAL SYSTEM DESCRIPTION
----------------------------------------

--ALL OF THIS SECTION IS NOW PULLED FROM SWHS

--s3 = genSysF [s3_1] s3_2_intro [] []
--TODO: If/when system constraints recieves any content, add s3_3_intro
--to the first empty list

------------------------------
--Section 3.1 : SYSTEM CONTEXT
------------------------------

{-s3_1 = SRS.sysCont [s3_1_intro, sys_context_fig] []

s3_1_intro = foldlSP [makeRef sys_context_fig, S "shows the" +:+.
  phrase sysCont, S "A circle represents an external entity outside the",
  phrase software `sC` S "the", phrase user,
  S "in this case. A rectangle represents the", phrase softwareSys,
  S "itself" +:+. sParen (getAcc progName), S "Arrows are used to show the",
  plural datum, S "flow between the", phrase section_,
  S "and its", phrase environment]
            
sys_context_fig = Figure (makeRef sys_context_fig :+: S ":" +:+
  titleize sysCont) "SystemContextFigure.png"-}
  
------------------------------------
--Section 3.2 : USER CHARACTERISTICS
------------------------------------

{-s3_2_intro = foldlSP [S "The end", phrase user, S "of",
  short progName, S "should have an understanding of undergraduate",
  S "Level 1 Calculus and", titleize physics]-}

----------------------------------
--Section 3.3 : SYSTEM CONSTRAINTS
----------------------------------

--s3_3_intro = Paragraph $ EmptyS

--TODO: Placeholder value until content can be added



-----------------------------------------
--Section 4 : SPECIFIC SYSTEM DESCRIPTION
-----------------------------------------

--TODO: finish filling in the subsections
s4 = specSysDesF words_ [s4_1, s4_2]
  where words_ = (plural definition +:+ S "and finally the" +:+
                  phrase inModel +:+ sParen (getAcc ode) +:+
                  S "that" +:+ plural model +:+ S "the" +:+ phrase sWHT)

-----------------------------------
--Section 4.1 : PROBLEM DESCRIPTION
-----------------------------------

s4_1 = SRS.probDesc [s4_1_intro] [s4_1_1, s4_1_2, s4_1_3]

s4_1_intro = foldlSP [getAcc progName, S "is a computer",
  phrase program, S "developed to investigate",
  S "the heating of", phrase water, S "in a", phrase sWHT]

s4_1_1 = termDefnF EmptyS [s4_1_1_bullets]

s4_1_1_bullets = Enumeration $ (Bullet $ map (\x -> Flat $ 
  (at_start x) :+: S ":" +:+ (x ^. defn))
  [ht_flux, heat_cap_spec, thermal_conduction, transient])
  
s4_1_2 = physSystDesc (getAcc progName) fig_tank [s4_1_2_list, fig_tank]

fig_tank = Figure (at_start sWHT `sC` S "with" +:+ phrase ht_flux +:+
  S "from" +:+ phrase coil +:+ S "of" +:+ getS ht_flux_C)
  "TankWaterOnly.png"

s4_1_2_list = enumSimple 1 (short physSyst) $ map foldlSent_ [physSyst1, physSyst2]

s4_1_3 = SRS.goalStmt [s4_1_3_intro, s4_1_3_list] []

s4_1_3_intro = foldlSPCol [S "Given the", phrase temp,
  S "of the", phrase coil `sC` S "initial", phrase temp_W 
  `sC` S "and material", plural property `sC`
  S "the", phrase goalStmt, S "are"]

s4_1_3_list = Enumeration $ Simple $ map (\(a, b) -> (a, Flat b)) [
  (acroGS 1, S "predict the" +:+ phrase temp_W +:+ S "over time"),
  (acroGS 2, S "predict the" +:+ phrase w_E +:+ S "over time")]

------------------------------------------------------
--Section 4.2 : SOLUTION CHARACTERISTICS SPECIFICATION
------------------------------------------------------
  
s4_2 = solChSpecF progName (s4_1, s6) s4_2_4_intro_end (mid,
  dataConstraintUncertainty, end) ([assump3, assump4], s4_2_2_T1, s4_2_3_paragraph,
  s4_2_4_DD1, [swhsSymbMapT eBalanceOnWtr] ++ s4_2_5_d1startPara ++
  s4_2_5_paragraph ++ [swhsSymbMapT heatEInWtr], [s4_2_6_table1, s4_2_6_table2]) []

  where mid = foldlSent [S "The", phrase column, S "for", phrase software,
          plural constraint, S "restricts the range of",
          plural input_, S "to reasonable", plural value]

        end = foldlSent [S "The", phrase uncertainty, phrase column,
          S "provides an estimate of the confidence with which the physical",
          S "quantities can be measured. This", phrase information,
          S "would be part of the input if one were performing an",
          phrase uncertainty, S "quantification exercise"]

-- s4_2_1_list :: Contents
-- s4_2_1_list = enumSimple 1 (short assumption) $ map foldlSent s4_2_1_assump_list

-- s4_2_1_assump_list :: [[Sentence]]

-- assump3, assump4, assump5, {-assump6,-}{-assump1, assump2, -}
  -- assump7_npcm, assump8, assump9, assump10, assump11, assump12 :: [Sentence]

-- s4_2_1_assump_list = [assump1, assump2, assump3, assump4, assump5, assump7,
  -- assump7_npcm, assump8, assump9, assump10, assump11, assump12]
  
-- {-assump1 = [S "The only form of", phrase energy, S "that is",
  -- S "relevant for this", phrase problem, S "is" +:+.
  -- phrase thermal_energy, S "All other forms of", phrase energy
  -- `sC` S "such as", phrase mech_energy `sC` S "are assumed to be negligible",
  -- sSqBr (swhsSymbMapTRef t1consThermE)]-}

-- {-assump2 = [S "All", phrase heat_trans, S "coefficients are constant over",
  -- phrase time, sSqBr (acroGD "1")]-}
assump3, assump4 :: Contents
assump3 = Assumption $ nw $ npnc "assump3" $ nounPhraseSP "The water in the tank is fully mixed, so the water temperature is the same throughout the entire tank."
assump4 = Assumption $ nw $ npnc "assump4" $ nounPhraseSP "The water density has no spatial variation; that is it is constant over the entire volume."

-- assump3 = [S "The", phrase water, S "in the", phrase tank,
  -- S "is fully mixed, so the", phrase temp_W `isThe`
  -- S "same throughout the entire", phrase tank,
  -- sSqBr (acroGD 2)]

-- assump4 = [S "The", phrase w_density, S "has no spatial variation; that is"
  -- `sC` S "it is constant over their entire", phrase vol, sSqBr (acroGD 2)]

-- assump5 = [S "The", phrase htCap_W, S "has no spatial variation; that",
  -- S "is, it is constant over its entire", phrase vol, sSqBr (acroGD 2)]

-- {-assump6 = [at_start law_conv_cooling,
  -- S "applies between the", phrase coil, S "and the",
  -- phrase water, sSqBr (swhsSymbMapDRef dd1HtFluxC)]-}

-- assump7_npcm = [S "The", phrase temp_C, S "is constant over",
  -- phrase time, sSqBr (swhsSymbMapDRef dd1HtFluxC `sC` acroLC 1)]

-- assump8 = [S "The", phrase temp_C, S "does not vary along its length",
  -- sSqBr (swhsSymbMapDRef dd1HtFluxC `sC` acroLC 2)]

---- TODO: Re-implement above when Data Definitions is created.

-- assump9 = [S "The", phrase model,
  -- S "only accounts for charging of the tank" `sC`
  -- S "not discharging. The", phrase temp_W, S "can only increase, or remain",
  -- S "constant; it cannot decrease. This implies that the",
  -- phrase temp_init, sParen (acroA 12), S "is less than (or equal)",
  -- S "to the", phrase temp_C, sSqBr ((acroIM 1) `sC` (acroLC 3))]

-- assump10 = [(S "operating" +:+ phrase temp +:+ S "range" `ofThe'`
  -- phrase system), S "is such that the", phrase water,
  -- S "is always in", phrase liquid, S "form. That is,",
  -- S "the", phrase temp, S "will not drop below the",
  -- phrase melt_pt, S "of", phrase water `sC` S "or rise above its",
  -- phrase boil_pt, sSqBr (acroIM 1)]

-- assump11 = [S "The", phrase tank, S "is perfectly insulated",
  -- S "so that there is no", phrase heat, S "loss from the",
  -- phrase tank, sSqBr ((acroIM 1) `sC` (acroLC 4))]

-- assump12 = [S "No internal", phrase heat,
  -- S "is generated by the", phrase water `semiCol` S "therefore, the",
  -- phrase vol_ht_gen, S "is zero", sSqBr (acroIM 1)]

{-s4_2_2_TMods :: [Contents]
s4_2_2_TMods = map (Definition nopcmSymbMap . Theory) [t1consThermE]-}

s4_2_3_equation, s4_2_3_description, s4_2_3_paragraph, s4_2_5_equation,
  s4_2_5_description, s4_2_5_paragraph :: [Contents]

s4_2_3_paragraph = (map swhsSymbMapT swhsGenDefs) ++ [foldlSPCol [S "Detailed derivation of simplified",
  phrase rOfChng, S "of", phrase temp]] ++ (weave [s4_2_3_description, s4_2_3_equation])

s4_2_3_description = map foldlSPCol [

  [S "Integrating", swhsSymbMapTRef t1ConsThermE,
  S "over a", phrase vol, sParen (getS vol) `sC` S "we have"],

  [S "Applying", titleize gauss_div, S "to the first term over",
  (phrase surface +:+ getS surface `ofThe` phrase vol) `sC` S "with",
  getS thFluxVect, S "as the", phrase thFluxVect, S "for the",
  phrase surface, S "and", getS uNormalVect, S "as a", phrase unit_,
  S "outward", phrase uNormalVect, S "for a", phrase surface],

  [S "We consider an arbitrary" +:+. phrase vol, S "The",
  phrase vol_ht_gen, S "is assumed constant. Then (1) can be written as"],

  [S "Where", getS ht_flux_in `sC` getS ht_flux_out `sC`
  getS in_SA `sC` S "and", getS out_SA, S "are explained in" +:+.
  acroGD 2, S "Assuming", getS density `sC` getS QT.heat_cap_spec,
  S "and", getS QT.temp, S "are constant over the", phrase vol `sC`
  S "which is true in our case by", titleize' assumption,
  sParen (acroA 3) `sC` sParen (acroA 4) `sC`
  S "and", sParen (acroA 5) `sC` S "we have"],

  [S "Using the fact that", getS density :+: S "=" :+:
  getS mass :+: S "/" :+: getS vol `sC` S "(2) can be written as"]
  ]

s4_2_3_eq1, s4_2_3_eq2, s4_2_3_eq3, s4_2_3_eq4, s4_2_3_eq5,
  s4_2_5_eq1, s4_2_5_eq2, s4_2_5_eq3, s4_2_5_eq4 :: Expr

s4_2_3_eq1 = (Neg (UnaryOp (Integral (Just (Low (C vol)), Nothing)
  ((C gradient) :. (C thFluxVect)) vol))) + UnaryOp
  (Integral (Just (Low (C vol)), Nothing) (C vol_ht_gen) vol) :=
  UnaryOp (Integral (Just (Low (C vol)), Nothing) ((C density)
  * (C QT.heat_cap_spec) * Deriv Part (C QT.temp) (C time)) vol)

s4_2_3_eq2 = (Neg (UnaryOp (Integral (Just (Low (C surface)),
  Nothing) ((C thFluxVect) :. (C uNormalVect)) surface))) +
  (UnaryOp (Integral (Just (Low (C vol)), Nothing) (C vol_ht_gen)
  vol)) := UnaryOp (Integral (Just (Low (C vol)), Nothing)
  ((C density) * (C QT.heat_cap_spec) * Deriv Part (C QT.temp) (C time)) vol)

s4_2_3_eq3 = (C ht_flux_in) :* (C in_SA) :- (C ht_flux_out) :*
  (C out_SA) :+ (C vol_ht_gen) :* (C vol) := UnaryOp (Integral
  (Just (Low (C vol)), Nothing) ((C density) :* (C QT.heat_cap_spec) :*
  Deriv Part (C QT.temp) (C time)) vol)

s4_2_3_eq4 = (C density) :* (C QT.heat_cap_spec) :* (C vol) :* Deriv Total (C QT.temp)
  (C time) := (C ht_flux_in) :* (C in_SA) :- (C ht_flux_out) :*
  (C out_SA) :+ (C vol_ht_gen) :* (C vol)

s4_2_3_eq5 = (C mass) :* (C QT.heat_cap_spec) :* Deriv Total (C QT.temp)
  (C time) := (C ht_flux_in) :* (C in_SA) :- (C ht_flux_out)
  :* (C out_SA) :+ (C vol_ht_gen) :* (C vol)

s4_2_3_equation = map EqnBlock [s4_2_3_eq1, s4_2_3_eq2, s4_2_3_eq3, s4_2_3_eq4, s4_2_3_eq5]

s4_2_5_paragraph = weave [s4_2_5_description, s4_2_5_equation]

s4_2_5_description = map foldlSPCol [

  [S "To find the", phrase rOfChng, S "of", getS temp_W `sC`
  S "we look at the", phrase energy, S "balance on" +:+.
  phrase water, S "The", phrase vol, S "being considered" `isThe`
  phrase w_vol, getS w_vol `sC` S "which has", phrase mass,
  getS w_mass, S "and" +:+. (phrase htCap_W `sC` getS htCap_W),
  at_start heat_trans, S "occurs in the water from the coil as", (getS ht_flux_C
  `sC` S "over area") +:+. getS coil_SA, S "No",
  phrase heat_trans, S "occurs to", (S "outside" `ofThe`
  phrase tank) `sC` S "since it has been assumed to be",
  phrase perfect_insul +:+. sParen (acroA 11), S "Assuming no",
  phrase vol_ht_gen +:+. (sParen (acroA 12) `sC`
  E (C vol_ht_gen := Int 0)), S "Therefore, the", phrase equation, S "for",
  acroGD 2, S "can be written as"],

  [S "Using", swhsSymbMapDRef dd1HtFluxC `sC`
  S "this can be written as"],

  [S "Dividing (3) by", getS w_mass :+: getS htCap_W `sC` S "we obtain"],

  [S "Setting", (getS tau_W :+: S "=" :+: getS w_mass :+:
  getS htCap_W :+: S "/" :+: getS coil_HTC :+: getS coil_SA)
  `sC` titleize equation, S "(4) can be written in its final form as"]
  ]

s4_2_5_eq1 = (C w_mass) :* (C htCap_W) :* Deriv Total (C temp_W) (C time) :=
  (C ht_flux_C) :* (C coil_SA)
 
s4_2_5_eq2 = (C w_mass) :* (C htCap_W) :* Deriv Total (C temp_W) (C time) :=
  (C coil_HTC) :* (C coil_SA) :* ((C temp_C) :- (C temp_W))

s4_2_5_eq3 = Deriv Total (C temp_W) (C time) := ((C coil_HTC) :*
  (C coil_SA)) :/ ((C w_mass) :* (C htCap_W)) :* ((C temp_C) :-
  (C temp_W))

s4_2_5_eq4 = Deriv Total (C temp_W) (C time) := (1 / (C tau_W)) :*
  ((C temp_C) :- (C temp_W))

s4_2_5_equation = map EqnBlock [s4_2_5_eq1, s4_2_5_eq2, s4_2_5_eq3, s4_2_5_eq4]

s4_2_6_table1 = inDataConstTbl s4_2_6_conListIn
-- s4_2_6_table1 = Table [S "Var", titleize' physicalConstraint, titleize software +:+
  -- titleize' constraint, S "Typical" +:+ titleize value, titleize uncertainty]
  -- (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2), (\x -> x!!3), (\x -> x!!4)]
  -- s4_2_6_conListIn) (titleize input_ +:+ titleize' variable) True

s4_2_6_conListIn :: [UncertQ]
s4_2_6_conListIn = [tank_length, diam, coil_SA, temp_C, w_density, htCap_W,
  coil_HTC, temp_init, time_final]

s4_2_6_table2 = outDataConstTbl s4_2_6_conListOut
-- s4_2_6_table2 = Table [S "Var", titleize' physicalConstraint]
  -- (mkTable [(\x -> x!!0), (\x -> x!!1)] s4_2_6_conListOut)
  -- (titleize output_ +:+ titleize' variable) True

s4_2_6_conListOut :: [UncertQ]
s4_2_6_conListOut = [temp_W, w_E]

inputVar :: [QSWrapper]
inputVar = map qs s4_2_6_conListIn 




--------------------------
--Section 5 : REQUIREMENTS
--------------------------

s5 = reqF [s5_1, s5_2]

---------------------------------------
--Section 5.1 : FUNCTIONAL REQUIREMENTS
---------------------------------------

s5_1 = SRS.funcReq s5_1_list [] --TODO: Placeholder values until content can be added

s5_1_list_words, s5_1_list, s5_1_list_items :: [Contents]
s5_1_list = weave [s5_1_list_words, s5_1_list_items]

s5_1_list_items = [

  Table [titleize symbol_, titleize unit_, titleize description]
  (mkTable
  [getS,
  unit'2Contents,
  phrase] inputVar)
  (titleize input_ +:+ titleize variable +:+ titleize requirement) False,

  EqnBlock ((C w_mass) := (C w_vol) * (C w_density) :=
  (((C diam) / 2) * (C tank_length) * (C w_density)))
  ]

s5_1_list_words = map (\x -> Enumeration $ Simple [x])
  $ mkEnumAbbrevList 1 (short requirement) $ map foldlSent_ [

  [titleize input_, S "the following", plural quantity `sC`
  S "which define the", phrase tank, S "parameters, material",
  plural property, S "and initial" +: plural condition],

  [S "Use the", plural input_, S "in", acroR 1, S "to find the",
  phrase mass, S "needed for", acroIM 1, S "to", acroIM 4 `sC`
  S "as follows, where", getS w_vol `isThe` phrase w_vol,
  S "and" +: (getS tank_vol `isThe` phrase tank_vol)],

  [S "Verify that the", plural input_, S "satisfy the required",
  phrase physicalConstraint, S "shown in" +:+. makeRef s4_2_6_table1],

  [titleize' output_, S "and", plural input_, plural quantity, S "and derived",
  plural quantity, S "in the following list: the", plural quantity, S "from",
  (acroR 1) `sC` S "the", phrase mass, S "from", acroR 2, S "and", getS tau_W +:+.
  sParen(S "from" +:+ acroIM 1)],

  [S "Calculate and output the", phrase temp, S "of the", phrase water,
  sParen (getS temp_W :+: sParen (getS time)), S "over the", phrase simulation +:+.
  phrase time],

  [S "Calculate and", phrase output_, S "the", phrase w_E,
  sParen (getS w_E :+: sParen (getS time)), S "over the",
  phrase simulation, phrase time +:+. sParen (S "from" +:+ acroIM 3)]
  ]

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

{-s5_2 = nonFuncReqF [performance] [correctness, verifiability,
  understandability, reusability, maintainability]
  (S "This problem is small in size and relatively simple")
  (S "Any reasonable implementation will be very quick and use minimal storage.")-}



----------------------------
--Section 6 : LIKELY CHANGES
----------------------------

s6 = SRS.likeChg [s6_list] []

s6_list = enumSimple 1 (short likelyChg) $ map foldlSent s6_likeChg_list

s6_likeChg_list :: [[Sentence]]
likeChg1, likeChg2, likeChg3, likeChg4 :: [Sentence]

s6_likeChg_list = [likeChg1, likeChg2, likeChg3, likeChg4]

likeChg1 = [s6_start 7, S "The", phrase temp_C,
  S "will change over", S "course" `ofThe` S "day, depending",
  S "on the", phrase energy, S "received from the sun"]

likeChg2 = [s6_start 8, S "The", phrase temp_C,
  S "will actually change along its length as the",
  phrase water, S "within it cools"]

likeChg3 = [s6_start 9, S "The", phrase model +:+.
  S "currently only accounts for charging of the tank",
  S "A more complete", phrase model, S "would also",
  S "account for discharging of the tank"]

likeChg4 = [s6_start 11, S "Any real", phrase tank,
  S "cannot be perfectly insulated and will lose",
  phrase heat]



----------------------------------------------
--Section 7:  TRACEABILITY MATRICES AND GRAPHS
----------------------------------------------

s7 = traceMGF s7_refList s7_trailing
  ([s7_table1, s7_table2, s7_table3] ++
  (s7_intro2) ++ [s7_fig1, s7_fig2]) []

s7_refList :: [Contents]
s7_refList = [s7_table1, s7_table2, s7_table3]

s7_instaModel, s7_data, s7_funcReq, s7_likelyChg, s7_dataDefs, s7_genDefs,
  s7_assump, s7_theories :: [String]
s7_dataRef, s7_funcReqRef, s7_instaModelRef, s7_assumpRef, s7_theoriesRef,
  s7_dataDefRef, s7_likelyChgRef, s7_genDefRef :: [Sentence]

s7_instaModel = ["IM1", "IM2"]
s7_instaModelRef = map (refFromType Theory nopcmSymbMap) [eBalanceOnWtr, heatEInWtr]

s7_funcReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
s7_funcReqRef = makeListRef s7_funcReq s5_1

s7_data = ["Data Constraints"]
s7_dataRef = [makeRef s4_2_6_table1] --FIXME: Reference section?

s7_assump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
  "A11", "A12", "A13", "A14"]
s7_assumpRef = makeListRef s7_assump (SRS.inModel SRS.missingP [])

s7_theories = ["T1"]
s7_theoriesRef = map (refFromType Theory nopcmSymbMap) [t1ConsThermE]

s7_genDefs = ["GD1", "GD2"]
s7_genDefRef = map (refFromType Theory nopcmSymbMap) swhsGenDefs

s7_dataDefs = ["DD1"]
s7_dataDefRef = map (refFromType Data nopcmSymbMap) [dd1HtFluxC]

s7_likelyChg = ["LC1", "LC2", "LC3", "LC4"]
s7_likelyChgRef = makeListRef s7_likelyChg s6

{-Traceability Matrix 1-}

s7_row_t1 :: [String]
s7_row_t1 = s7_theories ++ s7_genDefs ++ s7_dataDefs ++ s7_instaModel

s7_row_header_t1 :: [Sentence]
s7_row_header_t1 = zipWith itemRefToSent s7_row_t1
  (s7_theoriesRef ++ s7_genDefRef ++ s7_dataDefRef ++ s7_instaModelRef)

s7_columns_t1 :: [[String]]
s7_columns_t1 = [s7_t1_T1, s7_t1_GD1, s7_t1_GD2, s7_t1_DD1, s7_t1_IM1, s7_t1_IM2]

s7_t1_T1, s7_t1_GD1, s7_t1_GD2, s7_t1_DD1, s7_t1_IM1, s7_t1_IM2 :: [String]

--list of each item that "X" item requires for traceability matrix
s7_t1_T1 = []
s7_t1_GD1 = []
s7_t1_GD2 = ["T1"]
s7_t1_DD1 = ["GD1"]
s7_t1_IM1 = ["GD2", "DD1"]
s7_t1_IM2 = []

s7_table1 :: Contents
s7_table1 = Table (EmptyS:s7_row_header_t1)
  (makeTMatrix (s7_row_header_t1) (s7_columns_t1) (s7_row_t1))
  (showingCxnBw traceyMatrix
  (titleize' requirement `sAnd` titleize' inModel)) True

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
s7_columns_t2 = [s7_t2_IM1, s7_t2_IM2, s7_t2_R1,
  s7_t2_R2, s7_t2_R3, s7_t2_R4, s7_t2_R5, s7_t2_R6]

s7_t2_IM1, s7_t2_IM2, s7_t2_R1, s7_t2_R2,
  s7_t2_R3, s7_t2_R4, s7_t2_R5, s7_t2_R6 :: [String]

--list of each item that "X" item requires for traceability matrix
s7_t2_IM1 = []
s7_t2_IM2 = []
s7_t2_R1 = []
s7_t2_R2 = ["R1","IM1"]
s7_t2_R3 = ["Data Constraints"]
s7_t2_R4 = ["R1", "R2", "IM1"]
s7_t2_R5 = ["IM1"]
s7_t2_R6 = ["IM2"]

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
s7_columns_t3 = [s7_t3_T1, s7_t3_GD1, s7_t3_GD2, s7_t3_DD1,
  s7_t3_IM1, s7_t3_IM2, s7_t3_LC1, s7_t3_LC2, s7_t3_LC3, s7_t3_LC4]

s7_t3_T1, s7_t3_GD1, s7_t3_GD2, s7_t3_DD1,
  s7_t3_IM1, s7_t3_IM2, s7_t3_LC1, s7_t3_LC2, s7_t3_LC3, s7_t3_LC4 :: [String]

s7_t3_T1  = ["A1"]
s7_t3_GD1 = ["A2"]
s7_t3_GD2 = ["A3", "A4", "A5"]
s7_t3_DD1 = ["A6", "A7", "A8"]
s7_t3_IM1 = ["A9", "A10"]
s7_t3_IM2 = ["A10"]
s7_t3_LC1 = ["A7"]
s7_t3_LC2 = ["A8"]
s7_t3_LC3 = ["A9"]
s7_t3_LC4 = ["A11"]

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

  -- Using the SWHS graphs as place holders until ones can be generated for PCM 



------------------------------------------
--Section 8: SPECIFICATION PARAMETER VALUE
------------------------------------------

specParamValList :: [QDefinition]
specParamValList = [tank_length_min, tank_length_max, htTransCoeff_min,
  w_density_min, w_density_max, htCap_W_min, htCap_W_max, coil_HTC_min,
  coil_HTC_max, time_final_max]

s8 = valsOfAuxConstantsF progName specParamValList



------------
--REFERENCES
------------

s9 = SRS.reference [s9_refs] []

s9_refs = mkRefsList 1 $ map foldlsC s9_refList

s9_refList :: [[Sentence]]
s9_refList = [ref2, ref3, ref4, ref5, ref6]