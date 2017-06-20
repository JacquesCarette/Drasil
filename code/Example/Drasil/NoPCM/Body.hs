module Drasil.NoPCM.Body where

import Control.Lens ((^.))
import Prelude hiding (id)
import Drasil.NoPCM.TMods
import Drasil.NoPCM.Definitions
import Drasil.NoPCM.Unitals

import Language.Drasil

import Data.Drasil.SI_Units 
import Data.Drasil.Authors
import Data.Drasil.Utils(enumSimple, listConstS, getS, unwrap)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (ode, equation)
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.PhysicalProperties (liquid, vol)
import Data.Drasil.Concepts.Physics (energy, mech_energy, time)
import Data.Drasil.Concepts.Thermodynamics (heat, thermal_analysis, thermal_energy, 
  heat_trans, law_conv_cooling, melt_pt, boil_pt)
import Data.Drasil.Units.Thermodynamics
import Data.Drasil.Quantities.Thermodynamics (temp, ht_flux)
import Data.Drasil.Software.Products (sciCompS)

import Drasil.Sections.ReferenceMaterial (intro)
import qualified Drasil.SRS as SRS
import Drasil.DocumentLanguage
import Drasil.Sections.SpecificSystemDescription
import Drasil.Sections.Requirements
import Drasil.Sections.GeneralSystDesc

import Data.Drasil.SentenceStructures

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

s3, s3_1, s4, s4_1, s4_1_1, s4_1_2, s4_1_3, s4_2,
  s5, s5_1, s5_2, s6, s7 :: Section

s3_1_intro, sys_context_fig, s3_2_intro, s3_3_intro, s4_1_intro, s4_1_1_bullets, 
  s4_1_2_list, s4_1_3_intro, s4_1_3_list, fig_tank, s4_2_3_intro, s4_2_4_intro, 
  s4_2_5_intro, s4_2_6_table1, s4_2_6_table2:: Contents

-------------------------------
--Section 1 : REFERENCE MATERIAL
-------------------------------
  
mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro
  [TUnits, tsymb [TSPurpose, SymbConvention 
  [Lit (nw ht_trans), Doc' (nw sWHS)], SymbOrder], TAandA]) : 
  IntroSec (IntroProg s2s s2e 
  [IPurpose s2_1,
  IScope s2_2s s2_2e,
  IChar s2_3kn s2_3un EmptyS,
  IOrgSec s2_4s inModel (SRS.inModel SRS.missingP []) s2_4e]) :
  map Verbatim [s3, s4, s5, s6, s7]  
        
pcm_si :: SystemInformation
pcm_si = SI srs_swhs srs [thulasi] this_si pcmSymbols (pcmSymbols) 
  acronyms ([] :: [Block QDefinition]) -- Place Holder until Data Definitions can be created
  
pcm_srs :: Document
pcm_srs = mkDoc mkSRS pcm_si

nopcmSymbMap :: SymbolMap
nopcmSymbMap = symbolMap pcmSymbols



--------------------------
--Section 2 : INTRODUCTION
--------------------------

s2s, s2e, s2_1, s2_2s, s2_2e, s2_3kn, s2_3un, s2_4s, s2_4e :: Sentence

s2s = foldlSent [S "Due to increasing cost, diminishing",
  S "availability, and negative environmental impact of",
  S "fossil fuels, there is a higher demand for renewable",
  phrase energy, plural source, S "and",
  phrase energy +:+. S "storage technology", (plural sWHS),
  S "provide a novel way of storing", phrase energy]

s2e = foldlSent_ [EmptyS +:+. plural sWHS, S "The developed",
  phrase program, S "will be referred to as", titleize sWHS,
  sParen (short sWHS)]

-----------------------------------
--Section 2.1 : PURPOSE OF DOCUMENT
-----------------------------------

s2_1 = foldlSent [S "The main", phrase purpose, S "of this",
  phrase document, S "is to describe the modelling of" +:+.
  phrase sWHS, S "The", plural goal, S "and", plural thModel,
  S "used in the", short sWHS, S "code are provided, with an emphasis",
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

s2_2s = foldlSent_ [phrase thermal_analysis, S "of a single",
  phrase sWHT]

s2_2e = foldlSent_ [S "predict the",
  phrase temp, S "and", phrase thermal_energy,
  S "histories for the", phrase water]

--------------------------------------------------
--Section 2.3 : CHARACTERISTICS Of INTENDED READER
--------------------------------------------------

s2_3kn = foldlSent_ [phrase heat, S "transfer" +:+. phrase theory,
  S "A third or fourth year Mechanical Engineering course on this topic",
  S "is recommended"]

s2_3un = foldlSent_ [S "differential", plural equation `sC`
  S "as typically covered in first and second year Calculus courses"]

{-s2_3 = charIntRdrF knowledge understanding sWHS EmptyS
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

s2_4s = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the template for an", short srs,
  S "for", phrase sciCompS, S "proposed by [2] and",
  sSqBr (S "5")]

s2_4e = foldlSent_ [S "The", phrase inModel,
  sParen (makeRef (SRS.inModel SRS.missingP [])),
  S "to be solved is referred to as" +:+. acroIM "1",
  S "The", phrase inModel, S "provides the", 
  titleize ode, sParen (short ode), S "that model the" +:+. phrase sWHS,
  short sWHS, S "solves this", short ode]
                        

                        
----------------------------------------
--Section 3 : GENERAL SYSTEM DESCRIPTION
----------------------------------------

s3 = genSysF [s3_1] s3_2_intro [] []
--TODO: If/when system constraints recieves any content, add s3_3_intro
--to the first empty list

------------------------------
--Section 3.1 : SYSTEM CONTEXT
------------------------------

s3_1 = SRS.sysCont [s3_1_intro, sys_context_fig] []

s3_1_intro = foldlSP [makeRef sys_context_fig, S "shows the" +:+.
  phrase sysCont, S "A circle represents an external entity outside the",
  phrase software `sC` S "the", phrase user,
  S "in this case. A rectangle represents the", phrase softwareSys,
  S "itself" +:+. sParen (getAcc sWHS), S "Arrows are used to show the",
  plural datum, S "flow between the", phrase section_,
  S "and its", phrase environment]
            
sys_context_fig = Figure (makeRef sys_context_fig :+: S ":" +:+
  titleize sysCont) "SystemContextFigure.png"
  
------------------------------------
--Section 3.2 : USER CHARACTERISTICS
------------------------------------

s3_2_intro = foldlSP [S "The end", phrase user, S "of",
  short sWHS, S "should have an understanding of undergraduate",
  S "Level 1 Calculus and", titleize physics]

----------------------------------
--Section 3.3 : SYSTEM CONSTRAINTS
----------------------------------

s3_3_intro = Paragraph $ EmptyS

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

s4_1_intro = foldlSP [getAcc sWHS, S "is a computer",
  phrase program, S "developed to investigate",
  S "the heating of", phrase water, S "in a", phrase sWHT]

s4_1_1 = termDefnF EmptyS [s4_1_1_bullets]
  
s4_1_1_bullets = Enumeration $ (Bullet $ map (\x -> Flat $ 
  (at_start x) :+: S ":" +:+ (x ^. defn)) 
  [thermal_flux, heat_cap_spec])
  
s4_1_2 = physSystDesc (getAcc sWHS) fig_tank [s4_1_2_list, fig_tank]

fig_tank = Figure (at_start sWHT `sC` S "with" +:+ phrase ht_flux +:+
  S "from" +:+ phrase coil +:+ S "of" +:+ getS ht_flux_C)
  "TankWaterOnly.png"
  
s4_1_2_list = Enumeration $ Simple $ map (\(a, b) -> (a, Flat b)) [
  (acroPS "1", at_start tank +:+ S "containing" +:+ phrase water), 
  (acroPS "2", foldlSent [S "Heating", phrase coil, S "at bottom of", phrase tank] +:+
  sParen (foldlSent [getS ht_flux_C, S "represents the", phrase ht_flux_C,
  S "into the", phrase water]))]

s4_1_3 = SRS.goalStmt [s4_1_3_intro, s4_1_3_list] []

s4_1_3_intro = Paragraph $ foldl (+:+) (EmptyS) [S "Given the", phrase temp, 
  S "of the", phrase coil `sC` S "initial", phrase temp, S "of the",
  phrase water `sC` S "and material", plural property `sC`
  S "the", phrase goalStmt, S "is"]

s4_1_3_list = Enumeration $ Simple $ map (\(a, b) -> (a, Flat b)) [
  (acroGS "1", S "predict the" +:+ phrase temp_water +:+ S "over time")]

------------------------------------------------------
--Section 4.2 : SOLUTION CHARACTERISTICS SPECIFICATION
------------------------------------------------------
  
s4_2 = solChSpecF sWHS (s4_1, s6) True EmptyS ((makeRef s4_2_6_table1 +:+
  S "and" +:+ makeRef s4_2_6_table2 +:+ S "show"), EmptyS, False, EmptyS)
  ([s4_2_1_list], s4_2_2_TMods, [s4_2_3_intro], [s4_2_4_intro], [s4_2_5_intro],
  [s4_2_6_table1, s4_2_6_table2]) []
  
s4_2_1_list :: Contents
s4_2_1_list = enumSimple 1 (short assumption) $ map foldlSent s4_2_1_assump_list

s4_2_1_assump_list :: [[Sentence]]

assump1, assump2, assump3, assump4, assump5, assump6,
  assump7, assump8, assump9, assump10, assump11, assump12 :: [Sentence]

s4_2_1_assump_list = [assump1, assump2, assump3, assump4, assump5, assump6,
  assump7, assump8, assump9, assump10, assump11, assump12]
  
assump1 = [S "The only form of", phrase energy, S "that is",
  S "relevant for this", phrase problem, S "is" +:+.
  phrase thermal_energy, S "All other forms of", phrase energy
  `sC` S "such as", phrase mech_energy `sC` S "are assumed to be negligible"{-,
  sSqBr (swhsSymbMapTRef t1consThermE)-}]
  
assump2 = [S "All", phrase heat_trans, S "coefficients are constant over",
  phrase time, sSqBr (acroGD "1")]

assump3 = [S "The", phrase water, S "in the", phrase tank,
  S "is fully mixed, so the", phrase temp_water `isThe`
  S "same throughout the entire", phrase tank,
  sSqBr (acroGD "2")]

assump4 = [S "The", phrase water_dense, S "has no spatial variation; that is"
  `sC` S "it is constant over their entire", phrase vol, sSqBr (acroGD "2")]

assump5 = [S "The", phrase htCap_W, S "has no spatial variation; that",
  S "is, it is constant over its entire", phrase vol, sSqBr (acroGD "2")]

assump6 = [at_start law_conv_cooling,
  S "applies between the", phrase coil, S "and the",
  phrase water{-, sSqBr (swhsSymbMapDRef dd1HtFluxC)-}]

assump7 = [S "The", phrase temp_coil, S "is constant over",
  phrase time{-, sSqBr (swhsSymbMapDRef dd1HtFluxC `sC` acroLC "2")-}]

assump8 = [S "The", phrase temp_coil, S "does not vary along its length"{-,
  sSqBr (swhsSymbMapDRef dd1HtFluxC `sC` acroLC "3")-}]
  
-- TODO: Re-implement above when Data Definitions is created.

assump9 = [S "The", phrase model,
  S "only accounts for charging of the tank" `sC` 
  S "not discharging. The", phrase temp_water, S "can only increase, or remain",
  S "constant; it cannot decrease. This implies that the",
  phrase temp_init, sParen (acroA "12"), S "is less than (or equal)",
  S "to the", phrase temp_coil, sSqBr ((acroIM "1") `sC` (acroLC "4"))]

assump10 = [(S "operating" +:+ phrase temp +:+ S "range" `ofThe'`
  phrase system), S "is such that the", phrase water,
  S "is always in", phrase liquid, S "form. That is,",
  S "the", phrase temp, S "will not drop below the",
  phrase melt_pt, S "of", phrase water `sC` S "or rise above its",
  phrase boil_pt, sSqBr ((acroIM "1") `sC` (acroIM "3"))]
  
assump11 = [S "The", phrase tank, S "is perfectly insulated",
  S "so that there is no", phrase heat, S "loss from the",
  phrase tank, sSqBr ((acroIM "1") `sC` (acroLC "6"))]
  
assump12 = [S "No internal", phrase heat,
  S "is generated by the", phrase water `semiCol` S "therefore, the", 
  phrase ht_gen_vol, S "is zero", sSqBr ((acroIM "1") `sC` (acroIM "2"))]
  
s4_2_2_TMods :: [Contents]
s4_2_2_TMods = map (Definition nopcmSymbMap . Theory) [t1consThermE]

s4_2_3_intro = Paragraph $ EmptyS --TODO: Placeholder values until content can be added

s4_2_4_intro = Paragraph $ EmptyS --TODO: Placeholder values until content can be added

s4_2_5_intro = Paragraph $ EmptyS --TODO: Placeholder values until content can be added

s4_2_6_table1 = Table [S "Var", titleize' physicalConstraint, titleize software +:+ titleize' constraint, S "Typical" +:+
  titleize value, titleize uncertainty] (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2), (\x -> x!!3), (\x -> x!!4)] 
  s4_2_6_conList) (titleize table_ +: S "1" +:+ titleize input_ +:+
  titleize' variable) True
  
s4_2_6_conList :: [[Sentence]]
s4_2_6_conList = [con1, con2, con3]

con1, con2, con3 :: [Sentence]
con1 = [getS tank_L, E ((C tank_L) :> (Int 0)),
  E (((C tank_L) :<= (C tank_L)) :<= (C tank_L)),
  E (Dbl 1.5) +:+ (unwrap $ getUnit tank_L), S "10%"]

con2 = [getS tank_D, E ((C tank_D) :> (Int 0)),
  E (((C tank_D) :<= (C tank_D)) :<= (C tank_D)),
  E (Dbl 0.412) +:+ (unwrap $ getUnit tank_D), S "10%"]
  
con3 = [getS coil_SA, E ((C coil_SA) :> (Int 0)) +:+ sParen (S "*"),
  E ((C coil_SA) :<= (C coil_SA)),
  E (Dbl 0.12) +:+ (unwrap $ getUnit coil_SA), S "10%"]

s4_2_6_table2 = Table [S "Var", titleize' physicalConstraint, S "Typical" +:+
  titleize value] (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2)] $ map
  (listConstS) []) (titleize table_ +: S "2" +:+ titleize output_ +:+
  titleize' variable) True
    

    
--------------------------
--Section 5 : REQUIREMENTS
--------------------------

s5 = reqF [s5_1, s5_2]

s5_1 = SRS.funcReq [] [] --TODO: Placeholder values until content can be added

s5_2 = nonFuncReqF [performance] [correctness, verifiability,
  understandability, reusability, maintainability]
  (S "This problem is small in size and relatively simple")
  (S "Any reasonable implementation will be very quick and use minimal storage.")

        
        
----------------------------
--Section 6 : LIKELY CHANGES
----------------------------

s6 = SRS.likeChg [] [] --TODO: Add the rest of the section



------------
--REFERENCES
------------

s7 = SRS.reference [] []