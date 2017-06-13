module Drasil.NoPCM.Body where

import Control.Lens ((^.))
import Prelude hiding (id)
import Drasil.NoPCM.Example

import Language.Drasil

import Data.Drasil.SI_Units 
import Data.Drasil.Authors
import Data.Drasil.Utils(listConstS)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (ode, equation)
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Thermodynamics (heat)
import Data.Drasil.Units.Thermodynamics
import Data.Drasil.Quantities.Thermodynamics (temp, ht_flux)

import Drasil.ReferenceMaterial (intro)
import qualified Drasil.SRS as SRS
import Drasil.DocumentLanguage
import Drasil.SpecificSystemDescription
import Drasil.Introduction
import Drasil.Requirements
import Drasil.GeneralSystDesc

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

s2, s2_1, s2_2, s2_3, s2_4, s3, s3_1, s4, s4_1, s4_1_1, s4_1_2, s4_1_3, s4_2,
  s5, s5_1, s5_2, s6, s7 :: Section

s3_1_intro, sys_context_fig, s4_1_intro, s4_1_1_bullets, s4_1_2_list,
  s4_1_3_intro, s4_1_3_list, fig_tank, s4_2_3_intro, s4_2_4_intro,
  s4_2_5_intro, s4_2_6_table1, s4_2_6_table2:: Contents

-------------------------------
--Section 1 : REFERENCE MATERIAL
-------------------------------
  
mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention
  [Lit (nw ht_trans), Doc' (nw sWHS)], SymbOrder], TAandA]) : 
  map Verbatim [s2, s3, s4, s5, s6, s7]  
        
pcm_si :: SystemInformation
pcm_si = SI srs_swhs srs [thulasi] this_si pcmSymbols (pcmSymbols) acronyms
  
pcm_srs :: Document
pcm_srs = mkDoc mkSRS pcm_si

nopcmSymbMap :: SymbolMap
nopcmSymbMap = symbolMap pcmSymbols


--------------------------
--Section 2 : INTRODUCTION
--------------------------

s2 = introductionSection EmptyS EmptyS [s2_1, s2_2, s2_3, s2_4]
--TODO: Placeholder values until content can be added

s2_1 = purposeOfDoc EmptyS
--TODO: Placeholder values until content can be added

s2_2 = scopeOfRequirements EmptyS sWHS EmptyS
--TODO: Placeholder values until content can be added

s2_3 = charIntRdrF knowledge understanding sWHS EmptyS
  (SRS.userChar SRS.missingP [])
  -- FIXME: referencing this for now until we figure out how to reference
  -- auto-generated section (section 3.2)
  where knowledge = phrase heat +:+ S "transfer" +:+. phrase theory +:+
          S "A third or fourth year Mechanical Engineering" +:+
          S "course on this topic is recommended"

        understanding = S "differential" +:+ plural equation `sC`
          S "as typically covered in first and second year" +:+
          S "Calculus courses"
                        
s2_4 = orgSec EmptyS inModel (SRS.inModel SRS.missingP []) EmptyS


                        
----------------------------------------
--Section 3 : GENERAL SYSTEM DESCRIPTION
----------------------------------------

s3 = genSysF [s3_1] (Paragraph EmptyS) [] []
--TODO: fill in the empty (last three) parameters

s3_1 = SRS.sysCont [s3_1_intro, sys_context_fig] []

s3_1_intro = Paragraph $ makeRef sys_context_fig +:+ S "shows the" +:+.
  phrase sysCont +:+ S "A circle represents an external entity outside the" +:+
  phrase software `sC` S "the" +:+ phrase user +:+
  S "in this case. A rectangle represents the" +:+ phrase softwareSys +:+
  S "itself" +:+. sParen (getAcc sWHS) +:+ S "Arrows are used to show the" +:+
  plural datum +:+ S "flow between the" +:+ phrase section_ +:+
  S "and its" +:+. phrase environment
            
sys_context_fig = Figure (makeRef sys_context_fig :+: S ":" +:+
  titleize sysCont) "SystemContextFigure.png"


-----------------------------------------
--Section 4 : SPECIFIC SYSTEM DESCRIPTION
-----------------------------------------

--TODO: finish filling in the subsections
s4 = specSysDesF words_ [s4_1, s4_2]
  where words_ = (plural definition +:+ S "and finally the" +:+
          phrase inModel +:+ sParen (getAcc ode) +:+
          S "that" +:+ plural model +:+ S "the" +:+ phrase sWHT)

s4_1 = SRS.probDesc [s4_1_intro] [s4_1_1, s4_1_2, s4_1_3]

s4_1_intro = Paragraph $ getAcc sWHS +:+ S "is a computer" +:+
  phrase program +:+ S "developed to investigate" +:+
  S "the heating of" +:+ phrase water +:+ S "in a" +:+. phrase sWHT

s4_1_1 = termDefnF Nothing [s4_1_1_bullets]
  
s4_1_1_bullets = Enumeration $ (Bullet $ map (\x -> Flat $ 
          (at_start x) :+: S ":" +:+ (x ^. defn)) 
          [thermal_flux, heat_cap_spec])
  
s4_1_2 = physSystDesc (getAcc sWHS) fig_tank [s4_1_2_list, fig_tank]

fig_tank = Figure (at_start sWHT `sC` S "with" +:+ phrase ht_flux +:+
  S "from" +:+ phrase coil +:+ S "of" +:+ P (ht_flux_C ^. symbol))
  "TankWaterOnly.png"
  
s4_1_2_list = Enumeration $ Simple $ map (\(a, b) -> (a, Flat b)) [
  (acroPS "1", at_start tank +:+ S "containing" +:+ phrase water), 
  (acroPS "2", S "Heating" +:+ phrase coil +:+ S "at bottom of" +:+. phrase tank +:+
  sParen (P (ht_flux_C ^. symbol) +:+ S "represents the" +:+ phrase ht_flux_C +:+
  S "into the" +:+. phrase water))]

s4_1_3 = SRS.goalStmt [s4_1_3_intro, s4_1_3_list] []

s4_1_3_intro = Paragraph $ S "Given the" +:+ phrase temp +:+ S "of the" +:+
  phrase coil `sC` S "initial" +:+ phrase temp +:+ S "of the" +:+
  phrase water `sC` S "and material" +:+ plural property `sC`
  S "the" +:+ phrase goalStmt +:+ S "is"

s4_1_3_list = Enumeration $ Simple $ map (\(a, b) -> (a, Flat b)) [
  (acroGS "1", S "predict the" +:+ phrase temp_water +:+ S "over time")]

s4_2 = solChSpecF sWHS (s4_1, s6) True EmptyS ((makeRef s4_2_6_table1 +:+
  S "and" +:+ makeRef s4_2_6_table2 +:+ S "show"), EmptyS, False, EmptyS)
  ([], s4_2_2_TMods, [s4_2_3_intro], [s4_2_4_intro], [s4_2_5_intro],
  [s4_2_6_table1, s4_2_6_table2]) []
  
s4_2_2_TMods :: [Contents]
s4_2_2_TMods = map (Definition nopcmSymbMap . Theory) [t1consThermE]

s4_2_3_intro = Paragraph $ EmptyS --TODO: Placeholder values until content can be added

s4_2_4_intro = Paragraph $ EmptyS --TODO: Placeholder values until content can be added

s4_2_5_intro = Paragraph $ EmptyS --TODO: Placeholder values until content can be added

s4_2_6_table1 = Table [S "Var", titleize' physicalConstraint, S "Typical" +:+
  titleize value] (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2)] $ map
  (listConstS) []) (titleize table_ +: S "1" +:+ phrase input_ +:+
  titleize' variable) True

s4_2_6_table2 = Table [S "Var", titleize' physicalConstraint, S "Typical" +:+
  titleize value] (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2)] $ map
  (listConstS) []) (titleize table_ +: S "2" +:+ phrase output_ +:+
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