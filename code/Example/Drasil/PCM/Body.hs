module Drasil.PCM.Body where

import Data.List (intersperse)
import Control.Lens ((^.))
import Prelude hiding (id)
import Drasil.PCM.Example

import Language.Drasil

import Data.Drasil.SI_Units 
import Data.Drasil.Authors
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Units.Thermodynamics
import Data.Drasil.Quantities.Thermodynamics (temp)

import Drasil.ReferenceMaterial (intro)
import Drasil.DocumentLanguage

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

s4,s4_1,s4_1_1,s4_1_2,s4_1_3,s4_2,s4_2_1,s4_2_2 :: Section

s4_intro,
  s4_1_intro,s4_1_1_intro,s4_1_1_bullets,s4_1_2_intro,s4_1_2_list,s4_1_3_intro,
  s4_1_3_list,s4_2_intro,s4_2_1_intro,s4_2_2_intro, fig_tank:: Contents

mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention [Lit (nw ht_trans), Doc (nw sWHS)], SymbOrder], TAandA]) : 
        map Verbatim [s4]  
        
pcm_si :: SystemInformation
pcm_si = SI srs_swhs srs [thulasi] this_si pcmSymbols (pcmSymbols) acronyms
  
pcm_srs :: Document
pcm_srs = mkDoc mkSRS pcm_si

s4 = section ((titleize $ specificsystemdescription ^. term)) [s4_intro] [s4_1, s4_2]

s4_intro = Paragraph $
           S "This" +:+ (phrase $ section_ ^. term) +:+ S "first presents the" +:+
            (phrase $ problemDescription ^. term) :+: S ", which gives a high-level view of the" +:+
            (phrase $ problem ^. term) +:+ S "to be solved. This is followed by the" +:+
            (phrase $ solution ^. term) +:+ (phrase $ characteristicsSpecification ^. term) :+:
           S ", which presents the" +:+ (plural assumption) `sC` (plural $ theory ^. term) :+: S "," +:+
            (plural $ definition ^. term) +:+ S "and finally the instance" +:+
            (phrase $ model ^. term) +:+ S "(":+: (getAcc ode) :+: S ") that models the" +:+ (phrase $ sWHT ^. term) :+: S "." --FIXME: We need something to handle the use of nouns as verbs

s4_1 = section ((titleize $ problemDescription ^. term)) [s4_1_intro] [s4_1_1, s4_1_2, s4_1_3]

s4_1_intro = Paragraph $
            (getAcc sWHS) +:+ S "is a computer" +:+ (phrase $ program ^. term) +:+ S "developed to investigate" +:+
           S "the heating of" +:+ (phrase $ water ^. term) +:+ S "in a" +:+ (phrase $ sWHT ^. term) :+: S "."

s4_1_1 = section ((titleize $ terminology ^. term) +:+ S "and" +:+ (titleize' $ definition ^. term)) [s4_1_1_intro, s4_1_1_bullets] []
  
s4_1_1_intro = Paragraph $
           S "This subsection provides a list of terms that" +:+
           S "are used in subsequent" +:+ (plural $ section_ ^. term) +:+ S "and their meaning, with the" +:+
            (phrase $ purpose ^. term) +:+ S "of reducing ambiguity and making it easier to correctly" +:+
           S "understand the" +:+ (plural $ requirement ^. term) :+: S ":"
  
s4_1_1_bullets = Enumeration $ (Bullet $ map (\c -> Flat $ 
  (sMap capitalize (phrase $ c ^. term)) :+: S ": " :+: (c ^. defn)) 
  [thermal_flux, heat_cap_spec])
  
s4_1_2 = section (titleize physSyst) [s4_1_2_intro, s4_1_2_list, fig_tank] []

s4_1_2_intro = Paragraph $
           S "The physical" +:+ (phrase $ system ^. term) +:+ S "of" +:+ (getAcc sWHS) :+:
           S ", as shown in" +:+ (makeRef fig_tank) :+: S ", includes the following elements:"

fig_tank = Figure ((at_start $ sWHT ^. term) :+: S ", with" +:+ (phrase $ ht_flux ^. term) +:+ S "from" +:+ (phrase $ coil ^. term) +:+ S "of" +:+
            P (ht_flux_C ^. symbol)) "TankWaterOnly.png"
  
s4_1_2_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
            (S "PS1", (at_start $ tank ^. term) +:+ S "containing" +:+ (phrase $ water ^. term)), 
            (S "PS2", S "Heating" +:+ (phrase $ coil ^. term) +:+ S "at bottom of" +:+ (phrase $ tank ^. term) :+: S ". (" :+:
           P (ht_flux_C ^. symbol) +:+ S "represents the" +:+ (phrase $ ht_flux_C ^. term) +:+
           S "into the" +:+ (phrase $ water ^. term) :+: S ".)")]

s4_1_3 = section (titleize' goalStmt) [s4_1_3_intro, s4_1_3_list] []

s4_1_3_intro = Paragraph $
           S "Given the" +:+ (phrase $ temp ^. term) +:+ S "of the" +:+ (phrase $ coil ^. term) :+: S ", initial" +:+
            (phrase $ temp ^. term) +:+ S "of the" +:+ (phrase $ water ^. term) :+: S "," +:+
           S "and material properties, the goal statement is"

s4_1_3_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
            (S "GS1", S "predict the " :+: (phrase $ temp_water ^. term) +:+ S "over time")]

s4_2 = section ((titleize $ solution ^. term) +:+ (titleize $ characteristicsSpecification ^. term)) 
  [s4_2_intro] [s4_2_1, s4_2_2]

s4_2_intro = Paragraph $
           S "The" +:+ (phrase $ inModel ^. term) +:+
           S "(" :+: getAcc ode :+: S ") that governs" +:+ (getAcc sWHS) +:+ S "is presented in" +:+ --TODO: Subsec reference
           S ". The" +:+ (phrase $ information ^. term) +:+ S "to understand the meaning of the" +:+
            (phrase $ inModel ^. term) +:+ 
           S "and its derivation is also" +:+ S "presented, so that the" +:+ 
            (phrase $ inModel ^. term) +:+ S "can be verified."
  
s4_2_1 = section (titleize' assumption) [s4_2_1_intro] []

s4_2_1_intro = Paragraph $
           S "This" +:+ (phrase $ section_ ^. term) +:+
           S "simplifies the original" +:+ (phrase $ problem ^. term) +:+
           S "and helps in developing the theoretical" +:+ (phrase $ model ^. term) +:+
           S "by filling in the missing" +:+ (phrase $ information ^. term) +:+
           S "for the physical" +:+ (phrase $ system ^. term) :+: S ". The numbers given in the" +:+
           S "square brackets refer to the" +:+ foldr1 (:+:) (intersperse (S ", ") 
            (map (\ch -> (phrase $ ch ^. term) +:+ S "[" :+:
            (getAcc ch) :+: S "]") [thModel, genDefn, dataDefn, inModel])) `sC` 
           S "or" +:+ phrase likelyChg +:+ S "[" :+: (getAcc likelyChg) :+:
           S "], in which the respective" +:+ (phrase assumption) +:+. S "is used"
--TODO: Simple List

s4_2_2 = section (titleize' thModel) [s4_2_2_intro, s4_2_2_TMods] []

s4_2_2_intro = Paragraph $
           S "This" +:+ (phrase $ section_ ^. term) +:+ S "focuses on the general equations" +:+
           S "and laws that" +:+ (getAcc sWHS) +:+ S "is based on." 
-- :+: foldr1 (:+:) (map makeRef s4_2_2_TMods) :+: S" " :+: makeRef s1
  
s4_2_2_TMods :: Contents
s4_2_2_TMods = Definition $ Theory t1consThermE
