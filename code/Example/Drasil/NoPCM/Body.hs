module Drasil.NoPCM.Body where

import Data.List (intersperse)
import Control.Lens ((^.))
import Prelude hiding (id)
import Drasil.NoPCM.Example

import Language.Drasil

import Data.Drasil.SI_Units 
import Data.Drasil.Authors
import Data.Drasil.Utils(mkConstraintList)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (ode, equation, number)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Concepts.Thermodynamics (heat)
import Data.Drasil.Units.Thermodynamics
import Data.Drasil.Quantities.Thermodynamics (temp, ht_flux)

import Drasil.ReferenceMaterial (intro)
import qualified Drasil.SRS as SRS
import Drasil.DocumentLanguage
import Drasil.OrganizationOfSRS

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

s2,s2_3, s3, s3_1, s4,s4_1, s4_1_1, s4_1_2, s4_1_3, s4_2, s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5 :: Section

s2_3_intro, s3_1_intro, sys_context_fig,
  s4_1_intro,s4_1_1_intro,s4_1_1_bullets,s4_1_2_intro,s4_1_2_list,s4_1_3_intro,
  s4_1_3_list,s4_2_intro,s4_2_1_intro, fig_tank, s4_2_6_table1, s4_2_6_table2:: Contents

mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention [Lit (nw ht_trans), Doc' (nw sWHS)], SymbOrder], TAandA]) : 
        map Verbatim [s2, s3, s4]  
        
pcm_si :: SystemInformation
pcm_si = SI srs_swhs srs [thulasi] this_si pcmSymbols (pcmSymbols) acronyms
  
pcm_srs :: Document
pcm_srs = mkDoc mkSRS pcm_si


s2 = SRS.intro [] [s2_3]

s2_3 = SRS.charOfIR [s2_3_intro] []

s2_3_intro = Paragraph $
            (at_start' $ reviewer ^. term) +:+ S "of this" +:+ (phrase $ documentation ^. term) +:+
            S "should have a strong knowledge in" +:+ (phrase $ heat ^. term) +:+ S "transfer" +:+. (phrase $ theory ^. term) +:+
           S "A third or fourth year Mechanical Engineering course on the topic is recommended. The" +:+
            (phrase $ reviewer ^. term) +:+ S "should also have an understanding of differential" +:+ (plural $ equation ^. term) `sC` S "as typically" +:+
           S "covered in first and second year Calculus courses. The" +:+ (plural $ user ^. term) +:+ S "of" +:+ (getAcc sWHS) +:+
           S "can have a lower level expertise, as explained in" +:+ (titleize $ section_ ^. term)
           -- FIXME: Section 3.2 does not exist yet, when it does, add reference

           
s3 = genSysF [s3_1]

s3_1 = SRS.sysCont [s3_1_intro, sys_context_fig] []

s3_1_intro = Paragraph $
              (makeRef sys_context_fig) +:+ S "shows the" +:+. (phrase $ sysCont ^. term) +:+
             S "A circle represents an external entity outside the" +:+ (phrase $ software ^. term) `sC`
             S "the" +:+ (phrase $ user ^. term) +:+ S "in this case. A rectangle represents the" +:+ (phrase $ softwareSys ^. term) +:+
             S "itself" +:+. sParen (getAcc sWHS) +:+ S "Arrows are used to show the" +:+ (plural $ datum ^. term) +:+ S "flow between the" +:+
              (phrase $ section_ ^. term) +:+ S "and its" +:+. (phrase $ environment ^. term)
            
sys_context_fig = Figure ((makeRef sys_context_fig) :+: S ":" +:+ (titleize $ sysCont ^. term))
            "SystemContextFigure.png"

           
s4 = specSysDesF (words_) [s4_1, s4_2]
  where words_ = (plural definition +:+ S "and finally the" +:+
                (phrase $ inModel ^. term) +:+ sParen (getAcc ode) +:+
                S "that models the" +:+. (phrase $ sWHT ^. term))

s4_1 = SRS.probDesc [s4_1_intro] [s4_1_1, s4_1_2, s4_1_3]

s4_1_intro = Paragraph $
            (getAcc sWHS) +:+ S "is a computer" +:+ (phrase $ program ^. term) +:+ S "developed to investigate" +:+
           S "the heating of" +:+ (phrase $ water ^. term) +:+ S "in a" +:+. (phrase $ sWHT ^. term)

s4_1_1 = SRS.termAndDefn [s4_1_1_intro, s4_1_1_bullets] []
  
s4_1_1_intro = Paragraph $
           S "This subsection provides a list of terms that" +:+
           S "are used in subsequent" +:+ (plural $ section_ ^. term) +:+ S "and their meaning, with the" +:+
            (phrase $ purpose ^. term) +:+ S "of reducing ambiguity and making it easier to correctly" +:+
           S "understand the" +: (plural $ requirement ^. term)
  
s4_1_1_bullets = Enumeration $ (Bullet $ map (\c -> Flat $ 
  ((at_start $ c ^. term)) +: (c ^. defn)) 
  [thermal_flux, heat_cap_spec])
  
s4_1_2 = SRS.physSyst [s4_1_2_intro, s4_1_2_list, fig_tank] []

s4_1_2_intro = Paragraph $
           S "The" +:+ (phrase $ physicalSystem ^. term) +:+ S "of" +:+ (getAcc sWHS) `sC`
           S "as shown in" +:+ (makeRef fig_tank) `sC` S "includes the following" +: (plural $ element ^. term)

fig_tank = Figure ((at_start $ sWHT ^. term) `sC` S "with" +:+ (phrase $ ht_flux ^. term) +:+ S "from" +:+ (phrase $ coil ^. term) +:+ S "of" +:+
            P (ht_flux_C ^. symbol)) "TankWaterOnly.png"
  
s4_1_2_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
            (S "PS1", (at_start $ tank ^. term) +:+ S "containing" +:+ (phrase $ water ^. term)), 
            (S "PS2", S "Heating" +:+ (phrase $ coil ^. term) +:+ S "at bottom of" +:+. (phrase $ tank ^. term) +:+
           sParen (P (ht_flux_C ^. symbol) +:+ S "represents the" +:+ (phrase $ ht_flux_C ^. term) +:+
           S "into the" +:+. (phrase $ water ^. term)))]

s4_1_3 = SRS.goalStmt [s4_1_3_intro, s4_1_3_list] []

s4_1_3_intro = Paragraph $
           S "Given the" +:+ (phrase $ temp ^. term) +:+ S "of the" +:+ (phrase $ coil ^. term) `sC` S "initial" +:+
            (phrase $ temp ^. term) +:+ S "of the" +:+ (phrase $ water ^. term) :+: S "," +:+
           S "and material" +:+ (plural $ property ^. term) `sC` S "the goal statement is"

s4_1_3_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
            (S "GS1", S "predict the " :+: (phrase $ temp_water ^. term) +:+ S "over time")]

s4_2 = SRS.solCharSpec [s4_2_intro] [s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5]

s4_2_intro = Paragraph $
           S "The" +:+ (phrase $ inModel ^. term) +:+ sParen (getAcc ode) +:+
           S "that governs" +:+ (getAcc sWHS) +:+. S "is presented in" +:+ --TODO: Subsec reference
           S "The" +:+ (phrase $ information ^. term) +:+
           S "to understand the meaning of the" +:+ (phrase $ inModel ^. term) +:+ 
           S "and its derivation is also" +:+ S "presented, so that the" +:+ 
            (phrase $ inModel ^. term) +:+. S "can be verified"
  
s4_2_1 = SRS.assump [s4_2_1_intro] []

s4_2_1_intro = Paragraph $
           S "This" +:+ (phrase $ section_ ^. term) +:+
           S "simplifies the original" +:+ (phrase $ problem ^. term) +:+
           S "and helps in developing the" +:+ (phrase $ thModel ^. term) +:+
           S "by filling in the missing" +:+ (phrase $ information ^. term) +:+
           S "for the" +:+. (phrase $ physicalSystem ^. term) +:+ S "The" +:+ (plural $ number ^. term)+:+
           S "given in the square brackets refer to the" +:+ foldr1 (:+:) (intersperse (S ", ") 
            (map (\ch -> (phrase $ ch ^. term) +:+ sSqBr (getAcc ch)) [thModel, genDefn, dataDefn, inModel]))
            `sC` S "or" +:+ phrase likelyChg +:+ sSqBr (getAcc likelyChg) `sC`
           S "in which the respective" +:+ (phrase assumption) +:+. S "is used"
--TODO: Simple List

s4_2_2 = thModF (getAcc sWHS) [s4_2_2_TMods, s4_2_6_table1, s4_2_6_table2]
  
s4_2_2_TMods :: Contents
s4_2_2_TMods = Definition $ Theory t1consThermE

s4_2_3 = SRS.genDefn [] []

s4_2_4 = SRS.dataDefn [] []

s4_2_5 = inModelF s4_1 s4_2_4 s4_2_2 s4_2_3 []

s4_2_6_table1 = Table [S "Var", titleize' physicalConstraint, S "Typical Value"]
  (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2)] $ map (mkConstraintList) []) 
    (S "Table 1: Input Variables") True

s4_2_6_table2 = Table [S "Var", titleize' physicalConstraint, S "Typical Value"]
  (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2)] $ map (mkConstraintList) []) 
    (S "Table 2: Output Variables") True
