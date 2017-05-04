module Drasil.PCM.Body where

import Data.Char (toLower)
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

import           Drasil.TableOfAbbAndAcronyms
import qualified Drasil.SRS as SRS
import           Drasil.ReferenceMaterial

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

s1, s1_1,s1_2, s1_3,s4,s4_1,s4_1_1,s4_1_2,s4_1_3,s4_2,s4_2_1,s4_2_2 :: Section

s1_1_intro, s1_1_table, s1_2_intro, s1_2_table, s4_intro,
  s4_1_intro,s4_1_1_intro,s4_1_1_bullets,s4_1_2_intro,s4_1_2_list,s4_1_3_intro,
  s4_1_3_list,s4_2_intro,s4_2_1_intro,s4_2_2_intro, fig_tank:: Contents

pcm_srs :: Document  
pcm_srs = SRS.doc' sWHS (name thulasi) [s1,s4]

s1 = refSec [s1_1, s1_2, s1_3]

s1_1 = Section (S "Table of Units") [Con s1_1_intro,Con s1_1_table]

s1_1_intro = Paragraph (S "Throughout this"+:+ (phrase $ document ^. term) +:+
           S "SI (Syst" :+: (F Grave 'e') :+: S "me International d'Unit" :+:
           (F Acute 'e') :+: S "s) is employed as the" +:+ (phrase $ unit_ ^. term) +:+
           S "system." +:+ S "In addition to the basic" +:+ (plural $ unit_ ^. term) :+:
           S ", several derived" +:+ (plural $ unit_ ^. term) +:+ S "are" +:+ 
           S "employed as described below. For each" +:+ (phrase $ unit_ ^. term) :+:
           S ", the" +:+ (phrase $ symbol_ ^. term) +:+ S "is given followed by a description" +:+
           S "of the" +:+ (phrase $ unit_ ^. term) +:+ S "followed by the SI name.")

s1_1_table = Table [(phrase $ symbol_ ^. term), (phrase $ description ^. term), S "Name"] (mkTable --FIXME: Change to RefSec (see line 72 SWHS Body)
  [(\x -> Sy (x ^. usymb)),
   (\x -> (x ^. defn)),
   (\x -> (phrase $ x ^. term))
  ] this_si)
  ((titleize $ tOfUnits ^. term)) True

s1_2 = Section ((phrase $ tOfSymb ^. term)) [Con s1_2_intro,Con s1_2_table]

s1_2_intro = Paragraph $ 
  S "The" +:+ (phrase $ table_ ^. term) +:+ S "that follows summarizes the" +:+ (plural $ symbol_ ^. term) +:+
  S "used in this" +:+ (phrase $ document ^. term) +:+ S "along with their" +:+ (plural $ unit_ ^. term) :+:
  S ".  The choice of" +:+ (plural $ symbol_ ^. term) +:+ S "was" +:+
  S "made to be consistent with the heat transfer literature and" +:+
  S "with existing documentation for solar water heating systems."
  
s1_2_table = Table [(phrase $ symbol_ ^. term), (plural $ units_ ^. term), (phrase $ description ^. term)] (mkTable
  [(\ch -> P (ch ^. symbol)), -- (\ch -> N (ch ^. symbol)) , 
   (\ch -> Sy $ unit_symb ch),
   (\ch -> phrase $ ch ^. term)
   ]
  pcmSymbols)
  ((titleize $ tOfSymb ^. term)) False

s1_3 = table_of_abb_and_acronyms acronyms

s4 = Section (S "Specific System Description") [Con s4_intro, Sub s4_1,Sub s4_2]

s4_intro = Paragraph $ S "This section first presents the problem" +:+
  S "description, which gives a high-level view of the" +:+ (phrase $ problem ^. term) +:+ S "to be solved" :+:
  S ". This is followed by the solution characteristics specification," +:+
  S "which presents the" +:+ (plural assumption) `sC` 
    (plural $ theory ^. term) :+: S ", definitions and finally the" +:+
  S "instance model (ODE) that models the solar water heating tank."

s4_1 = Section (S "Problem Description") [Con s4_1_intro,Sub s4_1_1,
                                            Sub s4_1_2,Sub s4_1_3]

s4_1_intro = Paragraph $ (getAcc sWHS) +:+ S "is a computer program" +:+
  S "developed to investigate the heating of water in a solar water heating" +:+
  S "tank."

s4_1_1 = Section (S "Terminology and Definitions") [Con s4_1_1_intro,
                                                      Con s4_1_1_bullets]
  
s4_1_1_intro = Paragraph $ S "This subsection provides a list of terms that" +:+
  S "are used in subsequent sections and their meaning, with the purpose of" +:+
  S "reducing ambiguity and making it easier to correctly understand the" +:+
  S "requirements:"
  
s4_1_1_bullets = Enumeration $ (Bullet $ map (\c -> Flat $ 
  (sMap capitalize (phrase $ c ^. term)) :+: S ": " :+: (c ^. defn)) 
  [thermal_flux, heat_cap_spec])
  
s4_1_2 = Section (titleize physSyst) [Con s4_1_2_intro,Con s4_1_2_list,
                                            Con fig_tank]

s4_1_2_intro = Paragraph $ S "The physical system of SWHS, as shown in" +:+
  (makeRef fig_tank) :+: S ", includes the following elements:"

fig_tank = Figure (S "Solar water heating tank, with heat flux from coil of" +:+
            P (ht_flux_C ^. symbol)) "TankWaterOnly.png"
  
s4_1_2_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "PS1", S "Tank containing water"), 
  (S "PS2", S "Heating coil at bottom of tank. (" :+:
  P (ht_flux_C ^. symbol) +:+ S "represents the" +:+ (phrase $ ht_flux_C ^. term) +:+
  S "into the water.)")]

s4_1_3 = Section (titleize' goalStmt) [Con s4_1_3_intro,
                                                    Con s4_1_3_list]

s4_1_3_intro = Paragraph $ S "Given the temperature of the coil, initial" +:+
  S "temperature of the water, and material properties, the goal statement is"

s4_1_3_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "GS1", S "predict the " :+: (phrase $ temp_water ^. term) +:+ S "over time")]

s4_2 = Section (S "Solution Characteristics Specification") 
  [Con s4_2_intro,Sub s4_2_1,Sub s4_2_2]

s4_2_intro = Paragraph $ S "The" +:+ 
  (sMap (map toLower) (phrase $ inModel ^. term)) +:+
  S "(" :+: getAcc ode :+: S ") that governs" +:+ 
  (getAcc sWHS) +:+ S "is presented in" +:+ --TODO: Subsec reference
  S ". The information to understand the meaning of the" +:+
  (sMap (map toLower) (phrase $ inModel ^. term)) +:+ 
  S "and its derivation is also" +:+ S "presented, so that the" +:+ 
  (sMap (map toLower) (phrase $ inModel ^. term)) +:+ S "can be verified."
  
s4_2_1 = Section (titleize' assumption) [Con s4_2_1_intro]

s4_2_1_intro = Paragraph $ S "This section simplifies the original problem" +:+
  S "and helps in developing the theoretical model by filling in the" +:+
  S "missing information for the physical system. The numbers given in the" +:+
  S "square brackets refer to the" +:+ foldr1 (:+:) (intersperse (S ", ") 
  (map (\ch -> (sMap (map toLower) (phrase $ ch ^. term)) +:+ S "[" :+:
  (getAcc ch) :+: S "]") [thModel, genDefn, dataDefn, inModel])) `sC` 
  S "or" +:+ phrase likelyChg +:+ S "[" :+: 
  (getAcc likelyChg) :+: S "], in which the respective" +:+ 
  (phrase assumption) +:+. S "is used"
--TODO: Simple List

s4_2_2 = Section (titleize' thModel) 
  ((Con s4_2_2_intro):(Con s4_2_2_TMods):[])

s4_2_2_intro = Paragraph $ S "This section focuses on the general equations" +:+
  S "and laws that" +:+ (getAcc sWHS) +:+ S "is based on." 
-- :+: foldr1 (:+:) (map makeRef s4_2_2_TMods) :+: S" " :+: makeRef s1
  
s4_2_2_TMods :: Contents
s4_2_2_TMods = Definition $ Theory t1consThermE
