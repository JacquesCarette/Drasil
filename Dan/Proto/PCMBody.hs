{-# OPTIONS -Wall #-} 
{-# LANGUAGE FlexibleContexts #-} 
module PCMBody where
import Data.Char (toLower)
import Data.List (intersperse)
import Helpers
import PCMExample
import Spec (Spec(..),sMap) --May need to update imports to hide Ref.
                            --More likely setup an API or something to
                            --Restrict access for novice users.
import Format (FormatC(..))
import Unit (Unit(..), UnitDefn(..))
import SI_Units 
import Chunk
import Control.Lens ((^.))
import RecipeTools
import PCMUnits
import LayoutObjs
import Reference
-- import ToCode
-- import ASTCode

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

s1, s1_intro, s1_1, s1_1_intro, s1_1_table, s1_2, s1_2_intro, 
  s1_2_table, s1_3, s1_3_table,s4,s4_intro,s4_1,s4_1_intro,
  s4_1_1,s4_1_1_intro,s4_1_1_bullets,s4_1_2,s4_1_2_intro,s4_1_3,
  s4_1_3_intro,s4_2,s4_2_intro,s4_2_1,s4_2_1_intro,s4_2_2,
  s4_2_2_intro:: LayoutObj

pcm_srs :: Document  
pcm_srs = Document (S "Software Requirements Specification for Solar Water " :+:
          S "Heating Systems") 
          (S "Thulasi Jegatheesan") [s1,s4]

s1 = Section 0 (S "Reference Material") [s1_intro, s1_1, s1_2, s1_3]

s1_intro = Paragraph (S "This section records information for easy reference")

s1_1 = Section 1 (S "Table of Units") [s1_1_intro, s1_1_table]

s1_1_intro = Paragraph (S "Throughout this document SI (Syst" :+: 
           (F Grave (S "e")) :+: S "me International d'Unit" :+:
           (F Acute (S "e")) :+: S "s) is employed as the unit system." :+:
           S " In addition to the basic units, several derived units are" :+: 
           S " employed as described below. For each unit, the symbol is" :+: 
           S " given followed by a description of the unit followed by " :+: 
           S "the SI name.")

s1_1_table = Table [S "Symbol", S "Description", S "Name"] (mkTable
  [(\x -> Sy (x ^. unit)),
   (\x -> (x ^. descr)),
   (\x -> S (x ^. name))
  ] this_si)
  (S "Table of Units") True

s1_2 = Section 1 (S "Table of Symbols") [s1_2_intro, s1_2_table]

s1_2_intro = Paragraph $ 
  S "The table that follows summarizes the symbols used in this " :+:
  S "document along with their units.  The choice of symbols was " :+:
  S "made to be consistent with the heat transfer literature and " :+:
  S "with existing documentation for solar water heating systems."
  
s1_2_table = Table [S "Symbol", S "Units", S "Description"] (mkTable
  [(\ch -> N (ch ^. symbol)) , 
   (\ch -> Sy $ ch ^. unit),
   (\ch -> ch ^. descr)
   ]
  pcmSymbols)
  (S "Table of Symbols") False

s1_3 = Section 1 (S "Abbreviations and Acronyms") [s1_3_table]

s1_3_table = Table [S "Symbol", S "Description"] (mkTable
  [(\ch -> S $ ch ^. name),
   (\ch -> ch ^. descr)]
  acronyms)
  (S "Abbreviations and Acronyms") False

s4 = Section 0 (S "Specific System Description") [s4_intro, s4_1,s4_2]

s4_intro = Paragraph $ S "This section first presents the problem " :+:
  S "description, which gives a high-level view of the problem to be solved" :+:
  S ". This is followed by the solution characteristics specification, " :+:
  S "which presents the assumptions, theories, definitions and finally the " :+:
  S "instance model (ODE) that models the solar water heating tank."

s4_1 = Section 1 (S "Problem Description") [s4_1_intro, s4_1_1, s4_1_2, s4_1_3]

s4_1_intro = Paragraph $ S (sWHS ^. name) :+: S " is a computer program " :+:
  S "developed to investigate the heating of water in a solar water heating" :+:
  S " tank."

s4_1_1 = Section 2 (S "Terminology and Definitions") [s4_1_1_intro, s4_1_1_bullets]
  
s4_1_1_intro = Paragraph $ S "This subsection provides a list of terms that " :+:
  S "are used in subsequent sections and their meaning, with the purpose of ":+:
  S "reducing ambiguity and making it easier to correctly understand the ":+:
  S "requirements:"
  
s4_1_1_bullets = BulletList $ map (\c -> S (capitalize (c ^. name)) :+: 
  S ": " :+: (c ^. descr)) [thermFluxU, heat_capacity]
  
s4_1_2 = Section 2 (physSysDescr ^. descr) [s4_1_2_intro,s4_1_2_list,fig_tank]

s4_1_2_intro = Paragraph $ S "The physical system of SWHS, as shown in " :+:
  (makeRef fig_tank) :+: S ", includes the following elements:"

fig_tank = Figure (S "Solar water heating tank, with heat flux from coil of ":+:
            N (ht_flux_C ^. symbol)) "TankWaterOnly.png"
  
s4_1_2_list = SimpleList $ [
  (S "PS1", S "Tank containing water"), 
  (S "PS2", S "Heating coil at bottom of tank. (" :+:
  N (ht_flux_C ^. symbol) :+: S " represents the " :+: (ht_flux_C ^. descr) :+:
  S " into the water.)")]

s4_1_3 = Section 2 ((goalStmt ^. descr) :+: S "s") [s4_1_3_intro]
s4_1_3_intro = Paragraph $ S "Given the temperature of the coil, initial " :+:
  S "temperature of the water, and material properties, the goal statement is"

s4_1_3_list = SimpleList $ [
  (S "GS1", S "predict the " :+: (temp_water ^. descr) :+: " over time")]

s4_2 = Section 1 (S "Solution Characteristics Specification") 
  [s4_2_intro,s4_2_1,s4_2_2]

s4_2_intro = Paragraph $ S "The " :+: 
  (sMap (map toLower) (instanceMod ^. descr)) :+:
  S (" " ++ (paren $ oDE ^. name)) :+: S " that governs " :+: 
  S (sWHS ^. name) :+: S " is presented in " :+: --TODO: Subsec reference
  S ". The information to understand the meaning of the " :+:
  (sMap (map toLower) (instanceMod ^. descr)) :+: 
  S " and its derivation is also" :+: S " presented, so that the " :+: 
  (sMap (map toLower) (instanceMod ^. descr)) :+: S " can be verified."
  
s4_2_1 = Section 2 (assumption ^. descr :+: S "s") [s4_2_1_intro]

s4_2_1_intro = Paragraph $ S "This section simplifies the original problem " :+:
  S "and helps in developing the theoretical model by filling in the " :+:
  S "missing information for the physical system. The numbers given in the " :+:
  S "square brackets refer to the " :+: foldr1 (:+:) (intersperse (S ", ") 
  (map (\ch -> (sMap (map toLower) (ch ^. descr)) :+: S (" " ++ 
  sqbrac (ch ^. name))) [theoreticMod, genDefn, dataDefn, instanceMod])) :+: 
  S ", or " :+: (sMap (map toLower) $ likelyChange ^. descr) :+: S (" " ++ 
  sqbrac (likelyChange ^. name)) :+: S ", in which the respective " :+: 
  (sMap (map toLower) $ assumption ^. descr) :+: S " is used."
--TODO: Simple List

s4_2_2 = Section 2 ((theoreticMod ^. descr) :+: S "s") 
  (s4_2_2_intro:s4_2_2_TMods)

s4_2_2_intro = Paragraph $ S "This section focuses on the general equations ":+:
  S "and laws that " :+: S (sWHS ^. name) :+: S " is based on."
  
s4_2_2_TMods :: [LayoutObj]
s4_2_2_TMods = map Definition (map Theory [t1consThermE])
