{-# OPTIONS -Wall #-} 
{-# LANGUAGE FlexibleContexts #-} 
module PCMBody where

-- import Data.List (intersperse)
import Helpers
import PCMExample
import Spec (Spec(..), LayoutObj(..), Document(..)) --, DType(Data))
import Format (FormatC(..))
import Unit (Unit(..), UnitDefn(..))
import SI_Units 
import Chunk
import Control.Lens ((^.))
import RecipeTools
import PCMUnits
-- import ToCode
-- import ASTCode

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

s1, s1_intro, s1_1, s1_1_intro, s1_1_table, s1_2, s1_2_intro, 
  s1_2_table, s1_3, s1_3_table :: LayoutObj

pcm_srs :: Document  
pcm_srs = Document (S "Software Requirements Specification for Solar Water " :+:
          S "Heating Systems Incorporating Phase Change Material") 
          (S "Thulasi Jegatheesan") [s1,s4]

s1 = Section (S "Reference Material") [s1_intro, s1_1, s1_2, s1_3]

s1_intro = Paragraph (S "This section records information for easy reference")

s1_1 = SubSection (S "Table of Units") [s1_1_intro, s1_1_table]

s1_1_intro = Paragraph (S "Throughout this document SI (Syst" :+: 
           (F Grave (S "e")) :+: S "me International d'Unit" :+:
           (F Acute (S "e")) :+: S "s) is employed as the unit system." :+:
           S " In addition to the basic units, several derived units are" :+: 
           S " employed as described below. For each unit, the symbol is" :+: 
           S " given followed by a description of the unit followed by " :+: 
           S "the SI name.")

s1_1_table = Table [S "Symbol", S "Description", S "Name"] $ mkTable
  [(\x -> Sy (x ^. unit)),
   (\x -> S (x ^. descr)),
   (\x -> S (x ^. name))
  ] this_si

s1_2 = SubSection (S "Table of Symbols") [s1_2_intro, s1_2_table]

s1_2_intro = Paragraph $ 
  S "The table that follows summarizes the symbols used in this " :+:
  S "document along with their units.  The choice of symbols was " :+:
  S "made to be consistent with the heat transfer literature and " :+:
  S "with existing documentation for solar water heating systems."
  
s1_2_table = Table [S "Symbol", S "Units", S "Description"] $ mkTable
  [(\ch -> N (ch ^. symbol)) , 
   (\ch -> Sy $ ch ^. unit),
   (\ch -> S $ ch ^. descr)
   ]
  pcmSymbols

s1_3 = SubSection (S "Abbreviations and Acronyms") [s1_3_table]

s1_3_table = Table [S "Symbol", S "Description"] (map (map S)
  [ --Should turn these into chunks
  ["A", "Assumption"],["DD","Data Definition"],["GD","General Definition"],
  ["GS", "Goal Statement"],["IM","Instance Model"],["LC","Likely Change"],
  ["ODE","Ordinary Differential Equation"],["PS","Physical System Description"],
  ["R","Requirement"],["SRS","Software Requirements Specification"],
  ["SWHS","Solar Water Heating System"],["T","Theoretical Model"]
  ])

s4 = Section (S "Problem Description") [s4_intro, s4_1]

s4_intro = Paragraph $ S "TODO: Adapt the original to this"

s4_1 = SubSection (S "Terminology and Definitions") [s4_1_intro, s4_1_bullets]

s4_1_intro = Paragraph $ S "This subsection provides a list of terms that " :+:
  S "are used in subsequent sections and their meaning, with the purpose of ":+:
  S "reducing ambiguity and making it easier to correctly understand the ":+:
  S "requirements:"
  
s4_1_bullets = BulletList $ map (\c -> S (capitalize (c ^. name)) :+: S ": " :+:
  S (c ^. descr)) [thermFluxU, heat_capacity]