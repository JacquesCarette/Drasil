{-# OPTIONS -Wall #-} 
{-# LANGUAGE FlexibleContexts #-} 
module Body1 where
import Example1
import Spec (Spec(..), LayoutObj(..), Document(..))
import Format (TeX(..), FormatC(..))
import SI_Units (si_units)
import Chunk
import UnitalChunk (unit)
import Control.Lens ((^.))
import RecipeTools

type SRS = TeX
type LPM = TeX

s1, s1_intro, s1_table, s2, s2_intro, s2_table :: LayoutObj

s1 = Section (S "Table of Units") [s1_intro, s1_table]

s1_intro = Paragraph (S "Throughout this document SI (Syst" :+: 
           (F Grave (S "e")) :+: S "me International d'Unit" :+:
           (F Acute (S "e")) :+: S "s) is employed as the unit system." :+:
           S " In addition to the basic units, several derived units are" :+: 
           S " employed as described below. For each unit, the symbol is" :+: 
           S " given followed by a description of the unit with the SI" :+: 
           S " name in parentheses.")

s1_table = Table $ mkTable
  [(\x -> x ^. symbol),
   (\x -> S (x ^. descr))
  ] si_units

s2 = Section (S "Table of Symbols") [s2_intro, s2_table]

s2_intro = Paragraph $ 
  S "The table that follows summarizes the symbols used in this " :+:
  S "document along with their units.  The choice of symbols was " :+:
  S "made with the goal of being consistent with the nuclear " :+:
  S "physics literature and that used in the FP manual.  The SI " :+:
  S "units are listed in brackets following the definition of " :+:
  S "the symbol."
  
s2_table = Table $ mkTable
  [(\c -> c ^. symbol) , (\c -> S $ c ^. descr), unit]
  [h_g,h_c] 

-- s3 = Section (S "Data Definitions") [s3_dd1, s3_dd2]

-- s3_dd1 = Definition Data h_g

-- s3_dd2 = Definition Data h_c

srsBody,lpmBody :: Document
srsBody = Document ((S "SRS for ") :+: (h_g ^. symbol) :+: (S " and ") :+: (h_c ^. symbol)) 
  (S "Spencer Smith") [s1,s2]  -- need to add s3 back
  
lpmBody = Document ((S "Literate Programmer's Manual for ") :+: 
  (h_g ^. symbol) :+: 
  (S "and ") :+: (h_c ^. symbol)) (S "Spencer Smith") [l1]

l1 :: LayoutObj
l1 = Section (Empty) [ Paragraph (
  S "@ First we define the overall structure of the library of functions." :+:
  S "\n\n@c\n@<Header files@>@/\n@<Functions@>@/\n\n" :+:
  S "Although not necessary for this simple example, we will include the " :+:
  S "math library, since future expansion will likely require it.\n\n" :+:
  S "@<Header files@>=\n#include <math.h>\n\n" :+:
  S "@ This library will consist of a set of functions.\n\n" :+:
  S "@<Functions@>=\n@<Function to Calculate hg@>@/\n" :+: 
  S "@<Function to Calculate hc@>@/\n\n"  
  )]
