module Example.Drasil.HGHC(srsBody, lpmBody) where

import Data.List (intersperse)
import Control.Lens ((^.))

import Example.Drasil.HeatTransfer
import Example.Drasil.Units

import Language.Drasil
import Language.Drasil.SI_Units (si_units)

vars :: [EqChunk]
vars = [h_g, h_c]

s1, s2, s2_intro, s2_table, 
  s3, s3_dd1, s3_dd2, s4, s4c :: LayoutObj

s1 = table_of_units si_units

s2 = Section 0 (S "Table of Symbols") [s2_intro, s2_table]

s2_intro = Paragraph $ 
  S "The table that follows summarizes the symbols used in this " :+:
  S "document along with their units.  The choice of symbols was " :+:
  S "made with the goal of being consistent with the nuclear " :+:
  S "physics literature and that used in the FP manual.  The SI " :+:
  S "units are listed in brackets following the definition of " :+:
  S "the symbol."
  
s2_table = Table [S "Symbol", S "Description", S "Units"] (mkTable
  [(\ch -> U (ch ^. symbol)) , 
   (\ch -> ch ^. descr), 
   (\ch -> Sy $ ch ^. unit)]
  vars)
  (S "Table of Symbols") False

s3 = Section 0 (S "Data Definitions") $ map (Definition . Data) vars

s4 = Section 0 (S "Code -- Test") [s4c]

s4c = CodeBlock (toCode CLang Calc h_c)

srs :: Quantity s => [s] -> String -> [LayoutObj] -> Document
srs ls author body =
  Document ((S "SRS for ") :+: 
    (foldr1 (:+:) (intersperse (S " and ") (map (\x -> U $ x ^. symbol) ls))))
    (S author) body  
  
srsBody,lpmBody :: Document
srsBody = srs [h_g, h_c] "Spencer Smith" [s1, s2, s3, s4]
  
lpmBody = Document ((S "Literate Programmer's Manual for ") :+: 
  (U $ h_g ^. symbol) :+: 
  (S "and ") :+: (U $ h_c ^. symbol)) (S "Spencer Smith") [l1]

l1 :: LayoutObj
l1 = Paragraph (
  S "@ First we define the overall structure of the library of functions." :+:
  S "\n\n@c\n@<Header files@>@/\n@<Functions@>@/\n\n" :+:
  S "Although not necessary for this simple example, we will include the " :+:
  S "math library, since future expansion will likely require it.\n\n" :+:
  S "@<Header files@>=\n#include <math.h>\n\n" :+:
  S "@ This library will consist of a set of functions.\n\n" :+:
  S "@<Functions@>=\n@<Function to Calculate hg@>@/\n" :+: 
  S "@<Function to Calculate hc@>@/\n\n"  
  )
