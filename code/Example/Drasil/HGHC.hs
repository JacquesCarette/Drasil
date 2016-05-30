module Example.Drasil.HGHC(srsBody, lpmBody) where

import Data.List (intersperse)
import Control.Lens ((^.))

import Example.Drasil.HeatTransfer
import Example.Drasil.Units
import Example.Drasil.TableOfSymbols

import Language.Drasil
import Language.Drasil.SI_Units (si_units)

vars :: [EqChunk]
vars = [h_g, h_c]

s1, s2, s3, s4 :: LayoutObj
s1 = table_of_units si_units
s2 = table_of_symbols vars
s3 = Section 0 (S "Data Definitions") $ map (Definition . Data) vars
s4 = Section 0 (S "Test Space") ((map (CodeBlock . toCode CLang Calc) [h_c]) ++
        [Paragraph (Quote (S "This is a quote with " :+: U (h_c ^. symbol)))])

srs :: Quantity s => [s] -> String -> [LayoutObj] -> Document
srs ls author body =
  Document ((S "SRS for ") :+: 
    (foldr1 (:+:) (intersperse (S " and ") (map (\x -> U $ x ^. symbol) ls))))
    (S author) body  
  
srsBody,lpmBody :: Document
srsBody = srs vars "Spencer Smith" [s1, s2, s3, s4]
  
lpmBody = Document ((S "Literate Programmer's Manual for ") :+: 
  (foldr1 (:+:) (intersperse (S " and ") (map (\x -> U $ x ^. symbol) vars))))
  (S "Spencer Smith") [l1]

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
