module Example.Drasil.HGHC(srsBody, lpmBody, mgBody) where

import Data.List (intersperse)
import Control.Lens ((^.))

import Example.Drasil.HeatTransfer
import Example.Drasil.Units
import Example.Drasil.TableOfSymbols
import Example.Drasil.Modules

import Language.Drasil
import Language.Drasil.SI_Units (si_units)

vars :: [EqChunk]
vars = [h_g, h_c]

s1, s2, s3, s4 :: LayoutObj
s1 = table_of_units si_units
s2 = table_of_symbols vars
s3 = Section 0 (S "Data Definitions") $ map (Definition . Data) vars
s4 = Section 0 (S "Code -- Test") $ [CodeBlock $ toCodeModule CLang mod_calc]

m1,m2,m3 :: LayoutObj
m1 = Module 0 mod_hw
m2 = Module 0 mod_behav
m3 = Module 1 mod_calc

srs :: Quantity s => [s] -> String -> [LayoutObj] -> Document
srs ls author body =
  Document ((S "SRS for ") :+: 
    (foldr1 (:+:) (intersperse (S " and ") (map (\x -> U $ x ^. symbol) ls))))
    (S author) body

mg :: Quantity s => [s] -> String -> [LayoutObj] -> Document
mg ls author body =
  Document ((S "MG for ") :+:
    (foldr1 (:+:) (intersperse (S " and ") (map (\x -> U $ x ^. symbol) ls))))
    (S author) body
  
srsBody,mgBody,lpmBody :: Document
srsBody = srs vars "Spencer Smith" [s1, s2, s3, s4]
mgBody = mg vars "Spencer Smith" [m1, m2, m3]
  
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
