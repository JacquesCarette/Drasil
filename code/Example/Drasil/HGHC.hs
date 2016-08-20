module Example.Drasil.HGHC(srsBody, mgBody, misBody) where

import Data.List (intersperse)
import Control.Lens ((^.))

import Example.Drasil.HeatTransfer
import Example.Drasil.Units
import Example.Drasil.TableOfSymbols
import Example.Drasil.Modules
import Example.Drasil.Concepts

import Language.Drasil
import Language.Drasil.SI_Units (si_units)

vars :: [EqChunk]
vars = [h_g, h_c]

modules :: [ModuleChunk]
modules = [mod_calc, mod_hw, mod_behav]

s1, s2, s3 :: Section --, s4 
s1 = table_of_units si_units -- probably want to not do all of them
s2 = table_of_symbols $ (map uc vars) ++ (map HasNot varChunks)
s3 = Section (S "Data Definitions") $ map (Con . Definition . Data) vars
--s4 = Section 0 (S "Code -- Test") $ map (CodeBlock . toCode CLang Calc) [h_c]

--m1,m2,m3 :: LayoutObj
--m1 = Module 1 mod_hw
--m2 = Module 1 mod_behav
--m3 = Module 2 mod_calc

doc :: Quantity s => String -> [s] -> String -> [Section] -> Document
doc name ls author body =
  Document ((S $ name ++ " for ") :+:
    (foldr1 (:+:) (intersperse (S " and ") (map (\x -> P $ x ^. symbol) ls))))
    (S author) body
  
srsBody :: Document
srsBody = doc "SRS" vars "Spencer Smith" [s1, s2, s3]--, s4]

(mgSecs, misSecs) = makeDD [] [] modules
  
mgBody :: Document
mgBody = doc "MG" vars "Spencer Smith" mgSecs

misBody :: Document
misBody = doc "MIS" vars "Spencer Smith" misSecs
  
-- lpmBody :: Document  
-- lpmBody = Document ((S "Literate Programmer's Manual for ") :+: 
  -- (foldr1 (:+:) (intersperse (S " and ") (map (\x -> U $ x ^. symbol) vars))))
  -- (S "Spencer Smith") [l1]

-- l1 :: LayoutObj
-- l1 = Paragraph (
  -- S "@ First we define the overall structure of the library of functions." :+:
  -- S "\n\n@c\n@<Header files@>@/\n@<Functions@>@/\n\n" :+:
  -- S "Although not necessary for this simple example, we will include the " :+:
  -- S "math library, since future expansion will likely require it.\n\n" :+:
  -- S "@<Header files@>=\n#include <math.h>\n\n" :+:
  -- S "@ This library will consist of a set of functions.\n\n" :+:
  -- S "@<Functions@>=\n@<Function to Calculate hg@>@/\n" :+: 
  -- S "@<Function to Calculate hc@>@/\n\n"  
  -- )
