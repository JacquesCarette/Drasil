module Drasil.HGHC.HGHC(srsBody, mgBody, misBody, modules) where

import Data.List (intersperse)
import Control.Lens ((^.))

import Drasil.HGHC.HeatTransfer
import Drasil.TableOfUnits
import Drasil.TableOfSymbols
import Drasil.HGHC.Modules

import Language.Drasil
import Data.Drasil.SI_Units (si_units)

vars :: [QDefinition]
vars = [h_g, h_c]

modules :: [ModuleChunk]
modules = [mod_calc, mod_hw, mod_inputp, mod_inputf, mod_behav]

s1, s2, s3 :: Section --, s4 
s1 = table_of_units si_units -- probably want to not do all of them
s2 = table_of_symbols ((map qs vars) ++ (map qs varChunks)) defaultF
s3 = Section (S "Data Definitions") $ map (Con . Definition . Data) vars
--s4 = Section 0 (S "Code -- Test") $ map (CodeBlock . toCode CLang Calc) [h_c]

--m1,m2,m3 :: LayoutObj
--m1 = Module 1 mod_hw
--m2 = Module 1 mod_behav
--m3 = Module 2 mod_calc

doc :: SymbolForm s => String -> [s] -> String -> [Section] -> Document
doc nam ls author body =
  Document ((S $ nam ++ " for ") :+:
    (foldr1 (:+:) (intersperse (S " and ") (map (\x -> P $ x ^. symbol) ls))))
    (S author) body
  
srsBody :: Document
srsBody = doc "SRS" vars "Spencer Smith" [s1, s2, s3]--, s4]

mgSecs, misSecs :: [Section]
(mgSecs, misSecs) = makeDD [] [] [] modules
  
mgBody :: Document
mgBody = doc "MG" vars "Spencer Smith" mgSecs

misBody :: Document
misBody = doc "MIS" vars "Spencer Smith" misSecs
