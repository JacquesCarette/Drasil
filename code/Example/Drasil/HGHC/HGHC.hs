module Drasil.HGHC.HGHC(srsBody, mgBody, misBody, modules) where

import Data.List (intersperse)
import Control.Lens ((^.))

import Drasil.HGHC.HeatTransfer (htVars,htTransCladFuel,htTransCladCool)
import Drasil.TableOfUnits
import Drasil.TableOfSymbols
import Drasil.HGHC.Modules

import Language.Drasil

import Data.Drasil.SI_Units (si_units)
import Data.Drasil.Authors (spencerSmith)

vars :: [QDefinition]
vars = [htTransCladFuel, htTransCladCool]

modules :: [ModuleChunk]
modules = [mod_calc, mod_hw, mod_inputp, mod_inputf, mod_behav, mod_outputf,
  mod_ctrl]

s1, s2, s3 :: Section --, s4 
s1 = table_of_units si_units -- probably want to not do all of them
s2 = table_of_symbols ((map qs vars) ++ (map qs htVars)) (\x -> phrase $ x ^. term)
s3 = Section (S "Data Definitions") $ map (Con . Definition . Data) vars
--s4 = Section 0 (S "Code -- Test") $ map (CodeBlock . toCode CLang Calc) [htTransCladCool]

--m1,m2,m3 :: LayoutObj
--m1 = Module 1 mod_hw
--m2 = Module 1 mod_behav
--m3 = Module 2 mod_calc

doc :: SymbolForm s => String -> [s] -> Sentence -> [Section] -> Document
doc nam ls author body =
  Document ((S nam +:+ S "for") +:+
    (foldr1 (+:+) (intersperse (S "and") (map (\x -> P $ x ^. symbol) ls))))
    author body
  
srsBody :: Document
srsBody = doc "SRS" vars (name spencerSmith) [s1, s2, s3]--, s4]

mgSecs, misSecs :: [Section]
(mgSecs, misSecs) = makeDD [] [] [] modules
  
mgBody :: Document
mgBody = doc "MG" vars (name spencerSmith) mgSecs

misBody :: Document
misBody = doc "MIS" vars (name spencerSmith) misSecs
