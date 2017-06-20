module Drasil.HGHC.HGHC(srsBody, mgBody, misBody, modules) where

import Data.List (intersperse)
import Control.Lens ((^.))

import Drasil.HGHC.HeatTransfer
import Drasil.HGHC.Modules
import Drasil.DocumentLanguage
import Drasil.Sections.ReferenceMaterial (intro)

import Drasil.Template.DD

import Language.Drasil

import Data.Drasil.SI_Units (si_units)
import Data.Drasil.Authors (spencerSmith)
import Data.Drasil.Concepts.Documentation (srs)
import Data.Drasil.Modules

import Drasil.Sections.SpecificSystemDescription

modules :: [ModuleChunk]
modules = [mod_calc, mod_hw, mod_inputp, mod_inputf, mod_behav, mod_outputf,
  mod_ctrl]
  
thisSI :: SystemInformation
thisSI = SI hghc srs [spencerSmith] si_units symbols ([] :: [UCWrapper]) ([] :: [CI]) ([] :: [Block QDefinition])
  
thisSRS :: DocDesc
thisSRS = RefSec (RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention [Lit (nw nuclearPhys), Manual (nw fp)]]]) : [Verbatim s3]
  
s3 :: Section --, s4 
s3 = dataDefnF EmptyS (map (Definition hghcSymMap . Data) vars)
  
srsBody :: Document
srsBody = mkDoc thisSRS thisSI

mgSecs, misSecs :: [Section]
(mgSecs, misSecs) = makeDD [] [] [] modules
  
mgBody :: Document
mgBody = doc "MG" vars (name spencerSmith) mgSecs

misBody :: Document
misBody = doc "MIS" vars (name spencerSmith) misSecs

doc :: SymbolForm s => String -> [s] -> Sentence -> [Section] -> Document
doc nam ls author body =
  Document ((S nam +:+ S "for") +:+
  (foldr1 (+:+) (intersperse (S "and") (map (\x -> P $ x ^. symbol) ls))))
  author body
