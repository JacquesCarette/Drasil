module Drasil.HGHC.HGHC (srsBody, mgBody, misBody, modules) where

import Language.Drasil
import Drasil.DocumentLanguage

import Drasil.Template.DD (makeDD)

import Data.List (intersperse)
import Control.Lens ((^.))

import Drasil.HGHC.HeatTransfer (hghcVars, hghcSymMap, fp, htOutputs,
  htInputs, symbols, nuclearPhys, hghc)
import Drasil.HGHC.Modules (mod_calc, mod_inputp, mod_inputf,
  mod_outputf, mod_ctrl)

import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.SpecificSystemDescription (dataDefnF)

import Data.Drasil.SI_Units (si_units)
import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Documentation (srs)
import Data.Drasil.Modules (mod_hw, mod_behav)


modules :: [ModuleChunk]
modules = [mod_calc, mod_hw, mod_inputp, mod_inputf, mod_behav, mod_outputf,
  mod_ctrl]

thisChoices :: Choices
thisChoices = Choices {
  lang             = [Python, Cpp, CSharp, Java],
  impType          = Program,
  logFile          = "log.txt",
  logging          = LogNone,
  comments         = CommentNone, 
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure   = AsClass
}

thisCode :: CodeSpec
thisCode = codeSpec' thisSI thisChoices
  
thisSI :: SystemInformation
thisSI = SI {
  _sys = hghc,
  _kind = srs,
  _authors = [spencerSmith],
  _units = si_units,  
  _quants = symbols,
  _concepts = ([] :: [UCWrapper]),
  _namedIdeas = ([] :: [CI]), 
  _definitions = hghcVars,
  _inputs = htInputs,
  _outputs = htOutputs,
  _defSequence = ([] :: [Block QDefinition]),
  _constraints = ([] :: [ConstrainedChunk]),
  _constants = []
}
  
thisSRS :: DocDesc
thisSRS = RefSec (RefProg intro 
  [TUnits, 
  tsymb [TSPurpose, SymbConvention [Lit (nw nuclearPhys), Manual (nw fp)]]])
  : [Verbatim s3]
  
s3 :: Section --, s4 
s3 = dataDefnF EmptyS (map (Definition hghcSymMap . Data) hghcVars)
  
srsBody :: Document
srsBody = mkDoc thisSRS (for) thisSI

mgSecs, misSecs :: [Section]
(mgSecs, misSecs) = makeDD [] [] [] modules
  
mgBody :: Document
mgBody = doc "MG" hghcVars (name spencerSmith) mgSecs

misBody :: Document
misBody = doc "MIS" hghcVars (name spencerSmith) misSecs

doc :: SymbolForm s => String -> [s] -> Sentence -> [Section] -> Document
doc nam ls author body =
  Document ((S nam +:+ S "for") +:+
  (foldr1 (+:+) (intersperse (S "and") (map (\x -> P $ x ^. symbol) ls))))
  author body
