-- Changes to this template should be reflected in the 'Creating Your Project 
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil
-- This comment can be removed after copying this template to build your own example.

module Drasil.Template.Body where

import Language.Drasil
import Drasil.SRSDocument
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import qualified Language.Drasil.Sentence.Combinators as S

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = []

si :: SystemInformation
si = SI {
  _sys         = example,
  _kind        = Doc.srs,
  _authors     = [authorName],
  _background  = [],
  _purpose     = [],
  _motivation  = [],
  _scope       = [],
  _sftwr_rev   = [],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = [] :: [InstanceModel],
  _datadefs    = [] :: [DataDefinition],
  _configFiles = [],
  _inputs      = [] :: [QuantityDict],
  _outputs     = [] :: [QuantityDict],
  _defSequence = [] :: [Block SimpleQDef],
  _constraints = [] :: [ConstrainedChunk],
  _constants   = [] :: [ConstQDef],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}

symbMap :: ChunkDB
symbMap = cdb ([] :: [QuantityDict]) [nw example] ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent]) ([] :: [Reference])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent]) ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb [] []

-- MOVE TO CONCEPTS
example :: CI -- name of example
example = commonIdeaWithDict "example" (pn "Template") "Template" []

-- MOVE TO DATA.PEOPLE
authorName :: Person
authorName = person "Author" "Name"
