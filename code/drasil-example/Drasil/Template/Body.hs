module Drasil.Template.Body where

import Language.Drasil
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _purpose, _concepts, _constants, _constraints, 
  _datadefs, _instModels, _configFiles, _defSequence, _inputs, _kind, _outputs, 
  _quants, _sys, _sysinfodb, _usedinfodb, _folderPath)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import qualified Utils.Drasil.Sentence as S

import Drasil.DocLang (SRSDecl, mkDoc, fillTraceSI)

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillTraceSI mkSRS si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = []

si :: SystemInformation
si = SI {
  _sys         = example,
  _kind        = Doc.srs,
  _authors     = [authorName],
  _purpose     = [],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = [] :: [InstanceModel],
  _datadefs    = [] :: [DataDefinition],
  _configFiles = [],
  _folderPath  = "",
  _inputs      = [] :: [QuantityDict],
  _outputs     = [] :: [QuantityDict],
  _defSequence = [] :: [Block QDefinition],
  _constraints = [] :: [ConstrainedChunk],
  _constants   = [] :: [QDefinition],
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
