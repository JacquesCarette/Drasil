module Drasil.Template.Body where

import Language.Drasil
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, RefbyMap, ReferenceDB, SystemInformation(SI),
  TraceMap, cdb, generateRefbyMap, rdb, refdb, _authors, _concepts, _constants,
  _constraints, _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs,
  _quants, _sys, _sysinfodb, _usedinfodb)

import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import Drasil.DocLang (DocDesc, generateTraceMap, generateTraceMap', mkDoc)

import Data.Drasil.Concepts.Documentation as Doc (srs)

import Data.Drasil.IdeaDicts (physics)
import Data.Drasil.Phrase (for'')

import qualified Data.Map as Map

srsDoc :: Document
srsDoc = mkDoc mkSRS (for'' titleize phrase) systInfo

mkSRS :: DocDesc
mkSRS = []

systInfo :: SystemInformation
systInfo = SI {
  _sys         = example,
  _kind        = Doc.srs,
  _authors     = [authorName],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = [] :: [QDefinition],
  _datadefs    = [] :: [DataDefinition],
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
symbMap = cdb ([] :: [QuantityDict]) ([nw example]) ([] :: [ConceptChunk]) ([] :: [UnitDefn]) label refBy
  ([] :: [DataDefinition]) ([] :: [InstanceModel]) ([] :: [GenDefn]) ([] :: [TheoryModel])
  ([] :: [ConceptInstance]) ([] :: [Section]) ([] :: [LabelledContent])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk]) ([] :: [UnitDefn]) label refBy
  ([] :: [DataDefinition]) ([] :: [InstanceModel]) ([] :: [GenDefn]) ([] :: [TheoryModel])
  ([] :: [ConceptInstance]) ([] :: [Section]) ([] :: [LabelledContent])

refDB :: ReferenceDB
refDB = rdb [] []

label :: TraceMap
label = Map.union (generateTraceMap mkSRS) $ generateTraceMap' []
 
refBy :: RefbyMap
refBy = generateRefbyMap label

printSetting :: PrintingInformation
printSetting = PI symbMap defaultConfiguration

-- MOVE TO CONCEPTS
example :: CI -- name of example
example = commonIdeaWithDict "example" (pn "Template") "Template" [physics]

-- MOVE TO DATA.PEOPLE
authorName :: Person
authorName = person "Author" "Name"
