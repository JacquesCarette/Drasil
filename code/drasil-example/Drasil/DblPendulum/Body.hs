module Drasil.DblPendulum.Body where

import Language.Drasil
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _purpose, _concepts, _constants, _constraints, 
  _datadefs, _configFiles, _definitions, _defSequence, _inputs, _kind, _outputs, 
  _quants, _sys, _sysinfodb, _usedinfodb)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import Utils.Drasil
import Data.Drasil.People (olu)
import Drasil.DocLang (SRSDecl, mkDoc)

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)

srs :: Document
srs = mkDoc mkSRS (for'' titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [ [RefSec $ RefProg intro [TUnits, tsymb tableOfSymbols, TAandA]
]

si :: SystemInformation
si = SI {
  _sys         = dblpendulum,
  _kind        = Doc.srs,
  _authors     = [olu],
  _purpose     = [],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = [] :: [QDefinition],
  _datadefs    = [] :: [DataDefinition],
  _configFiles  = [],
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
symbMap = cdb ([] :: [QuantityDict]) [nw dblpendulum] ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition]) ([] :: [InstanceModel])
  ([] :: [GenDefn]) ([] :: [TheoryModel]) ([] :: [ConceptInstance])
  ([] :: [Section]) ([] :: [LabelledContent])

refDB :: ReferenceDB
refDB = rdb [] []

-- MOVE TO CONCEPTS

dblpendulum :: CI
dblpendulum = commonIdeaWithDict "dblpendulum" (pn "Double Pendulum System") "Double Pendulum" []

-- MOVE TO DATA.PEOPLE
--authorName :: Person
--authorName = person "Author" "Name"
