module Drasil.Projectile.Body where

import Language.Drasil
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, RefbyMap, ReferenceDB, SystemInformation(SI),
  TraceMap, cdb, generateRefbyMap, rdb, refdb, _authors, _concepts, _constants,
  _constraints, _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs,
  _quants, _sys, _sysinfodb, _usedinfodb)

import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import Drasil.DocLang (DocDesc, DocSection(SSDSec), SCSSub(Assumptions),
  SSDSec(..), SSDSub(SSDSolChSpec), SolChSpec(SCSProg), generateTraceMap,
  generateTraceMap', mkDoc)

import Data.Drasil.Concepts.Documentation as Doc (assumption, information,
  physicalSystem, problemDescription, problem, section_,
  solutionCharacteristic, specification, srs)

import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, physics, thModel)
import Data.Drasil.People (samCrawford)
import Data.Drasil.Phrase (for'')

import qualified Data.Map as Map

import Drasil.Projectile.Assumptions (assumptions)

srsDoc :: Document
srsDoc = mkDoc mkSRS (for'' titleize phrase) systInfo

mkSRS :: DocDesc
mkSRS = [
  SSDSec $
    SSDProg
      [SSDSolChSpec $ SCSProg
        [Assumptions]
      ]
  ]

systInfo :: SystemInformation
systInfo = SI {
  _sys         = projectile,
  _kind        = Doc.srs,
  _authors     = [samCrawford],
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
symbMap = cdb ([] :: [QuantityDict])
  (nw projectile : map nw [information, physicalSystem, problemDescription,
    problem, section_, solutionCharacteristic, specification] ++
  map nw [assumption, dataDefn, genDefn, inModel, thModel])
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) label refBy
  ([] :: [DataDefinition]) ([] :: [InstanceModel]) ([] :: [GenDefn]) ([] :: [TheoryModel])
  concIns ([] :: [Section]) ([] :: [LabelledContent])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk]) ([] :: [UnitDefn]) label refBy
  ([] :: [DataDefinition]) ([] :: [InstanceModel]) ([] :: [GenDefn]) ([] :: [TheoryModel])
  concIns ([] :: [Section]) ([] :: [LabelledContent])

refDB :: ReferenceDB
refDB = rdb [] concIns

concIns :: [ConceptInstance]
concIns = assumptions

label :: TraceMap
label = Map.union (generateTraceMap mkSRS) $ generateTraceMap' concIns
 
refBy :: RefbyMap
refBy = generateRefbyMap label

printSetting :: PrintingInformation
printSetting = PI symbMap defaultConfiguration

-- MOVE TO CONCEPTS
projectile :: CI -- Title
projectile = commonIdeaWithDict "projectile" (pn "Projectile") "Projectile" [physics]
