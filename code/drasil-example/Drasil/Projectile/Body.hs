module Drasil.Projectile.Body where

import Language.Drasil
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, RefbyMap, ReferenceDB, SystemInformation(SI),
  TraceMap, cdb, generateRefbyMap, rdb, refdb, _authors, _concepts, _constants,
  _constraints, _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs,
  _quants, _sys, _sysinfodb, _usedinfodb)

import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import Drasil.DocLang (DocDesc, DocSection(SSDSec), Field(..), Fields,
  InclUnits(IncludeUnits),
  SCSSub(Assumptions, TMs), SSDSec(..), SSDSub(SSDSolChSpec), SolChSpec(SCSProg),
  Verbosity(Verbose),
  generateTraceMap, generateTraceMap', getSCSSub, getTraceMapFromTM, mkDoc)

import Data.Drasil.Concepts.Documentation as Doc (assumption, general, information,
  physicalSystem, problemDescription, problem, section_,
  solutionCharacteristic, specification, srs)
import Data.Drasil.Concepts.Math (equation)

import Data.Drasil.Quantities.Physics (position, time, velocity)

import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, physics, thModel)
import Data.Drasil.People (samCrawford)
import Data.Drasil.Phrase (for'')

import qualified Data.Map as Map

import Drasil.Projectile.Assumptions (assumptions)
import Drasil.Projectile.TMods (tMods)

srsDoc :: Document
srsDoc = mkDoc mkSRS (for'' titleize phrase) systInfo

mkSRS :: DocDesc
mkSRS = [
  SSDSec $
    SSDProg
      [SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields) tMods
        ]
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

theoryModels :: [TheoryModel]
theoryModels = getTraceMapFromTM $ getSCSSub mkSRS

symbMap :: ChunkDB
symbMap = cdb (map qw [position, time, velocity])
  (nw projectile : nw equation : map nw [general, information, physicalSystem, problemDescription,
    problem, section_, solutionCharacteristic, specification] ++ map nw [position, time, velocity] ++
  map nw [assumption, dataDefn, genDefn, inModel, thModel])
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) label refBy
  ([] :: [DataDefinition]) ([] :: [InstanceModel]) ([] :: [GenDefn]) theoryModels
  concIns ([] :: [Section]) ([] :: [LabelledContent])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk]) ([] :: [UnitDefn]) label refBy
  ([] :: [DataDefinition]) ([] :: [InstanceModel]) ([] :: [GenDefn]) theoryModels
  concIns ([] :: [Section]) ([] :: [LabelledContent])

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

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
