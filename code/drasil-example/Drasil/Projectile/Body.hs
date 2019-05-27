module Drasil.Projectile.Body where

import Language.Drasil
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, RefbyMap, ReferenceDB, SystemInformation(SI),
  TraceMap, cdb, generateRefbyMap, rdb, refdb, _authors, _concepts, _constants,
  _constraints, _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs,
  _quants, _sys, _sysinfodb, _usedinfodb)

import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)

import Drasil.DocLang (DocDesc, DocSection(SSDSec), Field(..), Fields,
  InclUnits(IncludeUnits), ProblemDescription(PDProg),
  SCSSub(Assumptions, TMs), SSDSec(..), SSDSub(SSDProblem, SSDSolChSpec), SolChSpec(SCSProg),
  Verbosity(Verbose),
  generateTraceMap, generateTraceMap', getSCSSub, getTraceMapFromTM, goalStmtF, mkDoc, mkEnumSimpleD)

import Data.Drasil.Concepts.Documentation as Doc (assumption, general, goalStmt, information, input_, output_,
  physicalSystem, problemDescription, problem,  section_,
  solutionCharacteristic, specification, srs, system)
import Data.Drasil.Concepts.Math (angle, equation)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (collision, position, twoD)
import Data.Drasil.Concepts.Software (program)

import Data.Drasil.Quantities.Physics (acceleration, displacement, time, velocity)

import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, physics, thModel)
import Data.Drasil.People (samCrawford)
import Data.Drasil.Phrase (for'')
import Data.Drasil.SentenceStructures (foldlSent, foldlSent_, ofThe, sAnd)

import qualified Data.Map as Map

import Drasil.Projectile.Assumptions (assumptions)
import Drasil.Projectile.Goals (goals)
import Drasil.Projectile.TMods (tMods)

srsDoc :: Document
srsDoc = mkDoc mkSRS (for'' titleize phrase) systInfo

mkSRS :: DocDesc
mkSRS = [
  SSDSec $
    SSDProg
      [ SSDProblem   $ PDProg  probStart projectile probEnding [goalStmts]
      , SSDSolChSpec $ SCSProg
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
symbMap = cdb (map qw [acceleration, displacement, time, velocity])
  (nw projectile : nw mass : nw twoD : map nw [angle, collision, equation, position, program] ++ map nw [general, information, input_, output_, physicalSystem, problemDescription,
    problem, section_, solutionCharacteristic, specification, system] ++ map nw [acceleration, displacement, time, velocity] ++
  map nw [assumption, dataDefn, genDefn, goalStmt, inModel, thModel])
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

-------------------------
-- Problem Description --
-------------------------

probStart :: Sentence
probStart = foldlSent [S "A", phrase system,
  S "is needed to efficiently" `sAnd` S "correctly predict the landing",
  phrase position, S "of a projectile"]

probEnding :: Sentence
probEnding = foldlSent_ [S "interpret the", plural input_,
  S "to give out the", plural output_,
  S "which predict the landing", phrase position,
  S "of a projectile"]

goalStmts :: Section
goalStmts = goalStmtF [(phrase angle `sAnd` phrase velocity) `ofThe` S "projectile"]
  (mkEnumSimpleD goals)
