module Drasil.Projectile.Body where

import Language.Drasil
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, RefbyMap, ReferenceDB, SystemInformation(SI),
  TraceMap, cdb, generateRefbyMap, rdb, refdb, _authors, _concepts, _constants,
  _constraints, _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs,
  _quants, _sys, _sysinfodb, _usedinfodb)

import Drasil.DocLang (DerivationDisplay(..), DocDesc, DocSection(SSDSec), Field(..), Fields,
  InclUnits(IncludeUnits), ProblemDescription(PDProg),
  SCSSub(Assumptions, DDs, GDs, IMs, TMs), SSDSec(..), SSDSub(SSDProblem, SSDSolChSpec), SolChSpec(SCSProg),
  Verbosity(Verbose),
  generateTraceMap, generateTraceMap', goalStmtF, mkDoc, mkEnumSimpleD)

import Data.Drasil.Concepts.Documentation as Doc (assumpDom, assumption, datum, general, goalStmt, information, input_, model, output_,
  physicalSystem, problemDescription, problem,  section_,
  solutionCharacteristic, specification, srs, symbol_, system)
import Data.Drasil.Concepts.Math (angle, equation)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (collision, position, twoD)
import Data.Drasil.Concepts.Software (program)

import Data.Drasil.Quantities.Physics (acceleration, displacement, distance, time, velocity)

import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, thModel)
import Data.Drasil.People (samCrawford)
import Data.Drasil.Phrase (for'')
import Data.Drasil.SentenceStructures (foldlSent, foldlSent_, ofThe, sAnd)

import qualified Data.Map as Map

import Drasil.Projectile.Assumptions (assumptions)
import Drasil.Projectile.Concepts (concepts, projectileTitle, projectile)
import Drasil.Projectile.DataDefs (dataDefns)
import Drasil.Projectile.GenDefs (genDefns)
import Drasil.Projectile.Goals (goals)
import Drasil.Projectile.IMods (iMods)
import Drasil.Projectile.TMods (tMods)
import Drasil.Projectile.Unitals (unitalIdeas, unitalQuants)

srsDoc :: Document
srsDoc = mkDoc mkSRS (for'' titleize phrase) systInfo

mkSRS :: DocDesc
mkSRS = [
  SSDSec $
    SSDProg
      [ SSDProblem   $ PDProg  probStart projectileTitle probEnding [goalStmts]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields) tMods
        , GDs [] ([Label, Units] ++ stdFields) genDefns ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
        , IMs [EmptyS] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) iMods HideDerivation
        ]
      ]
  ]

systInfo :: SystemInformation
systInfo = SI {
  _sys         = projectileTitle,
  _kind        = Doc.srs,
  _authors     = [samCrawford],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = [] :: [QDefinition],
  _datadefs    = dataDefns,
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
symbMap = cdb (map qw [acceleration, displacement, distance, time, velocity] ++ unitalQuants)
  (nw projectileTitle : nw mass : nw twoD : map nw [angle, collision, equation, position, program] ++
    map nw [datum, general, information, input_, model, output_, physicalSystem, problemDescription,
    problem, section_, solutionCharacteristic, specification, symbol_, system] ++
    map nw [acceleration, displacement, distance, time, velocity] ++
    map nw [assumption, dataDefn, genDefn, goalStmt, inModel, thModel] ++
    map nw concepts ++ unitalIdeas)
  [assumpDom] ([] :: [UnitDefn]) label refBy
  dataDefns iMods genDefns tMods
  concIns ([] :: [Section]) ([] :: [LabelledContent])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) [assumpDom] ([] :: [UnitDefn]) label refBy
  dataDefns iMods genDefns tMods
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

-------------------------
-- Problem Description --
-------------------------

probStart :: Sentence
probStart = foldlSent [S "A", phrase system,
  S "is needed to efficiently" `sAnd` S "correctly predict the landing",
  phrase position, S "of a", phrase projectile]

probEnding :: Sentence
probEnding = foldlSent_ [S "interpret the", plural input_,
  S "to give out the", plural output_, S "which predict the landing",
  phrase position, S "of a", phrase projectile]

goalStmts :: Section
goalStmts = goalStmtF [(phrase angle `sAnd` phrase velocity) `ofThe` phrase projectile]
  (mkEnumSimpleD goals)
