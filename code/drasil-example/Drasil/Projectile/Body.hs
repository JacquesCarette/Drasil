module Drasil.Projectile.Body where

import Language.Drasil
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, RefbyMap, ReferenceDB, SystemInformation(SI),
  TraceMap, cdb, generateRefbyMap, rdb, refdb, _authors, _concepts, _constants,
  _constraints, _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs,
  _quants, _sys, _sysinfodb, _usedinfodb)
import Utils.Drasil

import Drasil.DocLang (DerivationDisplay(ShowDerivation), DocDesc,
  DocSection(RefSec, ReqrmntSec, SSDSec), Field(..), Fields, InclUnits(IncludeUnits),
  ProblemDescription(PDProg), RefSec(..), RefTab(TUnits, TAandA), ReqrmntSec(..), ReqsSub(..),
  SCSSub(Assumptions, Constraints, CorrSolnPpties, DDs, GDs, IMs, TMs),
  SSDSec(..), SSDSub(SSDProblem, SSDSolChSpec), SolChSpec(SCSProg), Verbosity(Verbose),
  dataConstraintUncertainty, generateTraceMap, generateTraceMap', goalStmtF,
  inDataConstTbl, intro, mkDoc, mkEnumSimpleD, outDataConstTbl)--, tsymb'')

import Data.Drasil.Concepts.Computation (inParam)
import Data.Drasil.Concepts.Documentation as Doc (assumpDom, doccon, doccon',
  funcReqDom, input_, nonFuncReqDom, output_, srs, system)
import Data.Drasil.Concepts.Math (angle, mathcon)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (physicCon, position, speed, twoD)
import Data.Drasil.Concepts.Software (errMsg, program)

import Data.Drasil.Quantities.Physics (physicscon)

import Data.Drasil.People (brooks, samCrawford, spencerSmith)

import qualified Data.Map as Map

import Drasil.Projectile.Assumptions (assumptions)
import Drasil.Projectile.Concepts (concepts, projectileTitle, projectile)
import Drasil.Projectile.DataDefs (dataDefns)
import Drasil.Projectile.GenDefs (genDefns)
import Drasil.Projectile.Goals (goals)
import Drasil.Projectile.IMods (iMods)
import Drasil.Projectile.Requirements (funcReqs, inputParamsTable,
  nonfuncReqs, propsDeriv)
import Drasil.Projectile.TMods (tMods)
import Drasil.Projectile.Unitals (acronyms, inConstraints, outConstraints,
  unitalIdeas, unitalQuants)

srsDoc :: Document
srsDoc = mkDoc mkSRS (for'' titleize phrase) systInfo

mkSRS :: DocDesc
mkSRS = [
  RefSec $
    RefProg intro
      [ TUnits
     -- , tsymb'' tableOfSymbIntro TAD
      , TAandA
      ],
  SSDSec $
    SSDProg
      [ SSDProblem   $ PDProg  probStart projectileTitle probEnding [goalStmts]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields) tMods
        , GDs [] ([Label, Units] ++ stdFields) genDefns ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
        , IMs [EmptyS] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) iMods ShowDerivation
        , Constraints EmptyS dataConstraintUncertainty EmptyS
                      {-(foldlSent [makeRef2S $ valsOfAuxCons [] [],
                      S "gives", plural value `ofThe` S "specification",
                      plural parameter, S "used in", makeRef2S inDataCons])-}
                      [inDataCons, outDataCons]
        , CorrSolnPpties propsDeriv
        ]
      ],
  ReqrmntSec $
    ReqsProg
      [ FReqsSub funcReqs [inputParamsTable]
      , NonFReqsSub nonfuncReqs
      ]
  ]

systInfo :: SystemInformation
systInfo = SI {
  _sys         = projectileTitle,
  _kind        = Doc.srs,
  _authors     = [samCrawford, brooks, spencerSmith],
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
symbMap = cdb (map qw physicscon ++ unitalQuants)
  (nw projectileTitle : nw mass : nw twoD : nw inParam : [nw errMsg, nw program] ++
    map nw doccon ++ map nw doccon' ++ map nw physicscon ++ map nw physicCon ++
    map nw mathcon ++ concepts ++ unitalIdeas ++ map nw acronyms)
  [assumpDom, funcReqDom, nonFuncReqDom] ([] :: [UnitDefn]) label refBy
  dataDefns iMods genDefns tMods
  concIns ([] :: [Section]) ([] :: [LabelledContent])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms) [assumpDom, funcReqDom, nonFuncReqDom]
  ([] :: [UnitDefn]) label refBy dataDefns iMods genDefns tMods
  concIns ([] :: [Section]) ([] :: [LabelledContent])

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

refDB :: ReferenceDB
refDB = rdb [] concIns

concIns :: [ConceptInstance]
concIns = assumptions ++ funcReqs ++ nonfuncReqs

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
goalStmts = goalStmtF [(phrase angle `sAnd` phrase speed) `ofThe` phrase projectile]
  (mkEnumSimpleD goals)

----------------------
-- Data Constraints --
----------------------

inDataCons, outDataCons :: LabelledContent
inDataCons  = inDataConstTbl  inConstraints
outDataCons = outDataConstTbl outConstraints
