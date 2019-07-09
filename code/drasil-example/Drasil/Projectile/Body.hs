module Drasil.Projectile.Body where

import Language.Drasil hiding (Vector)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, RefbyMap, ReferenceDB, SystemInformation(SI),
  TraceMap, cdb, collectUnits, generateRefbyMap, rdb, refdb, _authors, _concepts,
  _constants, _constraints, _datadefs, _definitions, _defSequence, _inputs, _kind,
  _outputs, _quants, _sys, _sysinfodb, _usedinfodb)
import Utils.Drasil

import Drasil.DocLang (AuxConstntSec(AuxConsProg),
  DerivationDisplay(ShowDerivation), DocDesc,
  DocSection(AuxConstntSec, Bibliography, IntroSec, RefSec, ReqrmntSec, SSDSec, TraceabilitySec),
  Emphasis(Bold), Field(..), Fields, InclUnits(IncludeUnits),
  IntroSec(IntroProg), IntroSub(IScope), ProblemDescription(PDProg), PDSub(..),
  RefSec(..), RefTab(..), ReqrmntSec(..), ReqsSub(..), SCSSub(..), SSDSec(..),
  SSDSub(SSDProblem, SSDSolChSpec), SolChSpec(SCSProg), TConvention(..),
  TSIntro(..), TraceabilitySec(TraceabilityProg), Verbosity(Verbose),
  dataConstraintUncertainty, generateTraceMap, generateTraceMap', inDataConstTbl,
  intro, mkDoc, outDataConstTbl, traceMatStandard, tsymb)

import Data.Drasil.Concepts.Computation (inParam)
import Data.Drasil.Concepts.Documentation (analysis, doccon, doccon', physics,
  problem, srsDomains, srs)
import Data.Drasil.Concepts.Math (cartesian, mathcon)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (constAccel, gravity, physicCon, physicCon',
  rectilinear, twoD)
import Data.Drasil.Concepts.Software (errMsg, program)

import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.Physics (iVel, physicscon)

import Data.Drasil.People (brooks, samCrawford, spencerSmith)
import Data.Drasil.SI_Units (metre, radian, second)

import qualified Data.Map as Map

import Drasil.Projectile.Assumptions (assumptions)
import Drasil.Projectile.Concepts (concepts, projectileTitle, landingPos,
  launcher, projectile, target)
import Drasil.Projectile.DataDefs (dataDefns)
import Drasil.Projectile.Figures (figLaunch)
import Drasil.Projectile.GenDefs (genDefns)
import Drasil.Projectile.Goals (goals)
import Drasil.Projectile.IMods (iMods)
import Drasil.Projectile.References (citations)
import Drasil.Projectile.Requirements (funcReqs, inputParamsTable, nonfuncReqs)
import Drasil.Projectile.TMods (tMods)
import Drasil.Projectile.Unitals (acronyms, constants, inConstraints,
  launAngle, outConstraints, symbols, unitalIdeas, unitalQuants)

srsDoc :: Document
srsDoc = mkDoc mkSRS (for'' titleize phrase) systInfo

mkSRS :: DocDesc
mkSRS = [
  RefSec $
    RefProg intro
      [ TUnits
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits]
      , TAandA
      ],
  IntroSec $
    IntroProg justification (phrase projectileTitle)
      [ IScope scope1 scope2
      ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg prob []
        [ TermsAndDefs Nothing terms
        , PhySysDesc projectileTitle physSystParts figLaunch []
        , Goals [(phrase iVel +:+ S "vector") `ofThe` phrase projectile] goals]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields) tMods
        , GDs [] ([Label, Units] ++ stdFields) genDefns ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) iMods ShowDerivation
        , Constraints EmptyS dataConstraintUncertainty EmptyS [inDataCons]
        , CorrSolnPpties propsDeriv
        ]
      ],
  ReqrmntSec $
    ReqsProg
      [ FReqsSub funcReqs [inputParamsTable]
      , NonFReqsSub nonfuncReqs
      ],
  TraceabilitySec $
    TraceabilityProg
      (map fst traceMats) (map (foldlList Comma List . snd) traceMats) (map (LlC . fst) traceMats) [],
  AuxConstntSec $
    AuxConsProg projectileTitle constants,
  Bibliography
  ]

justification, scope1, scope2 :: Sentence
justification = foldlSent [atStart projectile, S "motion is a common" +:+.
  (phrase problem `sIn` phrase physics), S "Therefore, it is useful to have a",
  phrase program, S "to solve and model these types of" +:+. plural problem,
  S "The", phrase program, S "documented here is called", phrase projectileTitle]
scope1 = foldlSent_ [S "the", phrase analysis `sOf` S "a", phrase twoD,
  sParen (getAcc twoD), phrase projectile, S "motion", phrase problem, S "with",
  phrase constAccel]
scope2 = foldlSent_ [S "determines if the", phrase projectile, S "hits the", phrase target]

systInfo :: SystemInformation
systInfo = SI {
  _sys         = projectileTitle,
  _kind        = srs,
  _authors     = [samCrawford, brooks, spencerSmith],
  _quants      = symbols,
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
symbMap = cdb (qw pi_ : map qw physicscon ++ unitalQuants ++ symbols)
  (nw projectileTitle : nw mass : nw inParam : [nw errMsg, nw program] ++
    map nw doccon ++ map nw doccon' ++ map nw physicCon ++ map nw physicCon' ++
    map nw physicscon ++ map nw mathcon ++ concepts ++ unitalIdeas ++
    map nw acronyms ++ map nw symbols ++ map nw [metre, radian, second]) (cw pi_ : srsDomains)
  (map unitWrapper [metre, radian, second]) label refBy dataDefns iMods genDefns tMods
  concIns ([] :: [Section]) ([] :: [LabelledContent])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (nw pi_ : map nw acronyms ++ map nw symbols ++ map nw units)
  (cw pi_ : srsDomains) units label refBy dataDefns iMods genDefns tMods
  concIns ([] :: [Section]) ([] :: [LabelledContent])

units :: [UnitDefn]
units = collectUnits symbMap symbols 

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

refDB :: ReferenceDB
refDB = rdb citations concIns

concIns :: [ConceptInstance]
concIns = assumptions ++ funcReqs ++ goals ++ nonfuncReqs

label :: TraceMap
label = Map.union (generateTraceMap mkSRS) $ generateTraceMap' concIns
 
refBy :: RefbyMap
refBy = generateRefbyMap label

printSetting :: PrintingInformation
printSetting = PI symbMap defaultConfiguration

-------------------------
-- Problem Description --
-------------------------

prob :: Sentence
prob = foldlSent_ [S "efficiently" `sAnd` S "correctly predict the",
  phrase landingPos, S "of a", phrase projectile]

---------------------------------
-- Terminology and Definitions --
---------------------------------

terms :: [ConceptChunk]
terms = [launcher, projectile, target, gravity, cartesian, rectilinear]

---------------------------------
-- Physical System Description --
---------------------------------

physSystParts :: [Sentence]
physSystParts = map foldlSent [
  [S "The", phrase launcher],
  [S "The", phrase projectile, sParen (S "with" +:+ getTandS iVel `sAnd` getTandS launAngle)],
  [S "The", phrase target]]

----------------------
-- Data Constraints --
----------------------

inDataCons, outDataCons :: LabelledContent
inDataCons  = inDataConstTbl  inConstraints
outDataCons = outDataConstTbl outConstraints

------------------------------------
-- Properties of Correct Solution --
------------------------------------

propsDeriv :: [Contents]
propsDeriv = [LlC outDataCons]

--------------------------
-- Traceabilty Matrices --
--------------------------

traceMats :: [(LabelledContent, [Sentence])]
traceMats = traceMatStandard systInfo
