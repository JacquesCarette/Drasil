module Drasil.Projectile.Body (printSetting, si, srs) where

import Language.Drasil hiding (Symbol(..), Vector)
import Language.Drasil.Code (relToQD)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _concepts, _constants, _constraints, _datadefs,
  _definitions, _defSequence, _inputs, _kind, _outputs, _quants, _sys,
  _sysinfodb, _usedinfodb)
import Utils.Drasil

import Drasil.DocLang (AuxConstntSec(AuxConsProg),
  DerivationDisplay(ShowDerivation),
  DocSection(AuxConstntSec, Bibliography, IntroSec, RefSec, ReqrmntSec, SSDSec, TraceabilitySec),
  Emphasis(Bold), Field(..), Fields, InclUnits(IncludeUnits),
  IntroSec(IntroProg), IntroSub(IScope), ProblemDescription(PDProg), PDSub(..),
  RefSec(..), RefTab(..), ReqrmntSec(..), ReqsSub(..), SCSSub(..), SRSDecl,
  SSDSec(..), SSDSub(SSDProblem, SSDSolChSpec), SolChSpec(SCSProg),
  TConvention(..), TSIntro(..), TraceabilitySec(TraceabilityProg),
  Verbosity(Verbose), intro, mkDoc, traceMatStandard, tsymb)

import Data.Drasil.Concepts.Computation (inParam)
import Data.Drasil.Concepts.Documentation (analysis, doccon, doccon', physics,
  problem, srsDomains)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Math (cartesian, mathcon)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (constAccel, gravity, physicCon, physicCon',
  rectilinear, twoD)
import Data.Drasil.Concepts.Software (errMsg, program)

import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.Physics (iVel, physicscon)

import Data.Drasil.People (brooks, samCrawford, spencerSmith)
import Data.Drasil.SI_Units (metre, radian, second)

import Drasil.Projectile.Assumptions (assumptions)
import Drasil.Projectile.Concepts (concepts, projectileTitle, landingPos,
  launcher, projectile, target)
import Drasil.Projectile.DataDefs (dataDefs)
import Drasil.Projectile.Figures (figLaunch)
import Drasil.Projectile.GenDefs (genDefns)
import Drasil.Projectile.Goals (goals)
import Drasil.Projectile.IMods (iMods)
import Drasil.Projectile.References (citations)
import Drasil.Projectile.Requirements (funcReqs, inputParamsTable, nonfuncReqs)
import Drasil.Projectile.TMods (tMods)
import Drasil.Projectile.Unitals (acronyms, constants, constrained, inConstraints,
  inputs, launAngle, outConstraints, outputs, symbols, unitalIdeas, unitalQuants)

srs :: Document
srs = mkDoc mkSRS (for'' titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [
  RefSec $
    RefProg intro
      [ TUnits
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits]
      , TAandA
      ],
  IntroSec $
    IntroProg justification (phrase projectileTitle)
      [ IScope scope ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg prob []
        [ TermsAndDefs Nothing terms
        , PhySysDesc projectileTitle physSystParts figLaunch []
        , Goals [(phrase iVel +:+ S "vector") `ofThe` phrase projectile]]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inConstraints
        , CorrSolnPpties outConstraints []
        ]
      ],
  ReqrmntSec $
    ReqsProg
      [ FReqsSub [inputParamsTable]
      , NonFReqsSub
      ],
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $
    AuxConsProg projectileTitle constants,
  Bibliography
  ]

justification, scope :: Sentence
justification = foldlSent [atStart projectile, S "motion is a common" +:+.
  (phrase problem `sIn` phrase physics), S "Therefore, it is useful to have a",
  phrase program, S "to solve and model these types of" +:+. plural problem,
  S "The", phrase program, S "documented here is called", phrase projectileTitle]
scope = foldlSent_ [S "the", phrase analysis `sOf` S "a", phrase twoD,
  sParen (getAcc twoD), phrase projectile, S "motion", phrase problem, S "with",
  phrase constAccel]

si :: SystemInformation
si = SI {
  _sys         = projectileTitle,
  _kind        = Doc.srs,
  _authors     = [samCrawford, brooks, spencerSmith],
  _quants      = symbols,
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = map (relToQD symbMap) iMods ++
                 map (relToQD symbMap) genDefns,
  _datadefs    = dataDefs,
  _inputs      = inputs,
  _outputs     = outputs,
  _defSequence = [] :: [Block QDefinition],
  _constraints = map cnstrw constrained,
  _constants   = constants,
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
  (map unitWrapper [metre, radian, second]) dataDefs iMods genDefns tMods concIns [] []

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (nw pi_ : map nw acronyms ++ map nw symbols)
  (cw pi_ : srsDomains) ([] :: [UnitDefn]) [] [] [] [] [] [] []

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

refDB :: ReferenceDB
refDB = rdb citations concIns

concIns :: [ConceptInstance]
concIns = assumptions ++ funcReqs ++ goals ++ nonfuncReqs

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
