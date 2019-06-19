module Drasil.Projectile.Body where

import Language.Drasil hiding (Vector)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, RefbyMap, ReferenceDB, SystemInformation(SI),
  TraceMap, cdb, collectUnits, generateRefbyMap, rdb, refdb, _authors, _concepts,
  _constants, _constraints, _datadefs, _definitions, _defSequence, _inputs, _kind,
  _outputs, _quants, _sys, _sysinfodb, _usedinfodb)
import Utils.Drasil
import Control.Lens ((^.))

import Drasil.DocLang (AuxConstntSec(AuxConsProg),
  DerivationDisplay(ShowDerivation), DocDesc,
  DocSection(AuxConstntSec, IntroSec, RefSec, ReqrmntSec, SSDSec, TraceabilitySec),
  Emphasis(Bold), Field(..), Fields, InclUnits(IncludeUnits),
  IntroSec(IntroProg), IntroSub(IScope), ProblemDescription(PDProg),
  RefSec(..), RefTab(..), ReqrmntSec(..), ReqsSub(..), SCSSub(..), SSDSec(..),
  SSDSub(SSDProblem, SSDSolChSpec), SolChSpec(SCSProg), TConvention(..),
  TSIntro(..), TraceabilitySec(TraceabilityProg), Verbosity(Verbose),
  dataConstraintUncertainty, generateTraceMap, generateTraceMap', goalStmtF,
  inDataConstTbl, intro, mkDoc, mkEnumSimpleD, outDataConstTbl, physSystDesc,
  physSystDescriptionLabel, termDefnF, traceMatStandard, tsymb)

import Data.Drasil.Concepts.Computation (inParam)
import Data.Drasil.Concepts.Documentation (analysis, doccon, doccon', physics,
  physSyst, problem, srsDomains, srs)
import Data.Drasil.Concepts.Math (cartesian, mathcon, rightHand)
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
import Drasil.Projectile.Requirements (funcReqs, inputParamsTable,
  nonfuncReqs, propsDeriv)
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
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder]
      , TAandA
      ],
  IntroSec $
    IntroProg justification (phrase projectileTitle)
      [ IScope scope1 scope2
      ],
  SSDSec $
    SSDProg
      [ SSDProblem   $ PDProg prob
        [termsAndDefs, physSystDescription, goalStmts]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields) tMods
        , GDs [] ([Label, Units] ++ stdFields) genDefns ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) iMods ShowDerivation
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
      ],
  TraceabilitySec $
    TraceabilityProg
      (map fst traceMats) (map (foldlList Comma List . snd) traceMats) (map (LlC . fst) traceMats) [],
  AuxConstntSec $
    AuxConsProg projectileTitle constants
  ]

justification, scope1, scope2 :: Sentence
justification = foldlSent [atStart projectile, S "motion" `sIs` S "an incredibly common" +:+.
  (phrase problem `sIn` phrase physics), S "Therefore" `sC` S "it" `sIs` S "useful to have a",
  phrase program, S "to solve and model these types" `sOf` plural problem]
scope1 = foldlSent_ [S "the", phrase analysis `sOf` S "a", phrase twoD, phrase projectile,
  S "motion", phrase problem, S "with", phrase constAccel]
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
refDB = rdb [] concIns

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

termsAndDefs :: Section
termsAndDefs = termDefnF Nothing [termsAndDefsBullets]

termsAndDefsBullets :: Contents
termsAndDefsBullets = UlC $ ulcc $ Enumeration $ Bullet $ noRefs $
  map tAndDMap [launcher, projectile, target, gravity, cartesian, rightHand, rectilinear]
  where
    tAndDMap c = Flat $ foldlSent_ [atStart c +: EmptyS, c ^. defn]

---------------------------------
-- Physical System Description --
---------------------------------

physSystDescription :: Section
physSystDescription = physSystDesc (short projectileTitle) figLaunch [physSystDescList, LlC figLaunch]

physSystDescList :: Contents
physSystDescList = LlC $ enumSimple physSystDescriptionLabel 1 (short physSyst) systDescList

systDescList :: [Sentence]
systDescList = map foldlSent [
  [S "The", phrase launcher],
  [S "The", phrase projectile, sParen (S "with" +:+ getTandS iVel `sAnd` getTandS launAngle)],
  [S "The", phrase target]]

---------------------
-- Goal Statements --
---------------------

goalStmts :: Section
goalStmts = goalStmtF [(phrase iVel +:+ S "vector") `ofThe` phrase projectile]
  (mkEnumSimpleD goals)

----------------------
-- Data Constraints --
----------------------

inDataCons, outDataCons :: LabelledContent
inDataCons  = inDataConstTbl  inConstraints
outDataCons = outDataConstTbl outConstraints

--------------------------
-- Traceabilty Matrices --
--------------------------

traceMats :: [(LabelledContent, [Sentence])]
traceMats = traceMatStandard systInfo
