module Drasil.NoPCM.Body where

import Language.Drasil hiding (Symbol(..), section)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block(Parallel), ChunkDB, ReferenceDB,
  SystemInformation(SI), cdb, rdb, refdb, _authors, _concepts, _constants,
  _constraints, _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs,
  _quants, _sys, _sysinfodb, _usedinfodb, sampleData)
import Theory.Drasil (TheoryModel)
import Utils.Drasil

import Data.List ((\\))
import Data.Drasil.People (thulasi)

import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation as Doc (doccon, doccon', material_, srsDomains)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.IdeaDicts as Doc (inModel)
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Math (mathcon, mathcon', ode)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty, physicalcon)
import Data.Drasil.Concepts.Physics (physicCon, physicCon')
import Data.Drasil.Concepts.Software (softwarecon)
import Data.Drasil.Concepts.Thermodynamics (heatCapSpec, htFlux, phaseChange,
  temp, thermalAnalysis, thermalConduction, thermocon)

import qualified Data.Drasil.Quantities.Thermodynamics as QT (temp,
  heatCapSpec, htFlux, sensHeat)

import Data.Drasil.Quantities.Math (gradient, pi_, surface, uNormalVect)
import Data.Drasil.Quantities.PhysicalProperties (vol, mass, density)
import Data.Drasil.Quantities.Physics (time, energy, physicscon)

import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.SI_Units (metre, kilogram, second, centigrade, joule, watt,
  fundamentals, derived)

import qualified Drasil.DocLang.SRS as SRS (inModel)
import Drasil.DocLang (AuxConstntSec(AuxConsProg), DerivationDisplay(..),
  DocSection(..), Field(..), Fields, GSDSec(..), GSDSub(..), InclUnits(IncludeUnits),
  IntroSec(IntroProg), IntroSub(IOrgSec, IScope, IChar, IPurpose), Literature(Lit, Doc'),
  PDSub(..), ProblemDescription(PDProg), RefSec(RefProg), RefTab(TAandA, TUnits),
  ReqrmntSec(..), ReqsSub(..), SCSSub(..), SolChSpec(..), SRSDecl, SSDSec(..),
  SSDSub(..), TraceabilitySec(TraceabilityProg), Verbosity(Verbose), 
  TSIntro(SymbOrder, SymbConvention, TSPurpose, VectorUnits), intro, mkDoc,
  tsymb, traceMatStandard)

-- Since NoPCM is a simplified version of SWHS, the file is to be built off
-- of the SWHS libraries.  If the source for something cannot be found in
-- NoPCM, check SWHS.
import Drasil.SWHS.Body (charsOfReader, dataContMid, introEnd, introStart,
  orgDocIntro, physSyst1, physSyst2, purpDoc, sysCntxtDesc, sysCntxtFig,
  systContRespBullets, sysCntxtRespIntro, userChars)
import Drasil.SWHS.Changes (likeChgTCVOD, likeChgTCVOL, likeChgTLH)
import Drasil.SWHS.Concepts (acronyms, coil, progName, sWHT, tank, transient, water, con)
import Drasil.SWHS.Requirements (nfRequirements)
import Drasil.SWHS.TMods (PhaseChange(Liquid), consThermE, nwtnCooling, sensHtETemplate)
import Drasil.SWHS.Unitals (coilSAMax, deltaT, htFluxC, htFluxIn, 
  htFluxOut, htCapL, htTransCoeff, inSA, outSA, tankVol, tau, tauW, tempEnv, 
  tempW, thFluxVect, volHtGen, watE, wMass, wVol, unitalChuncks, absTol, relTol)

import Drasil.NoPCM.Assumptions
import Drasil.NoPCM.Changes (likelyChgs, unlikelyChgs)
import Drasil.NoPCM.DataDefs (qDefs)
import qualified Drasil.NoPCM.DataDefs as NoPCM (dataDefs)
import Drasil.NoPCM.Definitions (srsSWHS, htTrans)
import Drasil.NoPCM.GenDefs (genDefs)
import Drasil.NoPCM.Goals (goals)
import Drasil.NoPCM.IMods (eBalanceOnWtr, instModIntro)
import qualified Drasil.NoPCM.IMods as NoPCM (iMods)
import Drasil.NoPCM.Requirements (funcReqs, inputInitQuantsTable)
import Drasil.NoPCM.References (citations)
import Drasil.NoPCM.Unitals (inputs, constrained, unconstrained, 
  specParamValList)

srs :: Document
srs = mkDoc mkSRS for si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

resourcePath :: String
resourcePath = "../../../datafiles/NoPCM/"

-- This defines the standard concepts used throughout the document
units :: [UnitDefn]
units = map unitWrapper [metre, kilogram, second] ++ map unitWrapper [centigrade, joule, watt]
-- This contains the list of symbols used throughout the document
symbols :: [DefinedQuantityDict]
symbols = pi_ : map dqdWr concepts ++ map dqdWr constrained
 ++ map dqdWr [tempW, watE]
 ++ [gradient, uNormalVect] ++ map dqdWr [surface]
  
symbolsAll :: [QuantityDict] --FIXME: Why is PCM (swhsSymbolsAll) here?
                               --Can't generate without SWHS-specific symbols like pcmHTC and pcmSA
                               --FOUND LOC OF ERROR: Instance Models
symbolsAll = map qw symbols ++ map qw specParamValList ++ 
  map qw [coilSAMax] ++ map qw [tauW] ++ map qw [absTol, relTol]

concepts :: [UnitaryConceptDict]
concepts = map ucw [density, tau, inSA, outSA,
  htCapL, QT.htFlux, htFluxIn, htFluxOut, volHtGen,
  htTransCoeff, mass, tankVol, QT.temp, QT.heatCapSpec,
  deltaT, tempEnv, thFluxVect, time, htFluxC,
  vol, wMass, wVol, tauW, QT.sensHeat]

-------------------
--INPUT INFORMATION
-------------------

--------------------------------
--Section 1 : REFERENCE MATERIAL
--------------------------------
  
mkSRS :: SRSDecl
mkSRS = [RefSec $ RefProg intro
  [TUnits,
  tsymb [TSPurpose, SymbConvention [Lit $ nw htTrans, Doc' $ nw progName], SymbOrder, VectorUnits],
  TAandA],
  IntroSec $
    IntroProg (introStart +:+ introStartNoPCM) (introEnd (plural progName) progName)
    [ IPurpose $ purpDoc (phrase progName) progName
    , IScope scope
    , IChar [] charsOfReader []
    , IOrgSec orgDocIntro inModel (SRS.inModel [] []) orgDocEnd
    ],
  GSDSec $
    GSDProg
      [ SysCntxt [sysCntxtDesc progName, LlC sysCntxtFig, sysCntxtRespIntro progName, systContRespBullets]
      , UsrChars [userChars progName]
      , SystCons [] []
      ],
  SSDSec $
    SSDProg
    [ SSDProblem $ PDProg probDescIntro []
      [ TermsAndDefs Nothing terms
      , PhySysDesc progName physSystParts figTank []
      , Goals goalInputs]
    , SSDSolChSpec $ SCSProg
      [ Assumptions
      , TMs [] (Label : stdFields)
      , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
      , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
      , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
      , Constraints dataContMid constrained
      , CorrSolnPpties dataConstListOut []
      ]
    ],
  ReqrmntSec $ ReqsProg [
    FReqsSub [inputInitQuantsTable],
    NonFReqsSub
  ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $ AuxConsProg progName specParamValList,
  Bibliography]

concIns :: [ConceptInstance]
concIns = goals ++ funcReqs ++ nfRequirements ++ assumptions ++
 [likeChgTCVOD, likeChgTCVOL] ++ likelyChgs ++ [likeChgTLH] ++ unlikelyChgs

labCon :: [LabelledContent]
labCon = [inputInitQuantsTable]

section :: [Section]
section = extractSection srs

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

si :: SystemInformation
si = SI {
  _sys = srsSWHS,
  _kind = Doc.srs,
  _authors = [thulasi],
  -- FIXME: Everything after (and including) \\ should be removed when
  -- #1658 is resolved. Basically, _quants is used here, but neither tankVol
  -- or tau appear in the document and thus should not be displayed.
  _quants = (map qw unconstrained ++ map qw symbolsAll) \\ map qw [tankVol, tau],
  _concepts = symbols,
  _definitions = [],
  _datadefs = NoPCM.dataDefs,
  _inputs = inputs ++ map qw [tempW, watE], --inputs ++ outputs?
  _outputs = map qw [tempW, watE],     --outputs
  _defSequence = [(\x -> Parallel (head x) (tail x)) qDefs],
  _constraints = map cnstrw constrained ++ map cnstrw [tempW, watE],        --constrained
  _constants = specParamValList,
  _sysinfodb = symbMap,
  _usedinfodb = usedDB,
   refdb = refDB,
   sampleData = "../../datafiles/NoPCM/sampleInput.txt"
}

refDB :: ReferenceDB
refDB = rdb citations concIns

symbMap :: ChunkDB
symbMap = cdb symbolsAll (map nw symbols ++ map nw acronyms ++ map nw thermocon
  ++ map nw physicscon ++ map nw doccon ++ map nw softwarecon ++ map nw doccon' ++ map nw con
  ++ map nw prodtcon ++ map nw physicCon ++ map nw physicCon' ++ map nw mathcon ++ map nw mathcon'
  ++ map nw specParamValList ++ map nw fundamentals ++ map nw educon ++ map nw derived 
  ++ map nw physicalcon ++ map nw unitalChuncks ++ [nw srsSWHS, nw algorithm, nw htTrans]
  ++ map nw [absTol, relTol] ++ [nw materialProprty])
  (map cw symbols ++ srsDomains) units NoPCM.dataDefs NoPCM.iMods genDefs
  tMods concIns section labCon

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw symbols ++ map nw acronyms)
 ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] []

--------------------------
--Section 2 : INTRODUCTION
--------------------------

introStartNoPCM :: Sentence
introStartNoPCM = atStart' progName +:+ S "provide a novel way of storing" +:+. phrase energy

-----------------------------------
--Section 2.1 : PURPOSE OF DOCUMENT
-----------------------------------

-------------------------------------
--Section 2.2 : SCOPE OF REQUIREMENTS
-------------------------------------

scope :: Sentence
scope = phrase thermalAnalysis `sOf` S "a single" +:+ phrase sWHT

--------------------------------------------------
--Section 2.3 : CHARACTERISTICS Of INTENDED READER
--------------------------------------------------
          
---------------------------------------
--Section 2.4: ORGANIZATION OF DOCUMENT
---------------------------------------

orgDocEnd :: Sentence
orgDocEnd = foldlSent_ [S "The", phrase inModel,
  S "to be solved is referred to as" +:+. makeRef2S eBalanceOnWtr,
  S "The", phrase inModel, S "provides the", titleize ode,
  sParen (short ode), S "that models the" +:+. phrase progName,
  short progName, S "solves this", short ode]

----------------------------------------
--Section 3 : GENERAL SYSTEM DESCRIPTION
----------------------------------------

--ALL OF THIS SECTION IS NOW PULLED FROM SWHS

--TODO: If/when system constraints recieves any content, add s3_3_intro

------------------------------
--Section 3.1 : SYSTEM CONTEXT
------------------------------
  
------------------------------------
--Section 3.2 : USER CHARACTERISTICS
------------------------------------

----------------------------------
--Section 3.3 : SYSTEM CONSTRAINTS
----------------------------------

--s3_3_intro = Paragraph $ EmptyS

--TODO: Placeholder value until content can be added

-----------------------------------------
--Section 4 : SPECIFIC SYSTEM DESCRIPTION
-----------------------------------------

--TODO: finish filling in the subsections

-----------------------------------
--Section 4.1 : PROBLEM DESCRIPTION
-----------------------------------

probDescIntro :: Sentence
probDescIntro = foldlSent_ [S "investigate the heating" `sOf` phrase water, S "in a", phrase sWHT]

terms :: [ConceptChunk]
terms = [htFlux, heatCapSpec, thermalConduction, transient]
  
figTank :: LabelledContent
figTank = llcc (makeFigRef "Tank") $ fig (atStart sWHT `sC` S "with" +:+ phrase htFlux +:+
  S "from" +:+ phrase coil `sOf` ch htFluxC)
  $ resourcePath ++ "TankWaterOnly.png"

physSystParts :: [Sentence]
physSystParts = map foldlSent_ [physSyst1 tank water, physSyst2 coil tank htFluxC]

goalInputs :: [Sentence]
goalInputs = [phrase temp `ofThe` phrase coil,
  S "the initial" +:+ phrase tempW, S "the" +:+ plural materialProprty]

------------------------------------------------------
--Section 4.2 : SOLUTION CHARACTERISTICS SPECIFICATION
------------------------------------------------------

tMods :: [TheoryModel]
tMods = [consThermE, sensHtE, nwtnCooling]

sensHtE :: TheoryModel
sensHtE = sensHtETemplate Liquid sensHtEdesc

sensHtEdesc :: Sentence
sensHtEdesc = foldlSent [ch QT.sensHeat, S "occurs as long as the", phrase material_, S "does not reach a",
  phrase temp, S "where a", phrase phaseChange, S "occurs" `sC` S "as assumed in", makeRef2S assumpWAL]

--TODO: Implement physical properties of a substance

dataConstListOut :: [ConstrConcept]
dataConstListOut = [tempW, watE]

--------------------------
--Section 5 : REQUIREMENTS
--------------------------

-- in Requirements.hs

---------------------------------------
--Section 5.1 : FUNCTIONAL REQUIREMENTS
---------------------------------------

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

----------------------------
--Section 6 : LIKELY CHANGES
----------------------------

-------------------------------
--Section 6b : UNLIKELY CHANGES
-------------------------------

----------------------------------------------
--Section 7:  TRACEABILITY MATRICES AND GRAPHS
----------------------------------------------

------------------------
-- Traceabilty Graphs --
------------------------

-- Using the SWHS graphs as place holders until ones can be generated for NoPCM 

------------------------------------------
--Section 8: SPECIFICATION PARAMETER VALUE
------------------------------------------

------------
--REFERENCES
------------
