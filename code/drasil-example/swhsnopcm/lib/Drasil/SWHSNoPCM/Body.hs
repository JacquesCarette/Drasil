module Drasil.SWHSNoPCM.Body (si, srs, printSetting, noPCMODEInfo, fullSI) where

import Language.Drasil hiding (section)
import Drasil.SRSDocument
import qualified Drasil.DocLang.SRS as SRS (inModel)
import Theory.Drasil (TheoryModel)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Language.Drasil.Code (quantvar)

import Data.List ((\\))
import Data.Drasil.People (thulasi)

import Data.Drasil.Concepts.Computation (algorithm, inValue)
import Data.Drasil.Concepts.Documentation as Doc (doccon, doccon', material_, srsDomains, sysCont)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.TheoryConcepts as Doc (inModel)
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Math (mathcon, mathcon', ode)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty, physicalcon)
import Data.Drasil.Concepts.Physics (physicCon, physicCon')
import Data.Drasil.Concepts.Software (softwarecon)
import Data.Drasil.Concepts.Thermodynamics (heatCapSpec, htFlux, phaseChange,
  temp, thermalAnalysis, thermalConduction, thermocon)

import Data.Drasil.ExternalLibraries.ODELibraries (scipyODESymbols, osloSymbols,
  arrayVecDepVar, apacheODESymbols, odeintSymbols)

import qualified Data.Drasil.Quantities.Thermodynamics as QT (temp,
  heatCapSpec, htFlux, sensHeat)
import Data.Drasil.Quantities.Math (gradient, pi_, piConst, surface,
  uNormalVect)
import Data.Drasil.Quantities.PhysicalProperties (vol, mass, density)
import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.SI_Units (siUnits)

-- Since NoPCM is a simplified version of SWHS, the file is to be built off
-- of the SWHS libraries.  If the source for something cannot be found in
-- NoPCM, check SWHS.
import Drasil.SWHS.Body (charsOfReader, dataContMid, motivation,
  introStart, externalLinkRef, physSyst1, physSyst2, sysCntxtDesc,
  systContRespBullets, sysCntxtRespIntro, userChars)
import Drasil.SWHS.Changes (likeChgTCVOD, likeChgTCVOL, likeChgTLH)
import Drasil.SWHS.Concepts (acronyms, coil, sWHT, tank, transient, water, con, phsChgMtrl)
import Drasil.SWHS.Requirements (nfRequirements)
import Drasil.SWHS.TMods (PhaseChange(Liquid), consThermE, nwtnCooling, sensHtETemplate)
import Drasil.SWHS.Unitals (deltaT, htFluxC, htFluxIn,
  htFluxOut, htCapL, htTransCoeff, inSA, outSA, tankVol, tau, tauW,
  tempEnv, tempW, thFluxVect, volHtGen, watE,
  wMass, wVol, absTol, relTol)
import Drasil.SWHS.References (uriReferences)

import Drasil.SWHSNoPCM.Assumptions
import Drasil.SWHSNoPCM.Changes (likelyChgs, unlikelyChgs)
import qualified Drasil.SWHSNoPCM.DataDefs as NoPCM (dataDefs)
import Drasil.SWHSNoPCM.Definitions (srsSWHS, htTrans)
import Drasil.SWHSNoPCM.GenDefs (genDefs)
import Drasil.SWHSNoPCM.Goals (goals)
import Drasil.SWHSNoPCM.IMods (eBalanceOnWtr, instModIntro)
import Drasil.SWHSNoPCM.MetaConcepts (progName)
import qualified Drasil.SWHSNoPCM.IMods as NoPCM (iMods)
import Drasil.SWHSNoPCM.ODEs
import Drasil.SWHSNoPCM.Requirements (funcReqs, inReqDesc)
import Drasil.SWHSNoPCM.References (citations)
import Drasil.SWHSNoPCM.Unitals (inputs, constrained, unconstrained,
  specParamValList)

srs :: Document
srs = mkDoc mkSRS S.forT si

fullSI :: System
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

resourcePath :: String
resourcePath = "../../../../datafiles/swhsnopcm/"

-- This contains the list of symbols used throughout the document
symbols :: [DefinedQuantityDict]
symbols = map dqdWr concepts ++ map dqdWr constrained
 ++ map dqdWr [tempW, watE]

symbolsAll :: [QuantityDict] --FIXME: Why is PCM (swhsSymbolsAll) here?
                               --Can't generate without SWHS-specific symbols like pcmHTC and pcmSA
                               --FOUND LOC OF ERROR: Instance Models
symbolsAll = map qw [gradient, pi_, uNormalVect, dqdWr surface] ++ map qw symbols ++
  map qw symbolConcepts ++ map qw specParamValList ++ map qw [absTol, relTol] ++
  scipyODESymbols ++ osloSymbols ++ apacheODESymbols ++ odeintSymbols ++
  map qw [listToArray $ quantvar tempW, arrayVecDepVar noPCMODEInfo]

concepts :: [UnitalChunk]
concepts = map ucw [tau, inSA, outSA, htCapL, htFluxIn, htFluxOut, volHtGen,
  htTransCoeff, tankVol, deltaT, tempEnv, thFluxVect, htFluxC, wMass, wVol, tauW]

symbolConcepts :: [UnitalChunk]
symbolConcepts = map ucw [density, QT.htFlux, QT.heatCapSpec, mass, QT.sensHeat,
  QT.temp, time, vol]

-------------------
--INPUT INFORMATION
-------------------

--------------------------------
--Section 1 : REFERENCE MATERIAL
--------------------------------

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $ RefProg intro
  [TUnits,
  tsymb [TSPurpose, SymbConvention [Lit $ nw htTrans, Doc' $ nw progName], SymbOrder, VectorUnits],
  TAandA abbreviationsList],
  IntroSec $
    IntroProg (introStart +:+ introStartNoPCM) (introEnd (plural progName) progName)
    [ IPurpose $ purpDoc progName Verbose
    , IScope scope
    , IChar [] charsOfReader []
    , IOrgSec inModel (SRS.inModel [] []) orgDocEnd
    ],
  GSDSec $
    GSDProg
      [ SysCntxt [sysCntxtDesc progName, LlC sysCntxtFig, sysCntxtRespIntro progName, systContRespBullets progName]
      , UsrChars [userChars progName]
      , SystCons [] []
      ],
  SSDSec $
    SSDProg
    [ SSDProblem $ PDProg purp []
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
    FReqsSub inReqDesc [],
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

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

si :: System
si = SI {
  _sys         = srsSWHS,
  _kind        = Doc.srs,
  _authors     = [thulasi],
  _purpose     = [purp],
  _background  = [introStartNoPCM],
  _motivation  = [motivation],
  _scope       = [scope],
  -- FIXME: Everything after (and including) \\ should be removed when
  -- #1658 is resolved. Basically, _quants is used here, but 
  -- tau does not appear in the document and thus should not be displayed.
  _quants      = (map qw unconstrained ++ map qw symbolsAll) \\ [qw tau],
  _instModels  = NoPCM.iMods,
  _datadefs    = NoPCM.dataDefs,
  _configFiles = [],
  _inputs      = inputs ++ [qw watE], --inputs ++ outputs?
  _outputs     = map qw [tempW, watE],     --outputs
  _constraints = map cnstrw constrained ++ map cnstrw [tempW, watE], --constrained
  _constants   = piConst : specParamValList,
  _systemdb   = symbMap
}

purp :: Sentence
purp = foldlSent_ [S "investigate the heating" `S.of_` phraseNP (water `inA` sWHT)]

ideaDicts :: [IdeaDict]
ideaDicts =
  -- Actual IdeaDicts
  [inValue, htTrans, materialProprty] ++ prodtcon ++ doccon ++ educon ++
  -- CIs
  map nw [srsSWHS, progName, phsChgMtrl] ++ map nw doccon' ++
  map nw physicCon' ++ map nw mathcon'

conceptChunks :: [ConceptChunk]
conceptChunks =
  -- ConceptChunks
  algorithm : softwarecon ++ thermocon ++ con ++ physicCon ++ mathcon ++
  physicalcon ++ srsDomains ++
  -- DefinedQuantityDicts
  map cw symbols

symbMap :: ChunkDB
symbMap = cdb symbolsAll ideaDicts conceptChunks siUnits NoPCM.dataDefs
  NoPCM.iMods genDefs tMods concIns [] allRefs citations

abbreviationsList :: [IdeaDict]
abbreviationsList =
  -- CIs
  nw progName : map nw acronyms ++
  -- DefinedQuantityDicts
  map nw symbols

-- | Holds all references and links used in the document.
allRefs :: [Reference]
allRefs = [externalLinkRef, externalLinkRef'] ++ uriReferences

--------------------------
--Section 2 : INTRODUCTION
--------------------------

-- To get this generating properly we need to add a constructor for custom plural and capital case, see #3535
introStartNoPCM :: Sentence
introStartNoPCM = atStart' progName +:+ S "provide a novel way of storing" +:+. phrase energy

introEnd :: Sentence -> CI -> Sentence
introEnd progSent pro = foldlSent_ [progSent +:+ S "The developed program",
  S "will be referred to as", titleize pro, sParen (short pro),
  S "based on the original" `sC` S "manually created version" `S.of_` namedRef externalLinkRef' (S "SWHSNoPCM")]

externalLinkRef' :: Reference
externalLinkRef' = makeURI "SWHSNoPCM_SRSLink"
  "https://github.com/smiths/caseStudies/blob/master/CaseStudies/noPCM"
  (shortname' $ S "SWHSNoPCM_SRSLink")

-----------------------------------
--Section 2.1 : PURPOSE OF DOCUMENT
-----------------------------------
-- Purpose of Document automatically generated in IPurpose

-------------------------------------
--Section 2.2 : SCOPE OF REQUIREMENTS
-------------------------------------

scope :: Sentence
scope = phrase thermalAnalysis `S.of_` S "a single" +:+ phrase sWHT

--------------------------------------------------
--Section 2.3 : CHARACTERISTICS Of INTENDED READER
--------------------------------------------------

---------------------------------------
--Section 2.4: ORGANIZATION OF DOCUMENT
---------------------------------------

orgDocEnd :: Sentence
orgDocEnd = foldlSent_ [atStartNP (the inModel),
  S "to be solved" `S.is` S "referred to as" +:+. refS eBalanceOnWtr,
  atStartNP (the inModel), S "provides the", titleize ode,
  sParen (short ode), S "that models the" +:+. phrase progName,
  short progName, S "solves this", short ode]

----------------------------------------
--Section 3 : GENERAL SYSTEM DESCRIPTION
----------------------------------------

--ALL OF THIS SECTION IS NOW PULLED FROM SWHS (Exept System Context Figure)

--TODO: If/when system constraints recieves any content, add s3_3_intro

------------------------------
--Section 3.1 : SYSTEM CONTEXT
------------------------------

sysCntxtFig :: LabelledContent
sysCntxtFig = llcc (makeFigRef "SysCon")
  $ fig (titleize sysCont)
  $ resourcePath ++ "SystemContextFigure.png"

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

--Introduction of Problem Description section derived from purp

terms :: [ConceptChunk]
terms = [htFlux, heatCapSpec, thermalConduction, transient]

figTank :: LabelledContent
figTank = llcc (makeFigRef "Tank") $ fig (atStart sWHT `sC` S "with" +:+ phrase htFlux +:+
  S "from" +:+ phrase coil `S.of_` ch htFluxC)
  $ resourcePath ++ "TankWaterOnly.png"

physSystParts :: [Sentence]
physSystParts = map foldlSent_ [physSyst1 tank water, physSyst2 coil tank htFluxC]

goalInputs :: [Sentence]
goalInputs = [phraseNP (temp `the_ofThe` coil),
  S "the initial" +:+ phrase tempW, pluralNP (the materialProprty)]

------------------------------------------------------
--Section 4.2 : SOLUTION CHARACTERISTICS SPECIFICATION
------------------------------------------------------

tMods :: [TheoryModel]
tMods = [consThermE, sensHtE, nwtnCooling]

sensHtE :: TheoryModel
sensHtE = sensHtETemplate Liquid sensHtEdesc

sensHtEdesc :: Sentence
sensHtEdesc = foldlSent [ch QT.sensHeat, S "occurs as long as the", phrase material_, S "does not reach a",
  phrase temp, S "where a", phrase phaseChange, S "occurs" `sC` S "as assumed in", refS assumpWAL]

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
