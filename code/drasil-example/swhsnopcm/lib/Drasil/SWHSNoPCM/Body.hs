module Drasil.SWHSNoPCM.Body (srs, si, symbMap, printSetting, fullSI, mkSRS, noPCMODEInfo) where

import Data.List ((\\))
import Control.Lens ((^.))

import Language.Drasil hiding (section)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.System (SystemKind(Specification), mkSystem, systemdb)

import Drasil.Metadata (inModel)
import Drasil.SRSDocument
import qualified Drasil.DocLang.SRS as SRS (inModel)
import Drasil.Generator (cdb)
import Data.Drasil.People (thulasi)

import Data.Drasil.Concepts.Documentation as Doc (material_)
import Data.Drasil.Concepts.Math (mathcon', ode)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty, physicalcon)
import qualified Data.Drasil.Concepts.Physics as CP (physicCon', energy, mechEnergy, pressure)
import Data.Drasil.Concepts.Software (softwarecon)
import Data.Drasil.Concepts.Thermodynamics (heatCapSpec, htFlux, phaseChange,
  temp, thermalAnalysis, thermalConduction, thermocon, boilPt, latentHeat, meltPt)

import Data.Drasil.ExternalLibraries.ODELibraries (scipyODESymbols, osloSymbols,
  apacheODESymbols, odeintSymbols, odeInfoChunks)

import qualified Data.Drasil.Quantities.Thermodynamics as QT (temp,
  heatCapSpec, htFlux, sensHeat)
import Data.Drasil.Quantities.Math (gradient, pi_, piConst, surface,
  uNormalVect, surArea, area)
import Data.Drasil.Quantities.PhysicalProperties (vol, mass, density)
import Data.Drasil.Quantities.Physics (time, energy)

import Theory.Drasil (TheoryModel)

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
import Drasil.SWHSNoPCM.Definitions (htTrans)
import Drasil.SWHSNoPCM.GenDefs (genDefs)
import Drasil.SWHSNoPCM.Goals (goals)
import Drasil.SWHSNoPCM.IMods (eBalanceOnWtr, instModIntro)
import Drasil.SWHSNoPCM.LabelledContent (labelledContent, figTank, sysCntxtFig)
import Drasil.SWHSNoPCM.MetaConcepts (progName)
import qualified Drasil.SWHSNoPCM.IMods as NoPCM (iMods)
import Drasil.SWHSNoPCM.ODEs
import Drasil.SWHSNoPCM.Requirements (funcReqs, funcReqsTables)
import Drasil.SWHSNoPCM.References (citations)
import Drasil.SWHSNoPCM.Unitals (inputs, constrained, unconstrained,
  specParamValList, outputs)

-- This contains the list of symbols used throughout the document
symbols :: [DefinedQuantityDict]
symbols = dqdWr watE : map dqdWr concepts ++ map dqdWr constrained

symbolsAll :: [DefinedQuantityDict] --FIXME: Why is PCM (swhsSymbolsAll) here?
                               --Can't generate without SWHS-specific symbols like pcmHTC and pcmSA
                               --FOUND LOC OF ERROR: Instance Models
-- FIXME: the dependent variable of noPCMODEInfo (tempW) is currently added to symbolsAll automatically as it is used to create new chunks with tempW's UID suffixed in ODELibraries.hs.
-- The correct way to fix this is to add the chunks when they are created in the original functions. See #4298 and #4301
symbolsAll = [gradient, pi_, uNormalVect, dqdWr surface] ++ symbols ++
  map dqdWr symbolConcepts ++ map dqdWr specParamValList ++ map dqdWr [absTol, relTol] ++
  scipyODESymbols ++ osloSymbols ++ apacheODESymbols ++ odeintSymbols ++
  odeInfoChunks noPCMODEInfo

concepts :: [UnitalChunk]
concepts = map ucw [tau, inSA, outSA, htCapL, htFluxIn, htFluxOut, volHtGen,
  htTransCoeff, tankVol, deltaT, tempEnv, thFluxVect, htFluxC, wMass, wVol, tauW]

symbolConcepts :: [UnitalChunk]
symbolConcepts = map ucw [density, mass, time, vol,
  QT.temp, QT.heatCapSpec, QT.htFlux, QT.sensHeat]

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
    FReqsSub funcReqsTables,
    NonFReqsSub
  ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $ AuxConsProg progName specParamValList,
  Bibliography]

fullSI :: System
fullSI = fillcdbSRS mkSRS si

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) fullSI

printSetting :: PrintingInformation
printSetting = piSys (fullSI ^. systemdb) Equational defaultConfiguration

concIns :: [ConceptInstance]
concIns = goals ++ funcReqs ++ nfRequirements ++ assumptions ++
 [likeChgTCVOD, likeChgTCVOL] ++ likelyChgs ++ [likeChgTLH] ++ unlikelyChgs

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

si :: System
si = mkSystem
  progName Specification [thulasi]
  [purp] [introStartNoPCM] [scope] [motivation]
  -- FIXME: Everything after (and including) \\ should be removed when
  -- #1658 is resolved. Basically, _quants is used here, but
  -- tau does not appear in the document and thus should not be displayed.
  ((map dqdWr unconstrained ++ symbolsAll) \\ [dqdWr tau])
  tMods genDefs NoPCM.dataDefs NoPCM.iMods
  []
  inputs outputs
  (map cnstrw' constrained ++ map cnstrw' [tempW, watE]) (piConst : specParamValList)
  symbMap

purp :: Sentence
purp = foldlSent_ [S "investigate the heating" `S.of_` D.toSent (phraseNP (water `inA` sWHT))]

ideaDicts :: [IdeaDict]
ideaDicts =
  -- Actual IdeaDicts
  [htTrans, materialProprty] ++
  -- CIs
  map nw [progName, phsChgMtrl] ++
  map nw CP.physicCon' ++ map nw mathcon'

conceptChunks :: [ConceptChunk]
conceptChunks =
  -- ConceptChunks
  softwarecon ++ thermocon ++ con ++ physicalcon ++ [boilPt, latentHeat,
  meltPt] ++ [CP.energy, CP.mechEnergy, CP.pressure] ++
  -- DefinedQuantityDicts
  map cw [surArea, area]

symbMap :: ChunkDB
symbMap = cdb symbolsAll ideaDicts conceptChunks ([] :: [UnitDefn]) NoPCM.dataDefs
  NoPCM.iMods genDefs tMods concIns (labelledContent ++ funcReqsTables) allRefs citations

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
orgDocEnd = foldlSent_ [D.toSent (atStartNP (the inModel)),
  S "to be solved" `S.is` S "referred to as" +:+. refS eBalanceOnWtr,
  D.toSent (atStartNP (the inModel)), S "provides the", titleize ode,
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

physSystParts :: [Sentence]
physSystParts = map foldlSent_ [physSyst1 tank water, physSyst2 coil tank htFluxC]

goalInputs :: [Sentence]
goalInputs = [D.toSent (phraseNP (temp `the_ofThe` coil)),
  S "the initial" +:+ phrase tempW, D.toSent (pluralNP (the materialProprty))]

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
