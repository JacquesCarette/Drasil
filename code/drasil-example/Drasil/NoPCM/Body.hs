module Drasil.NoPCM.Body where

import Language.Drasil hiding (section, sec)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block(Parallel), ChunkDB, RefbyMap, ReferenceDB,
  SystemInformation(SI), TraceMap, ccss, cdb, collectUnits, generateRefbyMap,
  rdb, refdb, _authors, _concepts, _constants, _constraints, _datadefs,
  _definitions, _defSequence, _inputs, _kind, _outputs, _quants, _sys,
  _sysinfodb, _usedinfodb)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import Utils.Drasil

import qualified Data.Map as Map
import Data.Drasil.People (thulasi)

import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation as Doc (assumption, content,
  definition, doccon, doccon', document, goal, information, material_, model,
  problem, property, purpose, reference, srsDomains)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.IdeaDicts as Doc (inModel, thModel)
import Data.Drasil.Concepts.Education (educon)
import Data.Drasil.Concepts.Math (mathcon, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (materialProprty, physicalcon)
import Data.Drasil.Concepts.Physics (physicCon, physicCon')
import Data.Drasil.Concepts.Software (program, softwarecon)
import Data.Drasil.Concepts.Thermodynamics (enerSrc, thermalAnalysis, temp,
  thermalEnergy, htTransTheo, htFlux, heatCapSpec, thermalConduction, thermocon,
  phaseChange)

import qualified Data.Drasil.Concepts.Math as M (ode, de)
import qualified Data.Drasil.Quantities.Thermodynamics as QT (temp,
  heatCapSpec, htFlux, sensHeat)

import Data.Drasil.Quantities.Math (gradient, pi_, surface, uNormalVect)
import Data.Drasil.Quantities.PhysicalProperties (vol, mass, density)
import Data.Drasil.Quantities.Physics (time, energy, physicscon)

import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.SI_Units (metre, kilogram, second, centigrade, joule, watt,
  fundamentals, derived)

import qualified Drasil.DocLang.SRS as SRS (inModel)
import Drasil.DocLang (DocDesc, Fields, Field(..), Verbosity(Verbose), 
  InclUnits(IncludeUnits), SCSSub(..), DerivationDisplay(..), SSDSub(..),
  SolChSpec(..), SSDSec(..), DocSection(..), GSDSec(..), GSDSub(..),
  AuxConstntSec(AuxConsProg), IntroSec(IntroProg), LCsSec(..), UCsSec(..),
  IntroSub(IOrgSec, IScope, IChar, IPurpose), Literature(Lit, Doc'),
  ReqrmntSec(..), ReqsSub(..), RefSec(RefProg), RefTab(TAandA, TUnits),
  TraceabilitySec(TraceabilityProg), TSIntro(SymbOrder, SymbConvention, TSPurpose),
  ProblemDescription(PDProg), PDSub(..), inDataConstTbl, intro, mkDoc, outDataConstTbl, tsymb,
  getDocDesc, egetDocDesc, generateTraceMap, getTraceMapFromTM,
  getTraceMapFromGD, getTraceMapFromDD, getTraceMapFromIM, getSCSSub,
  generateTraceMap', traceMatStandard)

-- Since NoPCM is a simplified version of SWHS, the file is to be built off
-- of the SWHS libraries.  If the source for something cannot be found in
-- NoPCM, check SWHS.
import Drasil.SWHS.Body (charReader1, charReader2, dataContMid, orgDocIntro,
  physSyst1, physSyst2, sysCntxtDesc, sysCntxtFig, systContRespBullets,
  sysCntxtRespIntro, userChars)
import Drasil.SWHS.Changes (likeChgTCVOD, likeChgTCVOL, likeChgTLH)
import Drasil.SWHS.Concepts (acronyms, coil, progName, sWHT, tank, transient, water, con)
import Drasil.SWHS.DataDefs (dd1HtFluxC, dd1HtFluxCQD)
import Drasil.SWHS.References (incroperaEtAl2007, koothoor2013, lightstone2012, 
  parnasClements1986, smithLai2005)
import Drasil.SWHS.Requirements (nfRequirements)
import Drasil.SWHS.TMods (consThermE, sensHtETemplate, PhaseChange(Liquid))
import Drasil.SWHS.Unitals (coilSAMax, deltaT, eta, htFluxC, htFluxIn, 
  htFluxOut, htCapL, htTransCoeff, inSA, outSA, tankVol, tau, tauW, tempEnv, 
  tempW, thFluxVect, volHtGen, watE, wMass, wVol, unitalChuncks, absTol, relTol)

import Drasil.NoPCM.Assumptions
import Drasil.NoPCM.Changes (likelyChgs, unlikelyChgs)
import Drasil.NoPCM.Definitions (srsSWHS, htTrans)
import Drasil.NoPCM.GenDefs (genDefs)
import Drasil.NoPCM.Goals (goals)
import Drasil.NoPCM.IMods (eBalanceOnWtr, instModIntro)
import qualified Drasil.NoPCM.IMods as NoPCM (iMods)
import Drasil.NoPCM.Requirements (funcReqs, inputInitQuantsTable)
import Drasil.NoPCM.Unitals (inputs, constrained, specParamValList)

-- This defines the standard units used throughout the document
thisSi :: [UnitDefn]
thisSi = map unitWrapper [metre, kilogram, second] ++ map unitWrapper [centigrade, joule, watt]

checkSi :: [UnitDefn]
checkSi = collectUnits symbMap symbTT 

-- This contains the list of symbols used throughout the document
symbols :: [DefinedQuantityDict]
symbols = pi_ : map dqdWr units ++ map dqdWr constrained
 ++ map dqdWr [tempW, watE]
 ++ [gradient, uNormalVect] ++ map dqdWr [surface]

resourcePath :: String
resourcePath = "../../../datafiles/NoPCM/"
  
symbolsAll :: [QuantityDict] --FIXME: Why is PCM (swhsSymbolsAll) here?
                               --Can't generate without SWHS-specific symbols like pcmHTC and pcmSA
                               --FOUND LOC OF ERROR: Instance Models
symbolsAll = map qw symbols ++ map qw specParamValList ++ 
  map qw [coilSAMax] ++ map qw [tauW] ++ map qw [eta] ++
  map qw [absTol, relTol]

units :: [UnitaryConceptDict]
units = map ucw [density, tau, inSA, outSA,
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
  
mkSRS :: DocDesc
mkSRS = [RefSec $ RefProg intro
  [TUnits,
  tsymb [TSPurpose, SymbConvention [Lit $ nw htTrans, Doc' $ nw progName], SymbOrder],
  TAandA],
  IntroSec $ IntroProg (introStart enerSrc energy progName)
    (introEnd progName program)
  [IPurpose $ purpDoc progName,
  IScope (scopeReqStart thermalAnalysis sWHT) (scopeReqEnd temp thermalEnergy
    water),
  IChar [] (charReader1 htTransTheo ++ charReader2 M.de) [],
  IOrgSec orgDocIntro inModel (SRS.inModel [] []) $ orgDocEnd inModel M.ode progName],
  GSDSec $ GSDProg2 
    [ SysCntxt [sysCntxtDesc progName, LlC sysCntxtFig, sysCntxtRespIntro progName, systContRespBullets]
    , UsrChars [userChars progName]
    , SystCons [] []
    ],
  SSDSec $
    SSDProg
    [ SSDProblem $ PDProg probDescIntro []
      [ TermsAndDefs Nothing terms
      , PhySysDesc progName physSystParts figTank []
      , Goals goalInputs goals]
    , SSDSolChSpec $ SCSProg
      [ Assumptions
      , TMs [] (Label : stdFields) theoreticalModels
      , GDs [] ([Label, Units] ++ stdFields) genDefs ShowDerivation
      , DDs [] ([Label, Symbol, Units] ++ stdFields) [dd1HtFluxC] ShowDerivation
      , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields)
        NoPCM.iMods ShowDerivation
      , Constraints dataContMid [dataConstTable1]
      , CorrSolnPpties propsDeriv
      ]
    ],
  ReqrmntSec $ ReqsProg [
    FReqsSub funcReqs [inputInitQuantsTable],
    NonFReqsSub nfRequirements
  ],
  LCsSec $ LCsProg $ [likeChgTCVOD, likeChgTCVOL] ++ likelyChgs ++ [likeChgTLH],
  UCsSec $ UCsProg unlikelyChgs,
  TraceabilitySec $
    TraceabilityProg (map fst traceabilityMatrices)
      (map (foldlList Comma List . snd) traceabilityMatrices) (map (LlC . fst) traceabilityMatrices) [],
  AuxConstntSec $ AuxConsProg progName specParamValList,
  Bibliography]

label :: TraceMap
label = Map.union (generateTraceMap mkSRS) $ generateTraceMap' concIns
 
refBy :: RefbyMap
refBy = generateRefbyMap label

dataDefn :: [DataDefinition]
dataDefn = getTraceMapFromDD $ getSCSSub mkSRS

iMods :: [InstanceModel]
iMods = getTraceMapFromIM $ getSCSSub mkSRS

genDef :: [GenDefn]
genDef = getTraceMapFromGD $ getSCSSub mkSRS

theory :: [TheoryModel]
theory = getTraceMapFromTM $ getSCSSub mkSRS

concIns :: [ConceptInstance]
concIns =
 goals ++ funcReqs ++ [likeChgTCVOD, likeChgTCVOL] ++ assumptions ++ likelyChgs ++
 [likeChgTLH] ++ unlikelyChgs

section :: [Section]
section = sec

labCon :: [LabelledContent]
labCon = [inputInitQuantsTable, dataConstTable1, dataConstTable2]

sec :: [Section]
sec = extractSection srs

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

si :: SystemInformation
si = SI {
  _sys = srsSWHS,
  _kind = Doc.srs,
  _authors = [thulasi],
  _quants = symbTT,
  _concepts = symbols,
  _definitions = [],
  _datadefs = [dd1HtFluxC],
  _inputs = inputs ++ map qw [tempW, watE], --inputs ++ outputs?
  _outputs = map qw [tempW, watE],     --outputs
  _defSequence = [Parallel dd1HtFluxCQD []],
  _constraints = map cnstrw constrained ++ map cnstrw [tempW, watE],        --constrained
  _constants = specParamValList,
  _sysinfodb = symbMap,
  _usedinfodb = usedDB,
   refdb = refDB
}

refDB :: ReferenceDB
refDB = rdb referencesRefList concIns

srs :: Document
srs = mkDoc mkSRS for si

symbMap :: ChunkDB
symbMap = cdb symbolsAll (map nw symbols ++ map nw acronyms ++ map nw thermocon
  ++ map nw physicscon ++ map nw doccon ++ map nw softwarecon ++ map nw doccon' ++ map nw con
  ++ map nw prodtcon ++ map nw physicCon ++ map nw physicCon' ++ map nw mathcon ++ map nw mathcon'
  ++ map nw specParamValList ++ map nw fundamentals ++ map nw educon ++ map nw derived 
  ++ map nw physicalcon ++ map nw unitalChuncks ++ [nw srsSWHS, nw algorithm, nw htTrans] ++ map nw checkSi
  ++ map nw [absTol, relTol] ++ [nw materialProprty])
  (map cw symbols ++ srsDomains)
  thisSi label refBy dataDefn iMods genDef theory
  concIns section labCon

usedDB :: ChunkDB
usedDB = cdb (map qw symbTT) (map nw symbols ++ map nw acronyms ++ map nw checkSi)
 ([] :: [ConceptChunk]) checkSi label refBy
 dataDefn iMods genDef theory concIns
 section labCon

printSetting :: PrintingInformation
printSetting = PI symbMap defaultConfiguration

symbTT :: [DefinedQuantityDict]
symbTT = ccss (getDocDesc mkSRS) (egetDocDesc mkSRS) symbMap

--------------------------
--Section 2 : INTRODUCTION
--------------------------

introStart :: ConceptChunk -> UnitalChunk -> CI-> Sentence
introStart es en pro = foldlSent [S "Due to increasing cost, diminishing",
  S "availability, and negative environmental impact of",
  S "fossil fuels, there is a higher demand for renewable",
  plural es `sAnd` phrase en +:+. S "storage technology", 
  atStart' pro, S "provide a novel way of storing", phrase en]

introEnd :: CI -> ConceptChunk -> Sentence
introEnd pro pr = foldlSent_ [EmptyS +:+. plural pro, S "The developed",
  phrase pr, S "will be referred to as", titleize pro,
  sParen (short pro)]

-----------------------------------
--Section 2.1 : PURPOSE OF DOCUMENT
-----------------------------------

purpDoc :: CI -> Sentence
purpDoc pro = foldlSent [S "The main", phrase purpose, S "of this",
  phrase document, S "is to describe the modelling of" +:+.
  phrase pro, S "The", plural Doc.goal `sAnd` plural thModel,
  S "used in the", short pro, S "code are provided, with an emphasis",
  S "on explicitly identifying", plural assumption, S "and unambiguous" +:+.
  plural definition, S "This", phrase document,
  S "is intended to be used as a", phrase reference,
  S "to provide ad hoc access to all", phrase information,
  S "necessary to understand and verify the" +:+. phrase model, S "The",
  short Doc.srs, S "is abstract because the", plural content, S "say what",
  phrase problem, S "is being solved, but do not say how to solve it"]

-------------------------------------
--Section 2.2 : SCOPE OF REQUIREMENTS
-------------------------------------

scopeReqStart :: ConceptChunk -> ConceptChunk -> Sentence
scopeReqStart ta sw = foldlSent_ [phrase ta, S "of a single", phrase sw]

scopeReqEnd :: ConceptChunk -> ConceptChunk -> ConceptChunk -> Sentence
scopeReqEnd tem te wa = foldlSent_ [S "predicts the",
  phrase tem `sAnd` phrase te,
  S "histories for the", phrase wa]

--------------------------------------------------
--Section 2.3 : CHARACTERISTICS Of INTENDED READER
--------------------------------------------------
          
---------------------------------------
--Section 2.4: ORGANIZATION OF DOCUMENT
---------------------------------------

orgDocEnd :: CI -> CI -> CI -> Sentence
orgDocEnd im_ od pro = foldlSent_ [S "The", phrase im_,
  S "to be solved is referred to as" +:+. makeRef2S eBalanceOnWtr,
  S "The", phrase im_, S "provides the",
  titleize od, sParen (short od), S "that model the"
  +:+. phrase pro, short pro, S "solves this", short od]

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
  S "the initial" +:+ phrase tempW, S "the material" +:+ plural property]

------------------------------------------------------
--Section 4.2 : SOLUTION CHARACTERISTICS SPECIFICATION
------------------------------------------------------

theoreticalModels :: [TheoryModel]
theoreticalModels = [consThermE, sensHtE]

sensHtE :: TheoryModel
sensHtE = sensHtETemplate Liquid sensHtEdesc

sensHtEdesc :: Sentence
sensHtEdesc = foldlSent [ch QT.sensHeat, S "occurs as long as the", phrase material_, S "does not reach a",
  phrase temp, S "where a", phrase phaseChange, S "occurs" `sC` S "as assumed in", makeRef2S assumpWAL]

--TODO: Implement physical properties of a substance

dataConstTable1 :: LabelledContent
dataConstTable1 = inDataConstTbl constrained
-- s4_2_6_table1 = Table [S "Var", titleize' physicalConstraint, titleize software +:+
  -- titleize' constraint, S "Typical" +:+ titleize value, titleize uncertainty]
  -- (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2), (\x -> x!!3), (\x -> x!!4)]
  -- data_constraint_conListIn) (titleize input_ +:+ titleize' variable) True

dataConstTable2 :: LabelledContent
dataConstTable2 = outDataConstTbl dataConstListOut
-- s4_2_6_table2 = Table [S "Var", titleize' physicalConstraint]
  -- (mkTable [(\x -> x!!0), (\x -> x!!1)] s4_2_6_conListOut)
  -- (titleize output_ +:+ titleize' variable) True

dataConstListOut :: [ConstrConcept]
dataConstListOut = [tempW, watE]

--------------------------
--Section 5 : REQUIREMENTS
--------------------------

propsDeriv :: [Contents]
propsDeriv = [LlC dataConstTable2]

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

traceabilityMatrices :: [(LabelledContent, [Sentence])]
traceabilityMatrices = traceMatStandard si

-- These matrices can probably be generated automatically when enough info is
-- abstracted out.

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

referencesRefList :: BibRef
referencesRefList = [incroperaEtAl2007, koothoor2013, lightstone2012, parnasClements1986, smithLai2005]
