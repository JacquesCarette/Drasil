module Drasil.SSP.Body (srs, si, symbMap, printSetting) where

import Language.Drasil hiding (Symbol(..), Verb, number, organization, section)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block(Parallel), ChunkDB, ReferenceDB,
  SystemInformation(SI), cdb, rdb, refdb, _authors, _concepts, _constants,
  _constraints, _datadefs, _definitions, _defSequence, _inputs, _kind, _outputs,
  _quants, _sys, _sysinfodb, _usedinfodb, sampleData)
import Theory.Drasil (qdFromDD)

import Prelude hiding (sin, cos, tan)
import Utils.Drasil

import Drasil.DocLang (DocSection(..), IntroSec(..), IntroSub(..),
  LFunc(..), RefSec(..), RefTab(..), TConvention(..),
  TSIntro(..), Fields, Field(..), SRSDecl, SSDSec(..), SSDSub(..),
  Verbosity(..), InclUnits(..), DerivationDisplay(..), SolChSpec(..),
  SCSSub(..), GSDSec(..), GSDSub(..), TraceabilitySec(TraceabilityProg),
  ReqrmntSec(..), ReqsSub(..), AuxConstntSec(..), ProblemDescription(PDProg),
  PDSub(..), intro, mkDoc, tsymb'', traceMatStandard)

import qualified Drasil.DocLang.SRS as SRS (inModel, assumpt, sysCon,
  genDefn, dataDefn, datCon)

import Data.Drasil.Concepts.Documentation as Doc (analysis, assumption,
  constant, constraint, definition, document, effect, endUser, environment,
  goal, information, input_, interest, loss, method_, model, organization,
  physical, physics, problem, purpose, requirement, software,
  softwareSys, srsDomains, symbol_, sysCont, system, systemConstraint,
  template, type_, user, value, variable, doccon, doccon')
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.IdeaDicts as Doc (inModel, thModel)
import Data.Drasil.Concepts.Education (solidMechanics, undergraduate, educon)
import Data.Drasil.Concepts.Math (equation, shape, surface, mathcon, mathcon',
  number)
import Data.Drasil.Concepts.PhysicalProperties (dimension, mass, physicalcon)
import Data.Drasil.Concepts.Physics (cohesion, fbd, force, gravity, isotropy,
  strain, stress, time, twoD, physicCon)
import Data.Drasil.Concepts.Software (program, softwarecon)
import Data.Drasil.Concepts.SolidMechanics (mobShear, normForce, shearForce, 
  shearRes, solidcon)
import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Software.Products (sciCompS, prodtcon)
import Data.Drasil.Theories.Physics (physicsTMs)

import Data.Drasil.People (brooks, henryFrankis)
import Data.Drasil.Citations (koothoor2013, smithLai2005)
import Data.Drasil.SI_Units (degree, metre, newton, pascal, kilogram, second, derived, fundamentals)

import Drasil.SSP.Assumptions (assumptions)
import Drasil.SSP.Changes (likelyChgs, unlikelyChgs)
import qualified Drasil.SSP.DataDefs as SSP (dataDefs)
import Drasil.SSP.Defs (acronyms, crtSlpSrf, defs, defs', effFandS, factor, fsConcept,
  intrslce, layer, morPrice, mtrlPrpty, plnStrn, slice, slip, slope, slpSrf, soil,
  soilLyr, soilMechanics, soilPrpty, ssa, ssp, stabAnalysis, waterTable)
import Drasil.SSP.GenDefs (generalDefinitions)
import Drasil.SSP.Goals (goals)
import Drasil.SSP.IMods (instModIntro)
import qualified Drasil.SSP.IMods as SSP (iMods)
import Drasil.SSP.References (citations, morgenstern1965)
import Drasil.SSP.Requirements (funcReqs, funcReqTables, nonFuncReqs)
import Drasil.SSP.TMods (tMods)
import Drasil.SSP.Unitals (constrained, effCohesion, fricAngle, fs, index,
  inputs, inputsWUncrtn, outputs, symbols)

--Document Setup--

srs :: Document
srs = mkDoc mkSRS for si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

resourcePath :: String
resourcePath = "../../../datafiles/SSP/"

si :: SystemInformation
si = SI {
  _sys = ssp, 
  _kind = Doc.srs, 
  _authors = [henryFrankis, brooks],
  _quants = symbols,
  _concepts = [] :: [DefinedQuantityDict],
  _definitions = [] :: [QDefinition],
  _datadefs = SSP.dataDefs,
  _inputs = map qw inputs,
  _outputs = map qw outputs,
  _defSequence = [(\x -> Parallel (head x) (tail x)) $ map qdFromDD SSP.dataDefs],
  _constraints = constrained,
  _constants = [],
  _sysinfodb = symbMap,
  _usedinfodb = usedDB,
   refdb = refDB,
   sampleData = "../../datafiles/SSP/sampleInput.txt"
}
  
mkSRS :: SRSDecl
mkSRS = [RefSec $ RefProg intro
  [TUnits, tsymb'' tableOfSymbIntro TAD, TAandA],
  IntroSec $ IntroProg startIntro kSent
    [IPurpose prpsOfDoc_p1
    , IScope scope
    , IChar []
        [phrase undergraduate +:+ S "level 4" +:+ phrase Doc.physics,
        phrase undergraduate +:+ S "level 2 or higher" +:+ phrase solidMechanics]
        [phrase soilMechanics]
    , IOrgSec orgSecStart inModel (SRS.inModel [] []) orgSecEnd],
    --FIXME: issue #235
    GSDSec $ GSDProg2 [SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList],
      UsrChars [userCharIntro], SystCons [sysConstraints] []],
    SSDSec $
      SSDProg
        [ SSDProblem $ PDProg prob []
          [ TermsAndDefs Nothing terms
          , PhySysDesc ssp physSystParts figPhysSyst physSystContents 
          , Goals goalsInputs]
        , SSDSolChSpec $ SCSProg
          [Assumptions
          , TMs [] (Label : stdFields)
          , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
          , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
          , IMs instModIntro ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
          , Constraints EmptyS inputsWUncrtn --FIXME: issue #295
          , CorrSolnPpties outputs []
          ]
        ],
    ReqrmntSec $ ReqsProg [
    FReqsSub funcReqTables,
    NonFReqsSub
  ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $ AuxConsProg ssp [],
  Bibliography]

units :: [UnitDefn]
units = map unitWrapper [metre, degree, kilogram, second] ++ map unitWrapper [newton, pascal]

concIns :: [ConceptInstance]
concIns = goals ++ assumptions ++ funcReqs ++ nonFuncReqs ++ likelyChgs ++ unlikelyChgs

section :: [Section]
section = extractSection srs

labCon :: [LabelledContent]
labCon = [figPhysSyst, figIndexConv, figForceActing] ++ funcReqTables

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

-- SYMBOL MAP HELPERS --
symbMap :: ChunkDB
symbMap = cdb (map qw SSP.iMods ++ map qw symbols) (map nw symbols
  ++ map nw acronyms ++ map nw doccon ++ map nw prodtcon ++ map nw generalDefinitions ++ map nw SSP.iMods
  ++ map nw defs ++ map nw defs' ++ map nw softwarecon ++ map nw physicCon 
  ++ map nw physicsTMs
  ++ map nw mathcon ++ map nw mathcon' ++ map nw solidcon ++ map nw physicalcon
  ++ map nw doccon' ++ map nw derived ++ map nw fundamentals ++ map nw educon
  ++ map nw compcon ++ [nw algorithm, nw ssp] ++ map nw units)
  (map cw SSP.iMods ++ map cw symbols ++ srsDomains) units SSP.dataDefs SSP.iMods
  generalDefinitions tMods concIns section labCon

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw symbols ++ map nw acronyms)
 ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] []

refDB :: ReferenceDB
refDB = rdb citations concIns

-- SECTION 1 --
--automatically generated in mkSRS -

-- SECTION 1.1 --
--automatically generated in mkSRS

-- SECTION 1.2 --
--automatically generated in mkSRS using the intro below
tableOfSymbIntro :: [TSIntro]
tableOfSymbIntro = [TSPurpose, TypogConvention [Verb $ foldlSent_
  [plural value, S "with a subscript", ch index, S "implies that the",
  phrase value, S "will be taken at and analyzed at a", phrase slice
  `sOr` phrase slice, S "interface composing the total slip", phrase mass]],
  VectorUnits]

-- SECTION 1.3 --
--automatically generated in mkSRS

-- SECTION 2 --
startIntro, kSent :: Sentence
startIntro = foldlSent [S "A", phrase slope, S "of geological",
  phrase mass `sC` S "composed of", phrase soil, S "and rock and sometimes",
  S "water, is subject to the influence" `sOf` phrase gravity +:+ S "on the" +:+.
  phrase mass, S "This can cause instability in the form" `sOf` phrase soil +:+.
  S "or rock movement", S "The", plural effect `sOf` phrase soil +:+
  S "or rock movement can range from inconvenient to",
  S "seriously hazardous, resulting in significant life and economic" +:+.
  plural loss, atStart slope, S "stability is of", phrase interest,
  S "both when analysing natural", plural slope `sC`
  S "and when designing an excavated" +:+.  phrase slope, atStart ssa `sIs`
  (S "assessment" `ofThe` S "safety of a" +:+ phrase slope) `sC`
  S "identifying the", phrase surface,
  S "most likely to experience", phrase slip `sAnd`
  S "an index of its relative stability known as the", phrase fs]

kSent = keySent ssa ssp

keySent :: (Idea a, Idea b) => a -> b -> Sentence
keySent probType pname = foldlSent_ [S "a", phrase probType +:+. phrase problem,
  S "The developed", phrase program, S "will be referred to as the",
  introduceAbb pname]
  
-- SECTION 2.1 --
-- Purpose of Document automatically generated in IPurpose
prpsOfDoc_p1 :: Sentence
prpsOfDoc_p1 = purposeDoc ssp

purposeDoc :: (Idea a) => a -> Sentence
purposeDoc pname =
  foldlSent [S "The primary purpose of this", phrase document, S "is to",
  S "record the", plural requirement `sOf` short pname `andThe` plural model,
  S "that will be used to meet those" +:+. plural requirement, 
  atStart' goal `sC` plural assumption `sC` plural thModel `sC` 
  plural definition `sC` S "and other", phrase model, S "derivation",
  phrase information, S "are specified" `sC` S "allowing the reader to fully",
  S "understand" `sAnd` S "verify the", phrase purpose `sAnd` S "scientific",
  S "basis of" +:+. short pname, S "With the exception of", 
  plural systemConstraint, S "in", makeRef2S (SRS.sysCon [] []) `sC` S "this",
  short Doc.srs, S "will remain abstract, describing what", phrase problem,
  S "is being solved, but not how to solve it"] 
  --FIXME: Last sentence is also present in GlassBR, SWHS and NoPCM... pull out?

-- SECTION 2.2 --
-- Scope of Requirements automatically generated in IScope
scope :: Sentence
scope = foldlSent_ [phrase stabAnalysis, S "of a", phrase twoD, sParen (getAcc twoD),
  phrase soil, phrase mass `sC` S "composed of a single homogeneous", phrase layer,
  S "with", phrase constant +:+. plural mtrlPrpty, S "The", phrase soil,
  phrase mass `sIs` S "assumed to extend infinitely in the third" +:+.
  phrase dimension, S "The", phrase analysis, S "will be at an instant" `sIn`
  phrase time :+: S ";", plural factor, S "that may change the", plural soilPrpty,
  S "over", phrase time, S "will not be considered"]

-- SECTION 2.3 --
-- Characteristics of the Intended Reader generated in IChar

-- SECTION 2.4 --
-- Organization automatically generated in IOrgSec
orgSecStart, orgSecEnd :: Sentence
orgSecStart = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the", phrase template, S "for an",
  short Doc.srs, S "for", phrase sciCompS,
  S "proposed by Koothoor", makeRef2S koothoor2013, S "as well as Smith" `sAnd`
  S "Lai", makeRef2S smithLai2005]
orgSecEnd   = foldlSent_ [S "The", plural inModel, S "provide the set of",
  S "algebraic", plural equation, S "that must be solved"]

-- SECTION 3 --
-- SECTION 3.1 --
-- System Context automatically generated
sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [makeRef2S sysCtxFig1 +:+ S "shows the" +:+. phrase sysCont,
   S "A circle represents an external entity outside the" +:+. phrase software, S "A rectangle represents the",
   phrase softwareSys, S "itself" +:+. sParen (short ssp),
   S "Arrows are used to show the data flow between the" +:+ phrase system,
   S "and its" +:+ phrase environment]
   
sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) (resourcePath ++ "SystemContextFigure.png")

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol
  [S "The responsibilities of the", phrase user, S "and the", phrase system,
   S "are as follows"]
   
sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide the" +:+ phrase input_ +:+ S "data related to" +:+
  S "the" +:+ phrase soilLyr :+: S "(s) and water table (if applicable)" `sC`
  S "ensuring conformation to" +:+ phrase input_ +:+ S "data format" +:+
  S "required by" +:+ short ssp,
  S "Ensure that consistent units are used for" +:+ phrase input_ +:+ 
  plural variable,
  S "Ensure required" +:+ phrase software +:+ plural assumption +:+ sParen ( 
  makeRef2S $ SRS.assumpt ([]::[Contents]) ([]::[Section])) +:+ S "are" +:+ 
  S "appropriate for the" +:+ phrase problem +:+ S "to which the" +:+ 
  phrase user +:+ S "is applying the" +:+ phrase software]
  
sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Detect data" +:+ phrase type_ +:+ S "mismatch, such as" +:+
  S "a string of characters" +:+ phrase input_ +:+ S "instead of a floating" +:+
  S "point" +:+ phrase number,
  S "Verify that the" +:+ plural input_ +:+ S "satisfy the required" +:+
  phrase physical `sAnd` S "other data" +:+ plural constraint +:+ sParen (makeRef2S $ SRS.datCon ([]::[Contents]) ([]::[Section])),
  S "Identify the" +:+ phrase crtSlpSrf +:+ S "within the possible" +:+
  phrase input_ +:+ S "range",
  S "Find the" +:+ phrase fsConcept +:+ S "for the" +:+ phrase slope,
  S "Find the" +:+ phrase intrslce +:+ phrase normForce `sAnd` phrase shearForce +:+ S "along the" +:+ phrase crtSlpSrf]
  
sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short ssp +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]

-- SECTION 3.2 --
-- User Characteristics automatically generated with the
-- userContraints intro below

userCharIntro :: Contents
userCharIntro = userChar ssp [S "Calculus", titleize Doc.physics]
  [phrase soil, plural mtrlPrpty] [phrase effCohesion, phrase fricAngle, 
  S "unit weight"]

userChar :: (Idea a) => a -> [Sentence] -> [Sentence] -> [Sentence] -> Contents
userChar pname understandings familiarities specifics = foldlSP [
  S "The", phrase endUser `sOf` short pname,
  S "should have an understanding of undergraduate Level 1",
  foldlList Comma List understandings `sC`
  S "and be familiar with", foldlList Comma List familiarities `sC` 
  S "specifically", foldlList Comma List specifics]

-- SECTION 3.2 --
sysConstraints :: Contents
sysConstraints = foldlSP [S "The", phrase morPrice, phrase method_, 
  makeRef2S morgenstern1965 `sC` S "which involves dividing the", phrase slope,
  S "into vertical", plural slice `sC` S "will be used to derive the",
  plural equation, S "for analysing the", phrase slope]

-- SECTION 4 --

-- SECTION 4.1 --
prob :: Sentence
prob = foldlSent_ [S "evaluate the", phrase fs `sOf` S "a", phrasePoss slope,
  phrase slpSrf `sAnd` S "identify", phrase crtSlpSrf `ofThe` phrase slope `sC`
  S "as well as the", phrase intrslce, phrase normForce `sAnd` phrase shearForce,
  S "along the", phrase crtSlpSrf]

{-
From when solution was used in Problem Description:
  It is intended to be used as an educational tool for introducing slope stability
  issues and to facilitate the analysis and design of a safe slope.
-}

-- SECTION 4.1.1 --
terms :: [ConceptChunk]
terms = [fsConcept, slpSrf, crtSlpSrf, waterTable, stress, strain, normForce,
  shearForce, mobShear, shearRes, effFandS, cohesion, isotropy, plnStrn]

  -- most of these are in concepts (physics or solidMechanics)
  -- except for fsConcept, crtSlpSrf & plnStrn which are in defs.hs

-- SECTION 4.1.2 --
physSystParts :: [Sentence]
physSystParts = map foldlSent [
  [S "A", phrase slope, S "comprised of one", phrase soilLyr],
  [S "A", phrase waterTable `sC` S "which may or may not exist"]]

figPhysSyst :: LabelledContent
figPhysSyst = llcc (makeFigRef "PhysicalSystem") $
  fig (foldlSent_ [S "An example", phrase slope, S "for", phrase analysis,
  S "by", short ssp `sC` S "where the dashed line represents the",
  phrase waterTable]) (resourcePath ++ "PhysSyst.png")

physSystContents :: [Contents]
physSystContents = [physSysConv, LlC figIndexConv, physSysFbd, LlC figForceActing]

physSysConv :: Contents
physSysConv = foldlSP [atStart morPrice, phrase analysis, makeRef2S morgenstern1965,
  S "of the", phrase slope,  S "involves representing the", phrase slope,
  S "as a series of vertical" +:+. plural slice, S "As shown in",
  makeRef2S figIndexConv `sC` S "the", phrase index, ch index, S "is used to denote a",
  phrase value, S "for a single", phrase slice `sC` S "and an", phrase intrslce, 
  phrase value, S "at a given", phrase index, ch index, S "refers to the",
  phrase value, S "between", phrase slice, ch index `sAnd` S "adjacent", phrase slice,
  E $ sy index + 1]

figIndexConv :: LabelledContent
figIndexConv = llcc (makeFigRef "IndexConvention") $ 
  fig (foldlSent_ [S "Index convention for", phrase slice `sAnd` 
  phrase intrslce, plural value]) (resourcePath ++ "IndexConvention.png")

physSysFbd :: Contents
physSysFbd = foldlSP [S "A", phrase fbd, S "of the", plural force, S "acting on a",
  phrase slice `sIs` S "displayed in" +:+. makeRef2S figForceActing, S "The specific",
  plural force `sAnd` plural symbol_, S "will be discussed in detail in",
  makeRef2S (SRS.genDefn [] []) `sAnd` makeRef2S (SRS.dataDefn [] [])]

figForceActing :: LabelledContent
figForceActing = llcc (makeFigRef "ForceDiagram") $
  fig (atStart fbd `sOf` plural force +:+ S "acting on a" +:+
  phrase slice) (resourcePath ++ "ForceDiagram.png")

-- SECTION 4.1.3 --
goalsInputs :: [Sentence]
goalsInputs = [phrase shape `ofThe` phrase soil +:+ S "mass",
  S "location" `ofThe` phrase waterTable, plural mtrlPrpty `ofThe` phrase soil]

-- SECTION 4.2 --

-- SECTION 4.2.1 --
-- Assumptions is automatically generated

-- SECTION 4.2.2 --
-- TModels is automatically generated

-- SECTION 4.2.3 --
-- General Definitions is automatically generated

-- SECTION 4.2.4 --
-- Data Definitions is automatically generated
--FIXME: derivations should be with the appropriate DDef

-- SECTION 4.2.5 --
-- Instance Models is automatically generated
--FIXME: derivations should be with the appropriate IMod

-- SECTION 4.2.6 --
-- Data Constraints is automatically generated

{-
{-input data-}
noTypicalVal, vertConvention :: Sentence
noTypicalVal   = short notApp
vertConvention = S "Consecutive vertexes have increasing x" +:+.
  plural value +:+ S "The start and end vertices of all layers" +:+
  S "go to the same x" +:+. plural value --Monotonicly increasing?

verticesConst :: Sentence -> [Sentence]
verticesConst vertexType = [vertVar vertexType, vertConvention,
  noTypicalVal, noTypicalVal, noTypicalVal]

waterVert, slipVert, slopeVert :: [Sentence]
waterVert = verticesConst $ S "water" +:+ phrase table_
slipVert  = verticesConst $ phrase slip
slopeVert = verticesConst $ phrase slope
-}

-- SECTION 4.2.7 --

-- SECTION 5 --

-- SECTION 5.1 --

-- SECTION 5.2 --

-- SECTION 6 --
--Likely Changes is automatically generated

-- SECTION 7 --
-- Table of aux consts is automatically generated

-- References --
-- automatically generated
