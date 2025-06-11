{-# LANGUAGE PostfixOperators #-}
module Drasil.SSP.Body (srs, si, symbMap, printSetting, fullSI) where

import Control.Lens ((^.))

import Language.Drasil hiding (Verb, number, organization, section, variable)
import Drasil.SRSDocument
import qualified Drasil.DocLang.SRS as SRS (inModel, assumpt,
  genDefn, dataDefn, datCon)
import Theory.Drasil (output)

import Prelude hiding (sin, cos, tan)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation as Doc (analysis, assumption,
  constant, effect, endUser, environment, input_, interest, loss, method_,
  physical, physics, problem, software, softwareSys, srsDomains, symbol_,
  sysCont, system, type_, user, value, variable, doccon, doccon',
  datumConstraint)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.TheoryConcepts as Doc (inModel)
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
import Data.Drasil.Software.Products (prodtcon)
import Data.Drasil.Theories.Physics (weightSrc, hsPressureSrc)

import Data.Drasil.People (brooks, henryFrankis)
import Data.Drasil.SI_Units (degree, metre, newton, pascal, kilogram, second, derived, fundamentals)

import Drasil.SSP.Assumptions (assumptions)
import Drasil.SSP.Changes (likelyChgs, unlikelyChgs)
import Drasil.SSP.DataDefs (dataDefs)
import Drasil.SSP.Defs (acronyms, crtSlpSrf, defs, defs', effFandS, factor, fsConcept,
  intrslce, layer, morPrice, mtrlPrpty, plnStrn, slice, slip, slope, slpSrf, soil,
  soilLyr, soilMechanics, soilPrpty, ssa, stabAnalysis, waterTable)
import Drasil.SSP.GenDefs (generalDefinitions)
import Drasil.SSP.Goals (goals)
import Drasil.SSP.MetaConcepts (progName)
import Drasil.SSP.IMods (instModIntro, iMods)
import Drasil.SSP.References (citations, morgenstern1965)
import Drasil.SSP.Requirements (funcReqs, funcReqTables, nonFuncReqs)
import Drasil.SSP.TMods (tMods)
import Drasil.SSP.Unitals (constrained, effCohesion, fricAngle, fs, index,
  inputs, inputsWUncrtn, outputs, symbols)

--Document Setup--

srs :: Document
srs = mkDoc mkSRS S.forT si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

fullSI :: System
fullSI = fillcdbSRS mkSRS si

resourcePath :: String
resourcePath = "../../../../datafiles/ssp/"

si :: System
si = SI {
  _sys         = progName, 
  _kind        = Doc.srs, 
  _authors     = [henryFrankis, brooks],
  _purpose     = [purp],
  _background  = [],
  _motivation  = [],
  _scope       = [],
  _quants      = symbols,
  _instModels  = iMods,
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = map qw inputs,
  _outputs     = map qw outputs,
  _constraints = constrained,
  _constants   = [],
  _systemdb   = symbMap,
  _usedinfodb  = usedDB
}
  
mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $ RefProg intro
  [TUnits, tsymb'' tableOfSymbIntro TAD, TAandA],
  IntroSec $ IntroProg startIntro kSent
    [ IPurpose $ purpDoc progName Verbose
    , IScope scope
    , IChar []
        [phrase undergraduate +:+ S "level 4" +:+ phrase Doc.physics,
        phrase undergraduate +:+ S "level 2 or higher" +:+ phrase solidMechanics]
        [phrase soilMechanics]
    , IOrgSec inModel (SRS.inModel [] []) orgSecEnd
    ],
    --FIXME: issue #235
  GSDSec $ GSDProg
    [ SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList]
    , UsrChars [userCharIntro], SystCons [sysConstraints] []
    ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg purp []
        [ TermsAndDefs Nothing terms
        , PhySysDesc progName physSystParts figPhysSyst physSystContents 
        , Goals goalsInputs]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs instModIntro ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inputsWUncrtn --FIXME: issue #295
        , CorrSolnPpties outputs []
        ]
      ],
  ReqrmntSec $ ReqsProg
    [ FReqsSub' funcReqTables
    , NonFReqsSub
    ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $ AuxConsProg progName [],
  Bibliography]

purp :: Sentence
purp = foldlSent_ [S "evaluate the", phrase fs `S.ofA` phrasePoss slope,
  phrase slpSrf `S.and_` S "identify", phraseNP (crtSlpSrf `the_ofThe` slope) `sC`
  S "as well as the", phrase intrslce, phraseNP (normForce `and_` shearForce),
  S "along the", phrase crtSlpSrf]

units :: [UnitDefn]
units = map unitWrapper [metre, degree, kilogram, second] ++ map unitWrapper [newton, pascal]

concIns :: [ConceptInstance]
concIns = goals ++ assumptions ++ funcReqs ++ nonFuncReqs ++ likelyChgs ++ unlikelyChgs

labCon :: [LabelledContent]
labCon = [figPhysSyst, figIndexConv, figForceActing] ++ funcReqTables

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

ideaDicts :: [IdeaDict]
ideaDicts = 
  -- Actual IdeaDicts
  doccon ++ prodtcon ++ defs ++ educon ++ compcon ++
  -- CIs
  nw progName : map nw acronyms ++ map nw mathcon' ++ map nw doccon' ++
  -- ConceptChunks
  nw algorithm : map nw defs' ++ map nw softwarecon ++ map nw physicCon ++
  map nw mathcon ++ map nw solidcon ++ map nw physicalcon ++
  -- DefinedQuantityDicts
  map nw symbols ++
  -- UnitDefns
  map nw derived ++ map nw fundamentals ++ map nw units

symbMap :: ChunkDB
symbMap = cdb (map (^. output) iMods ++ map qw symbols) ideaDicts
  (map cw iMods ++ map cw symbols ++ srsDomains) units dataDefs iMods
  generalDefinitions tMods concIns labCon allRefs citations

tableOfAbbrvsIdeaDicts :: [IdeaDict]
tableOfAbbrvsIdeaDicts =
  -- CIs
  map nw acronyms ++
  -- ConceptChunks
  nw progName :
  -- DefinedQuantityDicts
  map nw symbols

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) tableOfAbbrvsIdeaDicts
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] ([] :: [Reference]) []

-- | Holds all references and links used in the document.
allRefs :: [Reference]
allRefs = [externalLinkRef, weightSrc, hsPressureSrc]

-- SECTION 1 --
--automatically generated in mkSRS -

-- SECTION 1.1 --
--automatically generated in mkSRS

-- SECTION 1.2 --
--automatically generated in mkSRS using the intro below
tableOfSymbIntro :: [TSIntro]
tableOfSymbIntro = [TSPurpose, TypogConvention [Verb $ foldlSent_
  [S "a subscript", ch index, S "indicates that the", phrase value, 
  S "will be taken at" `sC` S "and analyzed at, a", phrase slice `S.or_` phrase slice, 
  S "interface composing the total slip", phrase mass]], VectorUnits]

-- SECTION 1.3 --
--automatically generated in mkSRS

-- SECTION 2 --
startIntro, kSent :: Sentence
startIntro = foldlSent [atStartNP (a_ slope), S "of geological",
  phrase mass `sC` S "composed of", phrase soil, S "and rock and sometimes",
  S "water" `sC` S "is subject" `S.toThe` S "influence" `S.of_` (phraseNP (gravity `onThe` mass) !.),
  S "This can cause instability" `S.inThe` S "form" `S.of_` phrase soil +:+.
  S "or rock movement", atStartNP' (NP.the (effect `of_PS` soil)),
  S "or rock movement can range from inconvenient to",
  S "seriously hazardous" `sC` S "resulting in significant life and economic" +:+.
  plural loss, atStart slope, S "stability is of", phrase interest,
  S "both when analysing natural", plural slope `sC`
  S "and when designing an excavated" +:+. phrase slope, atStart ssa `S.isThe`
  S "assessment" `S.ofThe` S "safety" `S.ofA` phrase slope `sC`
  S "identifying the", phrase surface,
  S "most likely to experience", phrase slip `S.and_`
  S "an index" `S.of_` S "its relative stability known as the" +:+. phrase fs]

kSent = keySent ssa progName

keySent :: (Idea a, Idea b) => a -> b -> Sentence
keySent probType pname = foldlSent_ [(phraseNP (NP.a_ (combineNINI probType problem)) !.),
  S "The developed", phrase program, S "will be referred to as the",
  introduceAbb pname,
  S "based on the original, manually created version of" +:+
  namedRef externalLinkRef (S "SSP")]

externalLinkRef :: Reference
externalLinkRef = makeURI "SSP" 
  "https://github.com/smiths/caseStudies/blob/master/CaseStudies/ssp" 
  (shortname' $ S "SSP")
  
-- SECTION 2.1 --
-- Purpose of Document automatically generated in IPurpose


-- SECTION 2.2 --
-- Scope of Requirements automatically generated in IScope
scope :: Sentence
scope = foldlSent_ [phraseNP (stabAnalysis `ofA` twoD), sParen (short twoD),
  phraseNP (combineNINI soil mass) `sC` S "composed of a single homogeneous", phrase layer,
  S "with" +:+. pluralNP (combineNINI constant mtrlPrpty), atStartNP (NP.the (combineNINI soil mass))
  `S.is` S "assumed to extend infinitely in the third" +:+.
  phrase dimension, atStartNP (the analysis), S "will be at an instant" `S.in_`
  phrase time :+: S ";", plural factor, S "that may change the", plural soilPrpty,
  S "over", phrase time, S "will not be considered"]

-- SECTION 2.3 --
-- Characteristics of the Intended Reader generated in IChar

-- SECTION 2.4 --
-- Organization automatically generated in IOrgSec
orgSecEnd :: Sentence
orgSecEnd = foldlSent_ [atStartNP' (the inModel), S "provide the set of",
  S "algebraic", plural equation, S "that must be solved"]

-- SECTION 3 --
-- SECTION 3.1 --
-- System Context automatically generated
sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [refS sysCtxFig1 +:+ S "shows the" +:+. phrase sysCont,
   S "A circle represents an external entity outside the" +:+. phrase software, S "A rectangle represents the",
   phrase softwareSys, S "itself" +:+. sParen (short progName),
   S "Arrows are used to show the data flow between the" +:+ phraseNP (system
   `andIts` environment)]
   
sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) (resourcePath ++ "SystemContextFigure.png")

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol
  [S "The responsibilities" `S.ofThe` phraseNP (user `andThe` system),
   S "are as follows"]
   
sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide" +:+ phraseNP (the input_) +:+ S "data related to" +:+
  phraseNP (the soilLyr) :+: S "(s) and water table (if applicable)" `sC`
  S "ensuring conformation to" +:+ phrase input_ +:+ S "data format" +:+
  S "required by" +:+ short progName,
  S "Ensure that consistent units are used for" +:+ pluralNP (combineNINI input_ variable),
  S "Ensure required" +:+ namedRef (SRS.assumpt [] []) (pluralNP (combineNINI software assumption)) 
  +:+ S "are" +:+ S "appropriate for the" +:+ phrase problem +:+ S "to which the" +:+ 
  phrase user +:+ S "is applying the" +:+ phrase software]
  
sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Detect data" +:+ phrase type_ +:+ S "mismatch, such as" +:+
  S "a string of characters" +:+ phrase input_ +:+ S "instead of a floating" +:+
  S "point" +:+ phrase number,
  S "Verify that the" +:+ plural input_ +:+ S "satisfy the required" +:+
  phrase physical `S.and_` S "other" +:+ namedRef (SRS.datCon [] []) (plural datumConstraint),
  S "Identify the" +:+ phrase crtSlpSrf +:+ S "within the possible" +:+
  phrase input_ +:+ S "range",
  S "Find the" +:+ phrase fsConcept +:+ S "for the" +:+ phrase slope,
  S "Find the" +:+ phrase intrslce +:+ phraseNP (normForce `and_` shearForce) +:+ S "along the" +:+ phrase crtSlpSrf]
  
sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short progName +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]

-- SECTION 3.2 --
-- User Characteristics automatically generated with the
-- userContraints intro below

userCharIntro :: Contents
userCharIntro = userChar progName [S "Calculus", titleize Doc.physics]
  [phrase soil, plural mtrlPrpty] [phrase effCohesion, phrase fricAngle, 
  S "unit weight"]

userChar :: (Idea a) => a -> [Sentence] -> [Sentence] -> [Sentence] -> Contents
userChar pname understandings familiarities specifics = foldlSP [
  atStartNP (the endUser) `S.of_` short pname,
  S "should have an understanding" `S.of_` S "undergraduate Level 1",
  foldlList Comma List understandings `sC`
  S "and be familiar with", foldlList Comma List familiarities `sC` 
  S "specifically", foldlList Comma List specifics]

-- SECTION 3.2 --
sysConstraints :: Contents
sysConstraints = foldlSP [atStartNP (NP.the (combineNINI morPrice method_)), 
  refS morgenstern1965 `sC` S "which involves dividing the", phrase slope,
  S "into vertical", plural slice `sC` S "will be used to derive the",
  plural equation, S "for analysing the", phrase slope]

-- SECTION 4 --

-- SECTION 4.1 --

-- Introduction of the Problem Description section derives from purp

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
  [atStartNP (a_ slope), S "comprised of one", phrase soilLyr],
  [atStartNP (a_ waterTable) `sC` S "which may or may not exist"]]

figPhysSyst :: LabelledContent
figPhysSyst = llcc (makeFigRef "PhysicalSystem") $
  fig (foldlSent_ [S "An example", phraseNP (slope `for` analysis),
  S "by", short progName `sC` S "where the dashed line represents the",
  phrase waterTable]) (resourcePath ++ "PhysSyst.png")

physSystContents :: [Contents]
physSystContents = [physSysConv, LlC figIndexConv, physSysFbd, LlC figForceActing]

physSysConv :: Contents
physSysConv = foldlSP [atStart morPrice, phrase analysis, refS morgenstern1965
  `S.ofThe` phrase slope, S "involves representing the", phrase slope,
  S "as a series of vertical" +:+. plural slice, S "As shown in",
  refS figIndexConv `sC` phraseNP (the index), ch index, S "is used to denote a",
  phrase value `S.for` S "a single", phrase slice `sC` S "and an", phrase intrslce, 
  phrase value, S "at a given", phrase index, ch index, S "refers to the",
  phrase value, S "between", phrase slice, ch index `S.and_` S "adjacent", phrase slice,
  eS $ sy index $+ int 1]

figIndexConv :: LabelledContent
figIndexConv = llcc (makeFigRef "IndexConvention") $ 
  fig (foldlSent_ [S "Index convention for", phraseNP (slice `and_` 
  intrslce), plural value]) (resourcePath ++ "IndexConvention.png")

physSysFbd :: Contents
physSysFbd = foldlSP [atStartNP' (NP.a_ (fbd `ofThe` force)), S "acting on a",
  phrase slice `S.is` S "displayed in" +:+. refS figForceActing, S "The specific",
  pluralNP (force `and_PP` symbol_), S "will be discussed in detail in",
  refS (SRS.genDefn [] []) `S.and_` refS (SRS.dataDefn [] [])]

figForceActing :: LabelledContent
figForceActing = llcc (makeFigRef "ForceDiagram") $
  fig (atStartNP' (fbd `of_` force) +:+ S "acting on a" +:+
  phrase slice) (resourcePath ++ "ForceDiagram.png")

-- SECTION 4.1.3 --
goalsInputs :: [Sentence]
goalsInputs = [phraseNP (the shape `NP.ofThe` combineNINI soil mass),
  S "location" `S.the_ofThe` phrase waterTable, pluralNP (mtrlPrpty `the_ofThePS` soil)]

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
