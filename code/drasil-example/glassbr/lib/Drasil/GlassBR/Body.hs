{-# LANGUAGE PostfixOperators #-}
module Drasil.GlassBR.Body where

import Control.Lens ((^.))
import Language.Drasil hiding (organization, section, variable)
import Drasil.SRSDocument
import Drasil.DocLang (auxSpecSent, termDefnF')
import qualified Drasil.DocLang.SRS as SRS (reference, assumpt, inModel)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Computation (computerApp, inDatum, compcon, algorithm)
import Data.Drasil.Concepts.Documentation as Doc (appendix, assumption,
  characteristic, company, condition, dataConst, datum, doccon, doccon',
  environment, input_, interface, model, physical, problem, product_,
  software, softwareConstraint, softwareSys, srsDomains, standard, sysCont,
  system, term_, user, value, variable, reference, definition)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.TheoryConcepts as Doc (dataDefn, inModel, thModel)
import Data.Drasil.Concepts.Education as Edu (civilEng, scndYrCalculus, structuralMechanics,
  educon)
import Data.Drasil.Concepts.Math (graph, mathcon, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (dimension, physicalcon, materialProprty)
import Data.Drasil.Concepts.Physics (distance)
import Data.Drasil.Concepts.Software (correctness, verifiability,
  understandability, reusability, maintainability, portability, softwarecon)
import Data.Drasil.Software.Products (sciCompS)

import Data.Drasil.People (mCampidelli, nikitha, spencerSmith)
import Data.Drasil.SI_Units (kilogram, metre, newton, pascal, second, fundamentals,
  derived)

import Drasil.GlassBR.Assumptions (assumptionConstants, assumptions)
import Drasil.GlassBR.Changes (likelyChgs, unlikelyChgs)
import Drasil.GlassBR.Concepts (acronyms, blastRisk, glaPlane, glaSlab, glassBR, 
  ptOfExplsn, con, con', glass)
import Drasil.GlassBR.DataDefs (qDefns, configFp)
import qualified Drasil.GlassBR.DataDefs as GB (dataDefs)
import Drasil.GlassBR.Figures
import Drasil.GlassBR.Goals (goals)
import Drasil.GlassBR.IMods (symb, iMods, instModIntro)
import Drasil.GlassBR.References (astm2009, astm2012, astm2016, citations)
import Drasil.GlassBR.Requirements (funcReqs, inReqDesc, funcReqsTables, nonfuncReqs)
import Drasil.GlassBR.Symbols (symbolsForTable, thisSymbols)
import Drasil.GlassBR.TMods (tMods)
import Drasil.GlassBR.Unitals (blast, blastTy, bomb, explosion, constants,
  constrained, inputDataConstraints, inputs, outputs, specParamVals, glassTy,
  glassTypes, glBreakage, lateralLoad, load, loadTypes, pbTol, probBr, stressDistFac, probBreak,
  sD, termsWithAccDefn, termsWithDefsOnly, terms)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

si :: SystemInformation
si = SI {
  _sys         = glassBR,
  _kind        = Doc.srs,
  _authors     = [nikitha, spencerSmith],
  _purpose     = [purp],
  _background  = [],
  _quants      = symbolsForTable,
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = iMods,
  _datadefs    = GB.dataDefs,
  _configFiles = configFp,
  _inputs      = inputs,
  _outputs     = outputs,
  _defSequence = qDefns,
  _constraints = constrained,
  _constants   = constants,
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}
  --FIXME: All named ideas, not just acronyms.

mkSRS :: SRSDecl
mkSRS = [TableOfContents,
  RefSec $ RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA],
  IntroSec $
    IntroProg (startIntro software blstRskInvWGlassSlab glassBR)
      (short glassBR)
    [IPurpose $ purpDoc glassBR Verbose,
     IScope scope,
     IChar [] (undIR ++ appStanddIR) [],
     IOrgSec Doc.dataDefn (SRS.inModel [] []) orgOfDocIntroEnd],
  StkhldrSec $
    StkhldrProg
      [Client glassBR $ phraseNP (a_ company)
        +:+. S "named Entuitive" +:+ S "It is developed by Dr." +:+ S (name mCampidelli),
      Cstmr glassBR],
  GSDSec $ GSDProg [SysCntxt [sysCtxIntro, LlC sysCtxFig, sysCtxDesc, sysCtxList],
    UsrChars [userCharacteristicsIntro], SystCons [] [] ],
  SSDSec $
    SSDProg
      [SSDProblem $ PDProg purp [termsAndDesc]
        [ PhySysDesc glassBR physSystParts physSystFig []
        , Goals goalInputs],
       SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] [] HideDerivation -- No Gen Defs for GlassBR
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) HideDerivation
        , Constraints auxSpecSent inputDataConstraints
        , CorrSolnPpties [probBr, stressDistFac] []
        ]
      ],
  ReqrmntSec $ ReqsProg [
    FReqsSub inReqDesc funcReqsTables,
    NonFReqsSub
  ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $ AuxConsProg glassBR auxiliaryConstants,
  Bibliography,
  AppndxSec $ AppndxProg [appdxIntro, LlC demandVsSDFig, LlC dimlessloadVsARFig]]

purp :: Sentence
purp = foldlSent_ [S "efficiently" `S.and_` S "correctly predict whether a",
  phrase glaSlab, S "can withstand a", phrase blast, S "under given",
  plural condition]

symbMap :: ChunkDB
symbMap = cdb thisSymbols (map nw acronyms ++ map nw thisSymbols ++ map nw con
  ++ map nw con' ++ map nw terms ++ map nw doccon ++ map nw doccon' ++ map nw educon
  ++ [nw sciCompS] ++ map nw compcon ++ map nw mathcon ++ map nw mathcon'
  ++ map nw softwarecon ++ map nw terms ++ [nw lateralLoad, nw materialProprty]
   ++ [nw distance, nw algorithm] ++
  map nw fundamentals ++ map nw derived ++ map nw physicalcon)
  (map cw symb ++ terms ++ Doc.srsDomains) (map unitWrapper [metre, second, kilogram]
  ++ map unitWrapper [pascal, newton]) GB.dataDefs iMods [] tMods concIns section
  labCon []

concIns :: [ConceptInstance]
concIns = assumptions ++ goals ++ likelyChgs ++ unlikelyChgs ++ funcReqs ++ nonfuncReqs

labCon :: [LabelledContent]
labCon = funcReqsTables ++ [demandVsSDFig, dimlessloadVsARFig]

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw thisSymbols)
 ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb citations concIns

section :: [Section]
section = extractSection srs

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

--------------------------------------------------------------------------------
termsAndDescBullets :: Contents
termsAndDescBullets = UlC $ ulcc $ Enumeration$ 
  Numeric $
    noRefs $
      map tAndDOnly termsWithDefsOnly 
      ++ termsAndDescBulletsGlTySubSec 
      ++ termsAndDescBulletsLoadSubSec 
      ++ map tAndDWAcc termsWithAccDefn 
      ++ [tAndDWSym probBreak probBr]
   --FIXME: merge? Needs 2 arguments because there is no instance for (SymbolForm ConceptChunk)...

termsAndDescBulletsGlTySubSec, termsAndDescBulletsLoadSubSec :: [ItemType]

termsAndDescBulletsGlTySubSec = [Nested (EmptyS +: titleize glassTy) $
  Bullet $ noRefs $ map tAndDWAcc glassTypes]

termsAndDescBulletsLoadSubSec = [Nested (atStart load `sDash` capSent (load ^. defn) !.) $
  Bullet $ noRefs $ map tAndDWAcc (take 2 loadTypes)
  ++
  map tAndDOnly (drop 2 loadTypes)]

solChSpecSubsections :: [CI]
solChSpecSubsections = [thModel, inModel, Doc.dataDefn, dataConst]

--Used in "Values of Auxiliary Constants" Section--
auxiliaryConstants :: [ConstQDef]
auxiliaryConstants = assumptionConstants ++ specParamVals

--Used in "Non-Functional Requirements" Section--
priorityNFReqs :: [ConceptChunk]
priorityNFReqs = [correctness, verifiability, understandability,
  reusability, maintainability, portability]

--------------------------------------------------------------------------------

{--INTRODUCTION--}

startIntro :: NamedChunk -> Sentence -> CI -> Sentence
startIntro prgm sfwrPredicts progName = foldlSent [
  atStart prgm, S "is helpful to efficiently" `S.and_` S "correctly predict the"
  +:+. sfwrPredicts, underConsidertn blast,
  atStartNP (the prgm) `sC` S "herein called", short progName `sC`
  S "aims to predict the", sfwrPredicts, S "using an intuitive",
  phrase interface]

undIR, appStanddIR :: [Sentence]
undIR = [phrase scndYrCalculus, phrase structuralMechanics, phrase glBreakage,
  phrase blastRisk, pluralNP (computerApp `in_PS` Edu.civilEng)]
appStanddIR = [S "applicable" +:+ plural standard +:+
  S "for constructions using glass from" +:+ foldlList Comma List
  (map refS [astm2009, astm2012, astm2016]) `S.in_`
  namedRef (SRS.reference ([]::[Contents]) ([]::[Section])) (plural reference)]

scope :: Sentence
scope = foldlSent_ [S "determining the safety of a", phrase glaSlab,
  S "under a", phrase blast, S "loading following the ASTM", phrase standard,
  sParen $ refS astm2009]

{--Purpose of Document--}
-- Purpose of Document automatically generated in IPurpose


{--Scope of Requirements--}

{--Organization of Document--}

orgOfDocIntroEnd :: Sentence
orgOfDocIntroEnd = foldlSent_ [atStartNP' (the dataDefn) `S.are`
  S "used to support", plural definition `S.the_ofThe` S "different", plural model]

{--STAKEHOLDERS--}

{--The Client--}
{--The Customer--}

{--GENERAL SYSTEM DESCRIPTION--}

{--System Context--}
  
sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [refS sysCtxFig +:+ S "shows the" +:+. phrase sysCont,
   S "A circle represents an external entity outside the" +:+ phrase software
   `sC` phraseNP (the user), S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself", (sParen (short glassBR) !.),
   S "Arrows are used to show the data flow between the" +:+ phraseNP (system
   `andIts` environment)]

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol
  [S "The interaction between the", phraseNP (product_ `andThe` user),
   S "is through a user" +:+. phrase interface,
   S "The responsibilities of the", phraseNP (user `andThe` system),
   S "are as follows"]
   
sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide the" +:+ plural inDatum +:+ S "related to the" +:+
  phraseNP (glaSlab `and_` blastTy) `sC` S "ensuring no errors in the" +:+
  plural datum +:+. S "entry",
  S "Ensure that consistent units are used for" +:+. pluralNP (combineNINI input_ variable),
  S "Ensure required" +:+ 
  namedRef (SRS.assumpt [] []) (pluralNP (combineNINI software assumption))
    +:+ S "are appropriate for any particular" +:+
    phrase problem +:+ S "input to the" +:+. phrase software]

sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Detect data type mismatch, such as a string of characters" +:+
  phrase input_ +:+. S "instead of a floating point number",
  S "Determine if the" +:+ plural input_ +:+ S "satisfy the required" +:+.
  pluralNP (physical `and_` softwareConstraint),
  S "Predict whether the" +:+ phrase glaSlab +:+. S "is safe or not"]
  
sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short glassBR +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]
   
{--User Characteristics--}

userCharacteristicsIntro :: Contents
userCharacteristicsIntro = enumBulletU $ map foldlSent
  [[S "The end user of GlassBR is expected to have completed at least the",
    S "equivalent of the second year of an undergraduate degree in civil engineering or structural engineering"],
  [S "The end user is expected to have an understanding of theory behind glass",
    S "breakage and blast risk"],
  [S "The end user is expected to have basic computer literacy to handle the software"]]

{--System Constraints--}

{--SPECIFIC SYSTEM DESCRIPTION--}

--Automatically generated

{--PROBLEM DESCRIPTION--}

--Introduction of Problem Description section derived from purp

{--Terminology and Definitions--}

termsAndDesc :: Section
termsAndDesc = termDefnF' (Just (S "All of the" +:+ plural term_ +:+
  S "are extracted from" +:+ refS astm2009)) [termsAndDescBullets]

{--Physical System Description--}

physSystParts :: [Sentence]
physSystParts = [(atStartNP (the glaSlab)!.),
  foldlSent [(atStartNP (the ptOfExplsn) !.), S "Where the", phrase bomb `sC`
  S "or", (blast ^. defn) `sC` (S "is located" !.), atStartNP (the sD) `S.isThe`
  phrase distance, S "between the", phrase ptOfExplsn `S.and_` phraseNP (the glass)]]

{--Goal Statements--}

goalInputs :: [Sentence]
goalInputs = [pluralNP (dimension `the_ofThePS` glaPlane), phraseNP (the glassTy),
  pluralNP (characteristic `the_ofThePS` explosion), phraseNP (the pbTol)]

{--SOLUTION CHARACTERISTICS SPECIFICATION--}

--Automatically generated

{--Assumptions--}

{--Theoretical Models--}

{--Data Definitions--}

{--Data Constraints--}

{--REQUIREMENTS--}

{--Functional Requirements--}

{--Nonfunctional Requirements--}

{--LIKELY CHANGES--}

{--UNLIKELY CHANGES--}

{--TRACEABLITY MATRICES AND GRAPHS--}

{--VALUES OF AUXILIARY CONSTANTS--}

{--REFERENCES--}

{--APPENDIX--}

appdxIntro :: Contents
appdxIntro = foldlSP [
  S "This", phrase appendix, S "holds the", plural graph,
  sParen (refS demandVsSDFig `S.and_` refS dimlessloadVsARFig),
  S "used for interpolating", plural value, S "needed in the", plural model]

blstRskInvWGlassSlab :: Sentence
blstRskInvWGlassSlab = phrase blastRisk +:+ S "involved with the" +:+
  phrase glaSlab
