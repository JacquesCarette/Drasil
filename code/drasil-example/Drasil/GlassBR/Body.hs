module Drasil.GlassBR.Body where

import Control.Lens ((^.))
import Language.Drasil hiding (Symbol(..), organization, section)
import Language.Drasil.Code (relToQD)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _concepts, _constants, _constraints, _datadefs,
  _definitions, _defSequence, _inputs, _kind, _outputs, _quants, _sys,
  _sysinfodb, _usedinfodb)
import Theory.Drasil (Theory(defined_fun, defined_quant))
import Utils.Drasil

import Drasil.DocLang (AppndxSec(..), AuxConstntSec(..), DerivationDisplay(..), 
  DocSection(..), Field(..), Fields, GSDSec(GSDProg2), GSDSub(..),
  InclUnits(IncludeUnits), IntroSec(IntroProg), IntroSub(IChar, IOrgSec, IPurpose, IScope), 
  ProblemDescription(..), PDSub(..), RefSec(RefProg), RefTab(TAandA, TUnits),
  ReqrmntSec(..), ReqsSub(..), SCSSub(..), SRSDecl, SSDSec(..), SSDSub(..),
  SolChSpec(..), StkhldrSec(StkhldrProg2), StkhldrSub(Client, Cstmr),
  TraceabilitySec(TraceabilityProg), TSIntro(SymbOrder, TSPurpose),
  Verbosity(Verbose), auxSpecSent, characteristicsLabel, intro, mkDoc,
  termDefnF', tsymb, traceMatStandard)

import qualified Drasil.DocLang.SRS as SRS (reference, assumpt, inModel)

import Data.Drasil.Concepts.Computation (computerApp, inDatum, compcon, algorithm)
import Data.Drasil.Concepts.Documentation as Doc (analysis, appendix, aspect,
  assumption, characteristic, company, condition, content, dataConst, datum,
  definition, doccon, doccon', document, emphasis, environment, goal,
  information, input_, interface, model, organization, physical, problem,
  product_, purpose, reference, software, softwareConstraint, softwareSys,
  srsDomains, standard, sysCont, system, template, term_, user, value, variable)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs, code)
import Data.Drasil.IdeaDicts as Doc (inModel, thModel)
import qualified Data.Drasil.IdeaDicts as Doc (dataDefn)
import Data.Drasil.Concepts.Education as Edu (civilEng, scndYrCalculus, structuralMechanics,
  educon)
import Data.Drasil.Concepts.Math (graph, mathcon, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (dimension, physicalcon, materialProprty)
import Data.Drasil.Concepts.Physics (distance)
import Data.Drasil.Concepts.Software (correctness, verifiability,
  understandability, reusability, maintainability, portability, softwarecon)
import Data.Drasil.Software.Products (sciCompS)

import Data.Drasil.Citations (koothoor2013, smithLai2005)
import Data.Drasil.People (mCampidelli, nikitha, spencerSmith)
import Data.Drasil.SI_Units (kilogram, metre, newton, pascal, second, fundamentals,
  derived)

import Drasil.GlassBR.Assumptions (assumptionConstants, assumptions)
import Drasil.GlassBR.Changes (likelyChgs, unlikelyChgs)
import Drasil.GlassBR.Concepts (acronyms, blastRisk, glaPlane, glaSlab, glassBR, 
  ptOfExplsn, con, con')
import Drasil.GlassBR.DataDefs (qDefns)
import qualified Drasil.GlassBR.DataDefs as GB (dataDefs)
import Drasil.GlassBR.Figures
import Drasil.GlassBR.Goals (goals)
import Drasil.GlassBR.IMods (symb, iMods, instModIntro)
import Drasil.GlassBR.References (astm2009, astm2012, astm2016, citations, rbrtsn2012)
import Drasil.GlassBR.Requirements (funcReqs, funcReqsTables, nonfuncReqs)
import Drasil.GlassBR.Symbols (symbolsForTable, thisSymbols)
import Drasil.GlassBR.TMods (tMods)
import Drasil.GlassBR.Unitals (blast, blastTy, bomb, explosion, constants,
  constrained, inputDataConstraints, inputs, outputs, specParamVals, glassTy,
  glassTypes, glBreakage, lateralLoad, load, loadTypes, pbTol, probBr, probBreak,
  sD, termsWithAccDefn, termsWithDefsOnly, terms)

srs :: Document
srs = mkDoc mkSRS (for'' titleize phrase) si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

si :: SystemInformation
si = SI {
  _sys         = glassBR,
  _kind        = Doc.srs,
  _authors     = [nikitha, spencerSmith],
  _quants      = symbolsForTable,
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = map (relToQD symbMap) iMods ++ 
                 concatMap (^. defined_quant) tMods ++
                 concatMap (^. defined_fun) tMods,
  _datadefs    = GB.dataDefs,
  _inputs      = inputs,
  _outputs     = outputs,
  _defSequence = qDefns,
  _constraints = constrained,
  _constants   = constants,
  _sysinfodb   = symbMap,
  _usedinfodb = usedDB,
   refdb       = refDB
}
  --FIXME: All named ideas, not just acronyms.

mkSRS :: SRSDecl
mkSRS = [RefSec $ RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA],
  IntroSec $
    IntroProg (startIntro software blstRskInvWGlassSlab glassBR)
      (short glassBR)
    [IPurpose $ purpOfDocIntro document glassBR glaSlab,
     IScope scope,
     IChar [] (undIR ++ appStanddIR) [],
     IOrgSec orgOfDocIntro Doc.dataDefn (SRS.inModel [] []) orgOfDocIntroEnd],
  StkhldrSec $
    StkhldrProg2
      [Client glassBR $ S "a" +:+ phrase company
        +:+ S "named Entuitive. It is developed by Dr." +:+ (S $ name mCampidelli),
      Cstmr glassBR],
  GSDSec $ GSDProg2 [SysCntxt [sysCtxIntro, LlC sysCtxFig, sysCtxDesc, sysCtxList],
    UsrChars [userCharacteristicsIntro], SystCons [] [] ],
  SSDSec $
    SSDProg
      [SSDProblem $ PDProg prob [termsAndDesc]
        [ PhySysDesc glassBR physSystParts physSystFig []
        , Goals goalInputs],
       SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] [] HideDerivation -- No Gen Defs for GlassBR
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) HideDerivation
        , Constraints auxSpecSent inputDataConstraints
        , CorrSolnPpties [probBr] []
        ]
      ],
  ReqrmntSec $ ReqsProg [
    FReqsSub funcReqsTables,
    NonFReqsSub
  ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $ AuxConsProg glassBR auxiliaryConstants,
  Bibliography,
  AppndxSec $ AppndxProg [appdxIntro, LlC demandVsSDFig, LlC dimlessloadVsARFig]]

symbMap :: ChunkDB
symbMap = cdb thisSymbols (map nw acronyms ++ map nw thisSymbols ++ map nw con
  ++ map nw con' ++ map nw terms ++ map nw doccon ++ map nw doccon' ++ map nw educon
  ++ [nw sciCompS] ++ map nw compcon ++ map nw mathcon ++ map nw mathcon'
  ++ map nw softwarecon ++ map nw terms ++ [nw lateralLoad, nw materialProprty]
   ++ [nw distance, nw algorithm] ++
  map nw fundamentals ++ map nw derived ++ map nw physicalcon)
  (map cw symb ++ Doc.srsDomains) (map unitWrapper [metre, second, kilogram]
  ++ map unitWrapper [pascal, newton]) GB.dataDefs iMods [] tMods concIns section
  labCon

concIns :: [ConceptInstance]
concIns = assumptions ++ goals ++ likelyChgs ++ unlikelyChgs ++ funcReqs ++ nonfuncReqs

labCon :: [LabelledContent]
labCon = funcReqsTables ++ [demandVsSDFig, dimlessloadVsARFig]

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw thisSymbols)
 ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] []

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
  noRefs $ map tAndDOnly termsWithDefsOnly
  ++
  termsAndDescBulletsGlTySubSec
  ++
  termsAndDescBulletsLoadSubSec
  ++
  map tAndDWAcc termsWithAccDefn
  ++
  [tAndDWSym probBreak probBr]
   --FIXME: merge? Needs 2 arguments because there is no instance for (SymbolForm ConceptChunk)...

termsAndDescBulletsGlTySubSec, termsAndDescBulletsLoadSubSec :: [ItemType]

termsAndDescBulletsGlTySubSec = [Nested (titleize glassTy :+: S ":") $
  Bullet $ noRefs $ map tAndDWAcc glassTypes]

termsAndDescBulletsLoadSubSec = [Nested (atStart load `sDash` (load ^. defn)) $
  Bullet $ noRefs $ map tAndDWAcc (take 2 loadTypes)
  ++
  map tAndDOnly (drop 2 loadTypes)]

solChSpecSubsections :: [CI]
solChSpecSubsections = [thModel, inModel, Doc.dataDefn, dataConst]

--Used in "Values of Auxiliary Constants" Section--
auxiliaryConstants :: [QDefinition]
auxiliaryConstants = assumptionConstants ++ specParamVals

--Used in "Non-Functional Requirements" Section--
priorityNFReqs :: [ConceptChunk]
priorityNFReqs = [correctness, verifiability, understandability,
  reusability, maintainability, portability]

--------------------------------------------------------------------------------

{--INTRODUCTION--}

startIntro :: NamedChunk -> Sentence -> CI -> Sentence
startIntro prgm sfwrPredicts progName = foldlSent [
  atStart prgm, S "is helpful to efficiently" `sAnd` S "correctly predict the"
  +:+. sfwrPredicts, underConsidertn blast,
  S "The", phrase prgm `sC` S "herein called", short progName `sC`
  S "aims to predict the", sfwrPredicts, S "using an intuitive",
  phrase interface]

undIR, appStanddIR :: [Sentence]
undIR = [phrase scndYrCalculus, phrase structuralMechanics, phrase glBreakage,
  phrase blastRisk, plural computerApp `sIn` phrase Edu.civilEng]
appStanddIR = [S "applicable" +:+ plural standard +:+
  S "for constructions using glass from" +:+ foldlList Comma List
  (map makeCiteS [astm2009, astm2012, astm2016]) `sIn`
  makeRef2S (SRS.reference ([]::[Contents]) ([]::[Section]))]

scope :: Sentence
scope = foldlSent_ [S "determining the safety of a", phrase glaSlab,
  S "under a", phrase blast, S "loading following the ASTM", phrase standard,
  sParen $ makeRef2S astm2009]

{--Purpose of Document--}

purpOfDocIntro :: NamedChunk -> CI -> NamedChunk -> Sentence
purpOfDocIntro typeOf progName gvnVar = foldlSent [S "The main", phrase purpose,
  S "of this", phrase typeOf, S "is to predict whether a given", phrase gvnVar,
  S "is likely to resist a specified" +:+. phrase blast, S "The", plural Doc.goal
  `sAnd` plural thModel, S "used in the", short progName, phrase Doc.code,
  S "are provided" `sC` S "with an", phrase emphasis,
  S "on explicitly identifying", plural assumption `sAnd` S "unambiguous" +:+.
  plural definition, S "This", phrase typeOf, S "is intended to be used as a",
  phrase reference, S "to provide all", phrase information,
  S "necessary to understand" `sAnd` S "verify the" +:+. phrase analysis,
  S "The", short Doc.srs, S "is abstract because the", plural content, S "say what",
  phrase problem, S "is being solved" `sC` S "but not how to solve it"]
  --FIXME: Last sentence is also present in SSP, SWHS and NoPCM... pull out?

{--Scope of Requirements--}

{--Organization of Document--}

orgOfDocIntro, orgOfDocIntroEnd :: Sentence
orgOfDocIntro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the", phrase template, S "for an", short Doc.srs,
  S "for", phrase sciCompS, S "proposed by" +:+ makeCiteS koothoor2013
  `sAnd` makeCiteS smithLai2005 `sC` S "with some", 
  plural aspect, S "taken from Volere", phrase template,
  S "16", makeCiteS rbrtsn2012]

orgOfDocIntroEnd = foldlSent_ [atStartNP' (the Doc.dataDefn) `sAre`
  S "used to support", plural definition `ofThe` S "different", plural model]

{--STAKEHOLDERS--}

{--The Client--}
{--The Customer--}

{--GENERAL SYSTEM DESCRIPTION--}

{--System Context--}
  
sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [makeRef2S sysCtxFig +:+ S "shows the" +:+. phrase sysCont,
   S "A circle represents an external entity outside the" +:+ phrase software
   `sC` S "the", phrase user, S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself", sParen (short glassBR) +:+. EmptyS,
   S "Arrows are used to show the data flow between the" +:+ phrase system,
   S "and its" +:+ phrase environment]

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol
  [S "The interaction between the", phrase product_, S "and the", phrase user,
   S "is through a user" +:+. phrase interface,
   S "The responsibilities of the", phrase user, S "and the", phrase system,
   S "are as follows"]
   
sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide the" +:+ plural inDatum +:+ S "related to the" +:+
  phrase glaSlab `sAnd` phrase blastTy `sC` S "ensuring no errors in the" +:+
  plural datum +:+. S "entry",
  S "Ensure that consistent units are used for" +:+ phrase input_ +:+. plural variable,
  S "Ensure required" +:+ phrase software +:+ plural assumption +:+
    sParen (makeRef2S $ SRS.assumpt ([]::[Contents]) ([]::[Section]))
    +:+ S "are appropriate for any particular" +:+
    phrase problem +:+ S "input to the" +:+. phrase software]

sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Detect data type mismatch, such as a string of characters" +:+
  phrase input_ +:+. S "instead of a floating point number",
  S "Determine if the" +:+ plural input_ +:+ S "satisfy the required" +:+.
  (phrase physical `sAnd` plural softwareConstraint),
  S "Predict whether the" +:+ phrase glaSlab +:+. S "is safe or not"]
  
sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short glassBR +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]
   
{--User Characteristics--}

userCharacteristicsIntro :: Contents
userCharacteristicsIntro = LlC $ enumBullet characteristicsLabel $ map foldlSent
  [[S "The end user of GlassBR is expected to have completed at least the",
    S "equivalent of the second year of an undergraduate degree in civil engineering or structural engineering"],
  [S "The end user is expected to have an understanding of theory behind glass",
    S "breakage and blast risk"],
  [S "The end user is expected to have basic computer literacy to handle the software"]]

{--System Constraints--}

{--SPECIFIC SYSTEM DESCRIPTION--}

--Automatically generated

{--PROBLEM DESCRIPTION--}

prob :: Sentence
prob = foldlSent_ [S "efficiently" `sAnd` S "correctly predict whether a",
  phrase glaSlab, S "can withstand a", phrase blast, S "under given",
  plural condition]

{--Terminology and Definitions--}

termsAndDesc :: Section
termsAndDesc = termDefnF' (Just (S "All" `sOf` S "the" +:+ plural term_ +:+
  S "are extracted from" +:+ makeCiteS astm2009 `sIn`
  makeRef2S (SRS.reference ([]::[Contents]) ([]::[Section])))) [termsAndDescBullets]

{--Physical System Description--}

physSystParts :: [Sentence]
physSystParts = [S "The" +:+. phrase glaSlab,
  foldlSent [S "The" +:+. phrase ptOfExplsn, S "Where the", phrase bomb `sC`
  S "or", (blast ^. defn) `sC` S "is located. The", phrase sD `isThe`
  phrase distance, S "between the", phrase ptOfExplsn `sAnd` S "the glass"]]

{--Goal Statements--}

goalInputs :: [Sentence]
goalInputs = [plural dimension `ofThe` phrase glaPlane, S "the" +:+ phrase glassTy,
  plural characteristic `ofThe` phrase explosion, S "the" +:+ phrase pbTol]

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
  sParen (makeRef2S demandVsSDFig `sAnd` makeRef2S dimlessloadVsARFig),
  S "used for interpolating", plural value, S "needed in the", plural model]

blstRskInvWGlassSlab :: Sentence
blstRskInvWGlassSlab = phrase blastRisk +:+ S "involved with the" +:+
  phrase glaSlab
