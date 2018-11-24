module Drasil.GlassBR.Body where

import Control.Lens ((^.))

import Language.Drasil hiding (organization)
import Language.Drasil.Code (CodeSpec, codeSpec, relToQD)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Language.Drasil.Development (UnitDefn, unitWrapper) -- FIXME

import Drasil.DocLang (AppndxSec(..), AuxConstntSec(..), DerivationDisplay(..), 
  DocDesc, DocSection(..), Field(..), Fields, GSDSec(GSDProg2), GSDSub(..), 
  InclUnits(IncludeUnits), IntroSec(IntroProg), IntroSub(IChar, IOrgSec, IPurpose, IScope), 
  LCsSec(..), ProblemDescription(..), RefSec(RefProg), RefTab(TAandA, TUnits), 
  ReqrmntSec(..), ReqsSub(FReqsSub, NonFReqsSub), ScpOfProjSec(ScpOfProjProg), SCSSub(..), 
  SSDSec(..), SSDSub(..), SolChSpec(..), StkhldrSec(StkhldrProg2), 
  StkhldrSub(Client, Cstmr), TraceabilitySec(TraceabilityProg), 
  TSIntro(SymbOrder, TSPurpose), UCsSec(..), Verbosity(Verbose), 
  dataConstraintUncertainty, goalStmtF, inDataConstTbl, intro, mkDoc, 
  outDataConstTbl, physSystDesc, termDefnF, traceGIntro, tsymb)

import qualified Drasil.DocLang.SRS as SRS (datConLabel, dataDefnLabel, indPRCaseLabel, 
  referenceLabel, valsOfAuxConsLabel, assumptLabel)

import Data.Drasil.Concepts.Computation (computerApp, inParam, compcon, algorithm)
import Data.Drasil.Concepts.Documentation as Doc (analysis, appendix, aspect, 
  assumption, characteristic, class_, code, company, condition, content, 
  dataConst, dataDefn, definition, document, emphasis, environment, figure, 
  goal, goalStmt, implementation, information, inModel, input_, interface, item, 
  likelyChg, model, organization, output_, physicalSystem, physSyst, problem, 
  product_, purpose, reference, requirement, reviewer, section_, software, 
  softwareSys, srs, srsDomains, standard, sysCont, system, template, term_,
  theory, thModel, traceyMatrix, user, userInput, value, doccon, doccon')
import Data.Drasil.Concepts.Education as Edu(civilEng, scndYrCalculus, structuralMechanics,
  educon)
import Data.Drasil.Concepts.Math (graph, parameter, probability, mathcon, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (dimension, physicalcon, materialProprty)
import Data.Drasil.Concepts.Physics (distance)
import Data.Drasil.Concepts.Software (correctness, verifiability,
  understandability, reusability, maintainability, portability,
  performance, softwarecon, program)
import Data.Drasil.Concepts.Thermodynamics (degree_')
import Data.Drasil.Software.Products (sciCompS)

import Data.Drasil.Citations (koothoor2013, smithLai2005)
import Data.Drasil.People (mCampidelli, nikitha, spencerSmith)
import Data.Drasil.Phrase (for'', the)
import Data.Drasil.SI_Units (kilogram, metre, newton, pascal, second, fundamentals,
  derived)
import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), 
  figureLabel, foldlList, foldlsC, foldlSent, foldlSP, foldlSPCol, 
  isThe, ofThe, sAnd, showingCxnBw, sIn, sOf, sOr, sVersus, tAndDOnly, tAndDWAcc, tAndDWSym, 
  underConsidertn)
import Data.Drasil.Utils (bulletFlat, bulletNested, enumBullet, enumSimple, itemRefToSent, 
  makeTMatrix, noRefs, prodUCTbl)
  
import Drasil.GlassBR.Assumptions (assumptionConstants, assumptions)
import Drasil.GlassBR.Changes (likelyChgs, likelyChgsList, unlikelyChgs,
  unlikelyChgsList)
import Drasil.GlassBR.Concepts (acronyms, aR, blastRisk, glaPlane, glaSlab, gLassBR, 
  ptOfExplsn, stdOffDist, glasscon, glasscon')
import Drasil.GlassBR.DataDefs (dataDefns, gbQDefns, probOfBreak)
import Drasil.GlassBR.IMods (glassBRsymb, calofCapacity, calofDemand, gbrIMods)
import Drasil.GlassBR.ModuleDefs (allMods)
import Drasil.GlassBR.References (astm2009, astm2012, astm2016, gbCitations, rbrtsn2012)
import Drasil.GlassBR.Requirements (funcReqsList, funcReqs)
import Drasil.GlassBR.Symbols (symbolsForTable, this_symbols)
import Drasil.GlassBR.TMods (gbrTMods)
import Drasil.GlassBR.Unitals (aspect_ratio, blast, blastTy, bomb, capacity, char_weight, 
  demand, demandq, dimlessLoad, explosion, gbConstants, gbConstrained, gbInputDataConstraints,
  gbInputs, gbOutputs, gBRSpecParamVals, glassGeo, glassTy, glassTypes, glBreakage,
  lateralLoad, load, loadTypes, pb_tol, prob_br, probBreak, sD, sdWithEqn, stressDistFac,
  termsWithAccDefn, termsWithDefsOnly, wtntWithEqn, terms)

{--}

gbSymbMap :: ChunkDB
gbSymbMap = cdb this_symbols (map nw acronyms ++ map nw this_symbols ++ map nw glasscon
  ++ map nw glasscon' ++ map nw terms ++ map nw doccon ++ map nw doccon' ++ map nw educon
  ++ [nw sciCompS] ++ map nw compcon ++ map nw mathcon ++ map nw mathcon'
  ++ map nw softwarecon ++ map nw terms ++ [nw lateralLoad, nw materialProprty]
   ++ [nw distance, nw algorithm] ++
  map nw fundamentals ++ map nw derived ++ map nw physicalcon)
  (map cw glassBRsymb ++ Doc.srsDomains) $ map unitWrapper [metre, second, kilogram]
  ++ map unitWrapper [pascal, newton]

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw this_symbols)
 ([] :: [ConceptChunk]) ([] :: [UnitDefn])

gbRefDB :: ReferenceDB
gbRefDB = rdb assumptions gbCitations $ funcReqs ++ likelyChgs ++
  unlikelyChgs

printSetting :: PrintingInformation
printSetting = PI gbSymbMap defaultConfiguration

this_si :: [UnitDefn]
this_si = map unitWrapper [metre, second, kilogram] ++ map unitWrapper [pascal, newton]

check_si :: [UnitDefn]
check_si = collectUnits gbSymbMap this_symbols 

resourcePath :: String
resourcePath = "../../../datafiles/GlassBR/"

glassBR_srs :: Document
glassBR_srs = mkDoc mkSRS (for'' titleize phrase) glassSystInfo

mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA]) :
  IntroSec (
    IntroProg (startIntro software blstRskInvWGlassSlab gLassBR)
      (short gLassBR)
    [IPurpose (purpOfDocIntro document gLassBR glaSlab),
     IScope incScoR endScoR,
     IChar (rdrKnldgbleIn glBreakage blastRisk) undIR appStanddIR,
     IOrgSec orgOfDocIntro dataDefn SRS.dataDefnLabel orgOfDocIntroEnd]) :
  StkhldrSec
    (StkhldrProg2
      [Client gLassBR (S "a" +:+ phrase company
        +:+ S "named Entuitive. It is developed by Dr." +:+ (S $ name mCampidelli)),
      Cstmr gLassBR]) :
  GSDSec (GSDProg2 [SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList], 
    UsrChars [user_characteristics_intro], SystCons [] [] ]) :
  ScpOfProjSec (ScpOfProjProg (short gLassBR) prodUseCaseTable (indivProdUseCase glaSlab
    capacity demandq probability)) :
  SSDSec 
    (SSDProg
      [SSDProblem  (PDProg probStart gLassBR probEnding [termsAndDesc, physSystDescription, goalStmts])
      , SSDSolChSpec 
        (SCSProg
          [ Assumptions
          , TMs ([Label] ++ stdFields) gbrTMods
          , GDs [] [] HideDerivation -- No Gen Defs for GlassBR
          , DDs ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
          , IMs ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) [calofCapacity, calofDemand] HideDerivation
          , Constraints EmptyS dataConstraintUncertainty
                        (foldlSent [makeRefS SRS.valsOfAuxConsLabel, S "gives", (plural value `ofThe` S "specification"), 
                        plural parameter, S "used in", (makeRefS inputDataConstraints)])
                        [inputDataConstraints, outputDataConstraints]
          ]
        )
      ]
    ) :
  ReqrmntSec (ReqsProg [
    FReqsSub funcReqsList,
    NonFReqsSub [performance] (gBRpriorityNFReqs)
    (S "This problem is small in size and relatively simple")
    (S "Any reasonable" +:+ phrase implementation +:+.
    (S "will be very quick" `sAnd` S "use minimal storage"))]) :
  LCsSec (LCsProg likelyChgsList) :
  UCsSec (UCsProg unlikelyChgsList) :
  TraceabilitySec
    (TraceabilityProg traceyMatrices [traceMatsAndGraphsTable1Desc, traceMatsAndGraphsTable2Desc, traceMatsAndGraphsTable3Desc]
    ((map LlC traceyMatrices) ++ traceMatsAndGraphsIntro2 ++ (map LlC traceyGraphs)) []) :
  AuxConstntSec (AuxConsProg gLassBR auxiliaryConstants) :
  Bibliography :
  AppndxSec (AppndxProg [appdxIntro, LlC fig_5, LlC fig_6]) : []
 
stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

glassSystInfo :: SystemInformation
glassSystInfo = SI {
  _sys         = gLassBR,
  _kind        = srs,
  _authors     = [nikitha, spencerSmith],
  _units       = check_si,
  _quants      = symbolsForTable,
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = (map (relToQD gbSymbMap) gbrIMods) ++ 
                 (concatMap (^. defined_quant) gbrTMods) ++
                 (concatMap (^. defined_fun) gbrTMods) ++
                  [wtntWithEqn, sdWithEqn],  -- wtntWithEqn is defined in Unitals but only appears
                                             -- in the description of the Calculation of Demand instance model;
                                             -- should this be included as a Data Definition?
                                             -- (same for sdWithEqn)
  _datadefs    = dataDefns,
  _inputs      = map qw gbInputs,
  _outputs     = map qw gbOutputs,
  _defSequence = gbQDefns,
  _constraints = gbConstrained,
  _constants   = gbConstants,
  _sysinfodb   = gbSymbMap,
  _usedinfodb = usedDB,
  _refdb       = gbRefDB
}
  --FIXME: All named ideas, not just acronyms.

glassBR_code :: CodeSpec
glassBR_code = codeSpec glassSystInfo allMods

termsAndDesc, physSystDescription, goalStmts :: Section

prodUseCaseTable, physSystDescriptionList, appdxIntro :: Contents

inputDataConstraints, outputDataConstraints, traceMatsAndGraphsTable1, traceMatsAndGraphsTable2, 
  traceMatsAndGraphsTable3, fig_glassbr, fig_2, fig_3, fig_4, fig_5, fig_6 :: LabelledContent

--------------------------------------------------------------------------------
termsAndDescBullets :: Contents
termsAndDescBullets = UlC $ ulcc $ Enumeration $ 
  Numeric $
  noRefs $ map tAndDOnly termsWithDefsOnly
  ++
  termsAndDescBulletsGlTySubSec
  ++
  termsAndDescBulletsLoadSubSec
  ++
  map tAndDWAcc termsWithAccDefn
  ++
  [tAndDWSym probBreak prob_br]
   --FIXME: merge? Needs 2 arguments because there is no instance for (SymbolForm ConceptChunk)...

termsAndDescBulletsGlTySubSec, termsAndDescBulletsLoadSubSec :: [ItemType]

termsAndDescBulletsGlTySubSec = [Nested (titleize glassTy :+: S ":") $
  Bullet $ noRefs $ map tAndDWAcc glassTypes]

termsAndDescBulletsLoadSubSec = [Nested (at_start load :+: S "-" +:+ (load ^.defn)) $
  Bullet $ noRefs $ (map tAndDWAcc $ take 2 loadTypes)
  ++
  (map tAndDOnly $ drop 2 loadTypes)]

--Used in "Goal Statements" Section--
goalStmtsList :: Contents
goalStmtsList = enumSimple 1 (short goalStmt) goalStmtsListGS1

--Used in "Traceability Matrices and Graphs" Section--

traceyMatrices :: [LabelledContent]
traceyMatrices = [traceMatsAndGraphsTable1, traceMatsAndGraphsTable2, traceMatsAndGraphsTable3]

traceyGraphs :: [LabelledContent]
traceyGraphs = [fig_2, fig_3, fig_4]

solChSpecSubsections :: [CI]
solChSpecSubsections = [thModel, inModel, dataDefn, dataConst]

--Used in "Values of Auxiliary Constants" Section--
auxiliaryConstants :: [QDefinition]
auxiliaryConstants = assumptionConstants ++ gBRSpecParamVals

--Used in "Non-Functional Requirements" Section--
gBRpriorityNFReqs :: [ConceptChunk]
gBRpriorityNFReqs = [correctness, verifiability, understandability,
  reusability, maintainability, portability]

--------------------------------------------------------------------------------

{--INTRODUCTION--}

startIntro :: NamedChunk -> Sentence -> CI -> Sentence
startIntro prgm sfwrPredicts progName = foldlSent [
  at_start prgm, S "is helpful to efficiently" `sAnd` S "correctly predict the"
  +:+. sfwrPredicts, underConsidertn blast,
  S "The", phrase prgm `sC` S "herein called", short progName `sC`
  S "aims to predict the", sfwrPredicts, S "using an intuitive",
  phrase interface]

rdrKnldgbleIn :: (NamedIdea n, NamedIdea n1) => n1 -> n -> Sentence
rdrKnldgbleIn undrstd1 undrstd2 = (phrase theory +:+ S "behind" +:+
  phrase undrstd1 `sAnd` phrase undrstd2)

undIR, appStanddIR, incScoR, endScoR :: Sentence
undIR = foldlList Comma List [phrase scndYrCalculus, phrase structuralMechanics,
  plural computerApp `sIn` phrase Edu.civilEng]
appStanddIR = foldlSent [S " In addition" `sC` plural reviewer, -- FIXME: space before "In" is a hack to get proper spacing
  S "should be familiar with the applicable", plural standard,
  S "for constructions using glass from", (foldlList Comma List
  $ map makeRefS [astm2009, astm2012, astm2016]) `sIn`
  (makeRefS SRS.referenceLabel)]
incScoR = foldl (+:+) EmptyS [S "getting all", plural inParam,
  S "related to the", phrase glaSlab `sAnd` S "also the", plural parameter,
  S "related to", phrase blastTy]
endScoR = foldl (+:+) EmptyS [S "predicts whether a", phrase glaSlab, 
  S "is safe" `sOr` S "not"]

{--Purpose of Document--}

purpOfDocIntro :: NamedChunk -> CI -> NamedChunk -> Sentence
purpOfDocIntro typeOf progName gvnVar = foldlSent [S "The main", phrase purpose,
  S "of this", phrase typeOf, S "is to predict whether a given", phrase gvnVar,
  S "is likely to resist a specified" +:+. phrase blast, S "The", plural Doc.goal
  `sAnd` plural thModel, S "used in the", short progName, phrase code,
  S "are provided" `sC` S "with an", phrase emphasis,
  S "on explicitly identifying", (plural assumption) `sAnd` S "unambiguous" +:+.
  plural definition, S "This", phrase typeOf, S "is intended to be used as a",
  phrase reference, S "to provide all", phrase information,
  S "necessary to understand" `sAnd` S "verify the" +:+. phrase analysis,
  S "The", short srs, S "is abstract because the", plural content, S "say what",
  phrase problem, S "is being solved" `sC` S "but not how to solve it"]
  --FIXME: Last sentence is also present in SWHS and NoPCM... pull out?

{--Scope of Requirements--}

{--Organization of Document--}

orgOfDocIntro, orgOfDocIntroEnd :: Sentence
orgOfDocIntro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the", phrase template, S "for an", short srs,
  S "for", phrase sciCompS, S "proposed by" +:+ makeRefS koothoor2013
  `sAnd` makeRefS smithLai2005 `sC` S "with some", 
  plural aspect, S "taken from Volere", phrase template,
  S "16", makeRefS rbrtsn2012]

orgOfDocIntroEnd = foldl (+:+) EmptyS [(at_startNP' $ the dataDefn),
  S "are used to support", (plural definition `ofThe` S "different"),
  plural model]

{--STAKEHOLDERS--}

{--The Client--}
{--The Customer--}

{--GENERAL SYSTEM DESCRIPTION--}

{--System Context--}
  
sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [makeRefS sysCtxFig1 +:+ S "shows the" +:+. phrase sysCont,
   S "A circle represents an external entity outside the" +:+ phrase software
   `sC` S "the", phrase user, S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself", (sParen $ short gLassBR) +:+. EmptyS,
   S "Arrows are used to show the data flow between the" +:+ phrase system,
   S "and its" +:+ phrase environment]
   
sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (mkLabelRAFig "sysCtxDiag") $ 
  fig (titleize sysCont) (resourcePath ++ "SystemContextFigure.png") 

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol
  [S "The interaction between the", phrase product_, S "and the", phrase user,
   S "is through a user" +:+. phrase interface,
   S "The responsibilities of the", phrase user, S "and the", phrase system,
   S "are as follows"]
   
sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide the input data related to the glass slab and blast",
    S "type ensuring no errors in the data entry",
  S "Ensure that consistent units are used for input variables",
  S "Ensure required" +:+ phrase software +:+ plural assumption +:+
    (sParen $ makeRefS SRS.assumptLabel) +:+ S "are appropriate for any particular" +:+
    phrase problem +:+ S "input to the" +:+ phrase software]

sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Detect data type mismatch, such as a string of characters",
    S "input instead of a floating point number",
  S "Determine if the inputs satisfy the required physical and software constraints",
  S "Predict whether the glass slab is safe or not."]
  
sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short gLassBR +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]
   
{--User Characteristics--}

user_characteristics_intro :: Contents
user_characteristics_intro = enumBullet $ map foldlSent
  [[S "The end user of GlassBR is expected to have completed at least the",
    S "equivalent of the second year of an undergraduate degree in civil engineering or structural engineering"],
  [S "The end user is expected to have an understanding of theory behind glass",
    S "breakage and blast risk"],
  [S "The end user is expected to have basic computer literacy to handle the software"]]

{--System Constraints--}

{--SCOPE OF THE PROJECT-}

{--Product Use Case Table--}

prodUseCaseTable = LlC $ prodUCTbl [prodUseCaseTableUC1, prodUseCaseTableUC2]

prodUseCaseTableUC1, prodUseCaseTableUC2 :: [Sentence]

prodUseCaseTableUC1 = [titleize user, titleize' characteristic +:+ S "of the"
  +:+ phrase glaSlab `sAnd` S "of the" +:+. phrase blast +:+ S "Details in"
  +:+ makeRefS SRS.indPRCaseLabel]

prodUseCaseTableUC2 = [short gLassBR, S "Whether" `sOr` S "not the" +:+
  phrase glaSlab +:+ S "is safe for the" +:+ S "calculated" +:+ phrase load
  `sAnd` S "supporting calculated" +:+ plural value]

{--Individual Product Use Case--}

indivProdUseCase :: NamedChunk -> ConceptChunk -> ConceptChunk -> ConceptChunk ->
  Contents
indivProdUseCase mainObj compare1 compare2 factorOfComparison =
  foldlSP [S "The", phrase user, S "provides the", plural input_, S "to",
  short gLassBR, S "for use within the" +:+. phrase analysis,
  S "There are two main", plural class_, S "of" +: plural input_ +:+.
  (phrase glassGeo `sAnd` phrase blastTy), S "The", phrase glassGeo, S "based",
  plural input_, S "include" +:+. (phrase glassTy `sAnd` plural dimension `ofThe`
  phrase glaPlane), blastTy ^. defn, S "These", plural parameter, S "describe"
  +:+. (phrase char_weight `sAnd` S "stand off blast"), S "Another",
  phrase input_, S "the", phrase user, S "gives is the tolerable" +:+.
  (phrase value `sOf` phrase prob_br)
  +:+
  short gLassBR, plural output_, S "if the", phrase mainObj,
  S "will be safe by comparing whether", phrase compare1, S "is greater than"
  +:+. phrase compare2, (at_start compare1 `isThe` (compare1 ^. defn))
  `sAnd` (phrase compare2 `isThe` phrase requirement) +:+.
  (S "which" `isThe` (compare2 ^. defn)), S "The second", phrase condition,
  S "is to check whether the calculated", phrase factorOfComparison,
  sParen (ch prob_br), S "is less than the tolerable",
  phrase factorOfComparison, sParen (ch pb_tol),
  S "which is obtained from the", phrase user, S "as an" +:+. phrase input_,
  S "If both", plural condition, S "return true, then it's shown that the",
  phrase mainObj, S "is safe to use" `sC`
  S "else if both return false, then the", phrase mainObj +:+.
  S "is considered unsafe", S "All the supporting calculated", plural value,
  S "are also displayed as", phrase output_]

{--SPECIFIC SYSTEM DESCRIPTION--}

--Automatically generated

{--PROBLEM DESCRIPTION--}

probStart, probEnding :: Sentence
probStart = foldlSent [S "A", phrase system,
  S "is needed to efficiently" `sAnd` S "correctly predict the",
  phrase blastRisk +:+ S "involved with the glass"]
probEnding = foldl (+:+) EmptyS [S "interpret the", plural input_,
  S "to give out the", plural output_,
  S "which predict whether the", phrase glaSlab,
  S "can withstand the", phrase blast, S "under the",
  plural condition]

{--Terminology and Definitions--}

termsAndDesc = termDefnF (Just (S "All" `sOf` S "the" +:+ plural term_ +:+
  S "are extracted from" +:+ makeRefS astm2009 `sIn`
  (makeRefS SRS.referenceLabel))) [termsAndDescBullets]

{--Physical System Description--}

physSystDescription = physSystDesc (short gLassBR) fig_glassbr 
  [physSystDescriptionList, LlC fig_glassbr]

fig_glassbr = llcc (mkLabelRAFig "physSystImage") $ figWithWidth 
  (at_startNP $ the physicalSystem) (resourcePath ++ "physicalsystimage.png") 30

physSystDescriptionList = enumSimple 1 (short physSyst) physSystDescriptionListPhysys

--"Dead" knowledge?
physSystDescriptionListPhysys :: [Sentence]
physSystDescriptionListPhysys1 :: Sentence
physSystDescriptionListPhysys2 :: NamedIdea n => n -> Sentence

physSystDescriptionListPhysys = [physSystDescriptionListPhysys1, physSystDescriptionListPhysys2 (ptOfExplsn)]

physSystDescriptionListPhysys1 = S "The" +:+. phrase glaSlab

physSystDescriptionListPhysys2 imprtntElem = foldlSent [S "The"
  +:+. phrase imprtntElem, S "Where the", phrase bomb `sC`
  S "or", (blast ^. defn) `sC` S "is located. The", phrase sD
  `isThe` phrase distance, S "between the", phrase imprtntElem `sAnd`
  S "the glass"]

{--Goal Statements--}

goalStmts = goalStmtF [foldlList Comma List [plural dimension `ofThe` phrase glaPlane,
  phrase glassTy, plural characteristic `ofThe` phrase explosion,
  S "the" +:+ phrase pb_tol]] [goalStmtsList]

goalStmtsListGS1 :: [Sentence]
goalStmtsListGS1 = [foldlSent [S "Analyze" `sAnd` S "predict whether",
  S "the", phrase glaSlab, S "under consideration will be able to withstand",
  S "the", phrase explosion `sOf` S "a certain", phrase degree_',
  S "which is calculated based on", phrase userInput]]

{--SOLUTION CHARACTERISTICS SPECIFICATION--}

--Automatically generated

{--Assumptions--}

{--Theoretical Models--}

{--Data Definitions--}

{--Data Constraints--}

{-input and output tables-}

inputDataConstraints = inDataConstTbl gbInputDataConstraints
outputDataConstraints = outDataConstTbl [prob_br]

{--REQUIREMENTS--}

{--Functional Requirements--}

{--Nonfunctional Requirements--}

{--LIKELY CHANGES--}

{--UNLIKELY CHANGES--}

{--TRACEABLITY MATRICES AND GRAPHS--}

traceMatsAndGraphsTable1Desc :: Sentence
traceMatsAndGraphsTable1Desc = foldlList Comma List (map plural (take 3 solChSpecSubsections)) +:+.
  S "with each other"

traceMatsAndGraphsTable2Desc :: Sentence
traceMatsAndGraphsTable2Desc = plural requirement +:+ S "on" +:+. foldlList Comma List
  (map plural solChSpecSubsections)

traceMatsAndGraphsTable3Desc :: Sentence
traceMatsAndGraphsTable3Desc = foldlsC (map plural (take 3 solChSpecSubsections)) `sC`
  plural likelyChg `sAnd` plural requirement +:+ S "on the" +:+
  plural assumption

traceMatsAndGraphsT, traceMatsAndGraphsIM, traceMatsAndGraphsDD, traceMatsAndGraphsDataCons, traceMatsAndGraphsFuncReq, traceMatsAndGraphsA,
  traceMatsAndGraphsLC :: [String]

traceMatsAndGraphsTRef, traceMatsAndGraphsIMRef, traceMatsAndGraphsDDRef, traceMatsAndGraphsDataConsRef, traceMatsAndGraphsFuncReqRef,
  traceMatsAndGraphsARef, traceMatsAndGraphsLCRef :: [Sentence]

traceMatsAndGraphsT = ["T1", "T2"]
traceMatsAndGraphsTRef = map makeRefS gbrTMods

traceMatsAndGraphsIM = ["IM1", "IM2", "IM3"]
traceMatsAndGraphsIMRef = map makeRefS gbrIMods

traceMatsAndGraphsDD =  ["DD1", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8"]
traceMatsAndGraphsDDRef = map makeRefS dataDefns

traceMatsAndGraphsDataCons  = ["Data Constraints"]
traceMatsAndGraphsDataConsRef = [makeRefS SRS.datConLabel]

traceMatsAndGraphsFuncReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
traceMatsAndGraphsFuncReqRef = map makeRef2S funcReqs

traceMatsAndGraphsA = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"]
traceMatsAndGraphsARef = map makeRefS assumptions

traceMatsAndGraphsLC = ["LC1", "LC2", "LC3", "LC4", "LC5"]
traceMatsAndGraphsLCRef = map makeRef2S likelyChgs

traceMatsAndGraphsRowT1 :: [String]
traceMatsAndGraphsRowT1 = traceMatsAndGraphsT ++ traceMatsAndGraphsIM ++ traceMatsAndGraphsDD

-- The headers for the first row, and column
traceMatsAndGraphsRowHdrT1 :: [Sentence]
traceMatsAndGraphsRowHdrT1 = zipWith itemRefToSent traceMatsAndGraphsRowT1 (traceMatsAndGraphsTRef ++
  traceMatsAndGraphsIMRef ++ traceMatsAndGraphsDDRef)

-- list of columns and their rows for traceability matrix
traceMatsAndGraphsColsT1 :: [[String]]
traceMatsAndGraphsColsT1 = [traceMatsAndGraphsColsT1_T1, traceMatsAndGraphsColsT1_T2, traceMatsAndGraphsColsT1_IM1, traceMatsAndGraphsColsT1_IM2, traceMatsAndGraphsColsT1_IM3,
  traceMatsAndGraphsColsT1_DD1, traceMatsAndGraphsColsT1_DD2, traceMatsAndGraphsColsT1_DD3, traceMatsAndGraphsColsT1_DD4, traceMatsAndGraphsColsT1_DD5, traceMatsAndGraphsColsT1_DD6, traceMatsAndGraphsColsT1_DD7,
  traceMatsAndGraphsColsT1_DD8]

traceMatsAndGraphsColsT1_T1, traceMatsAndGraphsColsT1_T2, traceMatsAndGraphsColsT1_IM1, traceMatsAndGraphsColsT1_IM2, traceMatsAndGraphsColsT1_IM3, traceMatsAndGraphsColsT1_DD1, traceMatsAndGraphsColsT1_DD2,
  traceMatsAndGraphsColsT1_DD3, traceMatsAndGraphsColsT1_DD4, traceMatsAndGraphsColsT1_DD5, traceMatsAndGraphsColsT1_DD6, traceMatsAndGraphsColsT1_DD7, traceMatsAndGraphsColsT1_DD8 :: [String]

-- list of each item that "this" item requires for traceability matrix
traceMatsAndGraphsColsT1_T1  = ["T2", "IM1"]
traceMatsAndGraphsColsT1_T2  = ["T1", "IM2", "IM3"]
traceMatsAndGraphsColsT1_IM1 = ["DD1", "DD2", "DD3"]
traceMatsAndGraphsColsT1_IM2 = ["DD4", "DD5"]
traceMatsAndGraphsColsT1_IM3 = []
traceMatsAndGraphsColsT1_DD1 = []
traceMatsAndGraphsColsT1_DD2 = []
traceMatsAndGraphsColsT1_DD3 = ["DD6"]
traceMatsAndGraphsColsT1_DD4 = ["DD2", "DD6"]
traceMatsAndGraphsColsT1_DD5 = []
traceMatsAndGraphsColsT1_DD6 = ["IM3", "DD2", "DD5"]
traceMatsAndGraphsColsT1_DD7 = ["DD8"]
traceMatsAndGraphsColsT1_DD8 = ["DD2"]

traceMatsAndGraphsTable1 = llcc (mkLabelSame "TraceyItemSecs" Tab) $ Table
  (EmptyS:traceMatsAndGraphsRowHdrT1)
  (makeTMatrix traceMatsAndGraphsRowHdrT1 traceMatsAndGraphsColsT1 traceMatsAndGraphsRowT1)
  (showingCxnBw traceyMatrix
  (titleize' item +:+ S "of Different" +:+ titleize' section_)) True

--

traceMatsAndGraphsRowT2 :: [String]
traceMatsAndGraphsRowT2 = traceMatsAndGraphsRowT1 ++ traceMatsAndGraphsDataCons ++ traceMatsAndGraphsFuncReq

traceMatsAndGraphsRowHdrT2, traceMatsAndGraphsColHdrT2 :: [Sentence]
traceMatsAndGraphsRowHdrT2 = traceMatsAndGraphsRowHdrT1 ++
  (zipWith itemRefToSent (traceMatsAndGraphsDataCons ++ traceMatsAndGraphsFuncReq)
   (traceMatsAndGraphsDataConsRef ++ traceMatsAndGraphsFuncReqRef))

traceMatsAndGraphsColHdrT2 = zipWith (\x y -> (S x) +:+ (sParen (S "in" +:+ y)))
  traceMatsAndGraphsFuncReq traceMatsAndGraphsFuncReqRef

traceMatsAndGraphsColsT2_R1, traceMatsAndGraphsColsT2_R2, traceMatsAndGraphsColsT2_R3,
  traceMatsAndGraphsColsT2_R4, traceMatsAndGraphsColsT2_R5, traceMatsAndGraphsColsT2_R6 :: [String]

traceMatsAndGraphsColsT2 :: [[String]]
traceMatsAndGraphsColsT2 = [traceMatsAndGraphsColsT2_R1, traceMatsAndGraphsColsT2_R2, 
  traceMatsAndGraphsColsT2_R3, traceMatsAndGraphsColsT2_R4, traceMatsAndGraphsColsT2_R5,
  traceMatsAndGraphsColsT2_R6]
traceMatsAndGraphsColsT2_R1 = []
traceMatsAndGraphsColsT2_R2 = []
traceMatsAndGraphsColsT2_R3 = ["Data Constraints"]
traceMatsAndGraphsColsT2_R4 = ["R1", "R2"]
traceMatsAndGraphsColsT2_R5 = ["T1", "T2"]
traceMatsAndGraphsColsT2_R6 = ["IM1", "IM2", "IM3", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8"]

traceMatsAndGraphsTable2 = llcc (mkLabelSame "TraceyReqsItems" Tab) $ Table
  (EmptyS:traceMatsAndGraphsRowHdrT2)
  (makeTMatrix traceMatsAndGraphsColHdrT2 traceMatsAndGraphsColsT2 traceMatsAndGraphsRowT2)
  (showingCxnBw traceyMatrix (titleize' requirement `sAnd` S "Other" +:+
  titleize' item)) True

--

traceMatsAndGraphsRowT3 :: [String]
traceMatsAndGraphsRowT3 = traceMatsAndGraphsA

traceMatsAndGraphsRowHdr3, traceMatsAndGraphsColHdr3 :: [Sentence]
traceMatsAndGraphsRowHdr3 = zipWith itemRefToSent traceMatsAndGraphsA traceMatsAndGraphsARef

traceMatsAndGraphsColHdr3 = traceMatsAndGraphsRowHdrT1 ++ (zipWith itemRefToSent
  (traceMatsAndGraphsLC ++ traceMatsAndGraphsFuncReq) (traceMatsAndGraphsLCRef ++ traceMatsAndGraphsFuncReqRef))

traceMatsAndGraphsColsT3 :: [[String]]
traceMatsAndGraphsColsT3 = [traceMatsAndGraphsColsT3_T1, traceMatsAndGraphsColsT3_T2, traceMatsAndGraphsColsT3_IM1, traceMatsAndGraphsColsT3_IM2, traceMatsAndGraphsColsT3_IM3, traceMatsAndGraphsColsT3_DD1,
  traceMatsAndGraphsColsT3_DD2, traceMatsAndGraphsColsT3_DD3, traceMatsAndGraphsColsT3_DD4, traceMatsAndGraphsColsT3_DD5, traceMatsAndGraphsColsT3_DD6, traceMatsAndGraphsColsT3_DD7, traceMatsAndGraphsColsT3_DD8,
  traceMatsAndGraphsColsT3_LC1, traceMatsAndGraphsColsT3_LC2, traceMatsAndGraphsColsT3_LC3, traceMatsAndGraphsColsT3_LC4, traceMatsAndGraphsColsT3_LC5, traceMatsAndGraphsColsT3_R1, traceMatsAndGraphsColsT3_R2,
  traceMatsAndGraphsColsT3_R3, traceMatsAndGraphsColsT3_R4, traceMatsAndGraphsColsT3_R5, traceMatsAndGraphsColsT3_R6]

traceMatsAndGraphsColsT3_T1, traceMatsAndGraphsColsT3_T2, traceMatsAndGraphsColsT3_IM1, traceMatsAndGraphsColsT3_IM2, traceMatsAndGraphsColsT3_IM3, traceMatsAndGraphsColsT3_DD1, traceMatsAndGraphsColsT3_DD2,
  traceMatsAndGraphsColsT3_DD3, traceMatsAndGraphsColsT3_DD4, traceMatsAndGraphsColsT3_DD5, traceMatsAndGraphsColsT3_DD6, traceMatsAndGraphsColsT3_DD7, traceMatsAndGraphsColsT3_DD8,
  traceMatsAndGraphsColsT3_LC1, traceMatsAndGraphsColsT3_LC2, traceMatsAndGraphsColsT3_LC3, traceMatsAndGraphsColsT3_LC4, traceMatsAndGraphsColsT3_LC5, traceMatsAndGraphsColsT3_R1,
  traceMatsAndGraphsColsT3_R2, traceMatsAndGraphsColsT3_R3, traceMatsAndGraphsColsT3_R4, traceMatsAndGraphsColsT3_R5, traceMatsAndGraphsColsT3_R6 :: [String]

-- list of each item that "this" item requires for traceability matrix
traceMatsAndGraphsColsT3_T1  = []
traceMatsAndGraphsColsT3_T2  = []
traceMatsAndGraphsColsT3_IM1 = ["A4", "A6", "A7"]
traceMatsAndGraphsColsT3_IM2 = ["A1", "A2", "A5"]
traceMatsAndGraphsColsT3_IM3 = []
traceMatsAndGraphsColsT3_DD1 = []
traceMatsAndGraphsColsT3_DD2 = []
traceMatsAndGraphsColsT3_DD3 = []
traceMatsAndGraphsColsT3_DD4 = ["A4"]
traceMatsAndGraphsColsT3_DD5 = []
traceMatsAndGraphsColsT3_DD6 = ["A5"]
traceMatsAndGraphsColsT3_DD7 = []
traceMatsAndGraphsColsT3_DD8 = ["A4"]
traceMatsAndGraphsColsT3_LC1 = ["A3"]
traceMatsAndGraphsColsT3_LC2 = ["A4", "A8"]
traceMatsAndGraphsColsT3_LC3 = ["A5"]
traceMatsAndGraphsColsT3_LC4 = ["A6"]
traceMatsAndGraphsColsT3_LC5 = ["A7"]
traceMatsAndGraphsColsT3_R1  = []
traceMatsAndGraphsColsT3_R2  = ["A4", "A5", "A8"]
traceMatsAndGraphsColsT3_R3  = []
traceMatsAndGraphsColsT3_R4  = []
traceMatsAndGraphsColsT3_R5  = []
traceMatsAndGraphsColsT3_R6  = []

traceMatsAndGraphsTable3 = llcc (mkLabelSame "TraceyAssumpsOthers" Tab) $ Table
  (EmptyS:traceMatsAndGraphsRowHdr3)
  (makeTMatrix traceMatsAndGraphsColHdr3 traceMatsAndGraphsColsT3 traceMatsAndGraphsRowT3)
  (showingCxnBw traceyMatrix (titleize' assumption `sAnd` S "Other"
  +:+ titleize' item)) True

--
traceMatsAndGraphsIntro2 :: [Contents]
traceMatsAndGraphsIntro2 = map UlC $ traceGIntro traceyGraphs
  [(foldlList Comma List (map plural (take 3 solChSpecSubsections)) +:+.
  S "on each other"), (plural requirement +:+ S "on" +:+. foldlList Comma List
  (map plural solChSpecSubsections)),
  (foldlList Comma List ((map plural (take 3 solChSpecSubsections))++
  [plural requirement, plural likelyChg +:+ S "on" +:+ plural assumption]))]

fig_2 = figureLabel 2 traceyMatrix
  (titleize' item +:+ S "of Different" +:+ titleize' section_)
  (resourcePath ++ "Trace.png") "TraceyItemSecs"

fig_3 = figureLabel 3 traceyMatrix
  (titleize' requirement `sAnd` S "Other" +:+ titleize' item)
  (resourcePath ++ "RTrace.png") "TraceyReqsItems"

fig_4 = figureLabel 4 traceyMatrix
  (titleize' assumption `sAnd` S "Other" +:+ titleize' item)
  (resourcePath ++ "ATrace.png") "TraceyAssumpsOthers"

{--VALUES OF AUXILIARY CONSTANTS--}

{--REFERENCES--}

{--APPENDIX--}

appdxIntro = foldlSP [
  S "This", phrase appendix, S "holds the", plural graph,
  sParen ((makeRefS fig_5) `sAnd` (makeRefS fig_6)),
  S "used for interpolating", plural value, S "needed in the", plural model]

fig_5 = llcc (mkLabelRAFig "demandVSsod") $ fig (titleize figure +: S "5" +:+ (demandq ^. defn) +:+
  sParen (ch demand) `sVersus` at_start sD +:+ sParen (getAcc stdOffDist)
  `sVersus` at_start char_weight +:+ sParen (ch char_weight))
  (resourcePath ++ "ASTM_F2248-09.png")

fig_6 = llcc (mkLabelRAFig "dimlessloadVSaspect") $ fig (titleize figure +: S "6" +:+ S "Non dimensional" +:+
  phrase lateralLoad +:+ sParen (ch dimlessLoad)
  `sVersus` titleize aspect_ratio +:+ sParen (getAcc aR)
  `sVersus` at_start stressDistFac +:+ sParen (ch stressDistFac))
  (resourcePath ++ "ASTM_F2248-09_BeasonEtAl.png")

blstRskInvWGlassSlab :: Sentence
blstRskInvWGlassSlab = phrase blastRisk +:+ S "involved with the" +:+
  phrase glaSlab
