module Drasil.GlassBR.Body where

import Control.Lens ((^.))
import Data.List (nub)

import Language.Drasil hiding (organization)
import Language.Drasil.Code (CodeSpec, codeSpec, relToQD)
import qualified Drasil.DocLang.SRS as SRS (dataDefnLabel, 
  valsOfAuxConsLabel, referenceLabel, indPRCaseLabel,
  datConLabel, funcReqLabel, assumptLabel, likeChgLabel, 
  funcReq, assumpt, likeChg)

import Drasil.DocLang (AppndxSec(..), AuxConstntSec(..), DerivationDisplay(..), 
  DocDesc, DocSection(..), Field(..), Fields, GSDSec(GSDProg2), 
  GSDSub(UsrChars, SystCons), InclUnits(IncludeUnits), IntroSec(IntroProg),
  IntroSub(IChar, IOrgSec, IPurpose, IScope), LCsSec(..), ProblemDescription(..), 
  RefSec(RefProg), RefTab(TAandA, TUnits), ReqrmntSec(..), 
  ReqsSub(FReqsSub, NonFReqsSub), ScpOfProjSec(ScpOfProjProg), SCSSub(..), 
  SSDSec(..), SSDSub(..), SolChSpec(..), StkhldrSec(StkhldrProg2), 
  StkhldrSub(Client, Cstmr), TraceabilitySec(TraceabilityProg), 
  TSIntro(SymbOrder, TSPurpose), UCsSec(..), Verbosity(Verbose), 
  cite, dataConstraintUncertainty, goalStmtF, inDataConstTbl, intro, mkDoc, 
  mkRequirement, outDataConstTbl, physSystDesc, probDescF, termDefnF, 
  traceGIntro, tsymb)

import Data.Drasil.Concepts.Computation (computerApp, inParam,
  computerLiteracy, inValue, inQty)
import Data.Drasil.Concepts.Documentation as Doc (analysis, appendix, aspect,
  characteristic, class_, code, condition, content,
  datum, definition, description, document, emphasis, endUser, failure,
  figure, goal, implementation, information, interface, input_, item,
  message, model, organization, output_, problem, purpose,
  quantity, reference, reviewer, section_, software, standard,
  symbol_, system, template, term_, theory, traceyMatrix, user, value,
  physicalSystem, datumConstraint, userInput, assumption, dataDefn,
  goalStmt, inModel, likelyChg, physSyst, requirement, srs, thModel,
  dataConst, company)

import Data.Drasil.Concepts.Education (secondYear, undergradDegree,
  civilEng, structuralEng, scndYrCalculus, structuralMechanics)
import Data.Drasil.Concepts.Math (graph, calculation, probability,
  parameter)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.Concepts.Physics (distance)
import Data.Drasil.Concepts.Software (correctness, verifiability,
  understandability, reusability, maintainability, portability,
  performance, errMsg)
import Data.Drasil.Concepts.Thermodynamics (degree_')
import Data.Drasil.SentenceStructures (acroR, sVersus, sAnd, foldlSP,
  foldlSent, foldlSent_, figureLabel, foldlList, SepType(Comma), FoldType(List),
  showingCxnBw, foldlsC, sOf, followA, ofThe, sIn, isThe, isExpctdToHv, sOr, 
  underConsidertn, tAndDWAcc, tAndDOnly, tAndDWSym, andThe)
import Data.Drasil.Software.Products (sciCompS)
import Data.Drasil.Utils (makeTMatrix, makeListRef, itemRefToSent, noRefs,
  refFromType, enumSimple, enumBullet, prodUCTbl)

import Drasil.GlassBR.Assumptions (assumptionConstants, assumptionDescs,
  gbRefDB, newAssumptions)
import Drasil.GlassBR.Changes (likelyChanges_SRS, unlikelyChanges_SRS)
import Drasil.GlassBR.Concepts (acronyms, aR, blastRisk, glaPlane, glaSlab, 
  glass, gLassBR, lShareFac, ptOfExplsn, stdOffDist)
import Drasil.GlassBR.DataDefs (aspRat, dataDefns, gbQDefns, hFromt, strDisFac, nonFL, 
  dimLL, glaTyFac, tolStrDisFac, tolPre, risk, standOffDis)
import Drasil.GlassBR.ModuleDefs (allMods)
import Drasil.GlassBR.References (rbrtsn2012)
import Drasil.GlassBR.Symbols (this_symbols)
import Drasil.GlassBR.TMods (tModels, pbSafetyReq, lrSafetyReq, pbIsSafe, lrIsSafe)
import Drasil.GlassBR.IMods (iModels, calOfCap, calOfDe, probOfBr, probOfBreak, 
  calofCapacity, calofDemand)

import Drasil.GlassBR.Unitals (stressDistFac, aspect_ratio, dimlessLoad, 
  lateralLoad, char_weight, sD, demand, demandq, lRe, wtntWithEqn, 
  sdWithEqn, prob_br, notSafe, safeMessage, is_safe1, is_safe2, plate_width, 
  plate_len, blast, glassTy, gbInputDataConstraints, explosion, pb_tol, 
  blast, bomb, blastTy, glassGeo, glass_type, nom_thick, sdx, sdy, sdz, tNT, 
  gBRSpecParamVals, loadTypes, load, glassTypes, probBreak, termsWithAccDefn, 
  termsWithDefsOnly, gbConstants, gbConstrained, gbOutputs, gbInputs, glBreakage, 
  capacity, constant_LoadDF, glassBRsymb)

import Data.Drasil.Citations (koothoor2013, smithLai2005)
import Data.Drasil.People (spencerSmith, nikitha, mCampidelli)
import Data.Drasil.Phrase (for'')
import Data.Drasil.SI_Units (kilogram, metre, millimetre, newton, pascal, 
  second)

{--}

gbSymbMap :: ChunkDB
gbSymbMap =
  cdb this_symbols (map nw acronyms ++ map nw this_symbols) glassBRsymb
      (map unitWrapper [metre, second, kilogram] ++ map unitWrapper [pascal, newton])

ccss'' :: Sentence -> [DefinedQuantityDict]
ccss'' s = combine s gbSymbMap

ccss' :: Expr -> [DefinedQuantityDict]
ccss' s = combine' s gbSymbMap

ccs' :: [DefinedQuantityDict]
ccs' = nub ((concatMap ccss'' $ getDoc glassBR_srs) ++ (concatMap ccss' $ egetDoc glassBR_srs))

outputuid :: [String]
outputuid = nub ((concatMap snames $ getDoc glassBR_srs) ++ (concatMap names $ egetDoc glassBR_srs))

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
  GSDSec (GSDProg2 [UsrChars [userCharsBullets endUser gLassBR secondYear
    undergradDegree civilEng structuralEng glBreakage blastRisk],
    SystCons [] []]) :
  ScpOfProjSec (ScpOfProjProg (short gLassBR) prodUseCaseTable (indivProdUseCase glaSlab
    capacity demandq probability)) :
  SSDSec 
    (SSDProg
      [SSDProblem  (PDProg probStart gLassBR probEnding [termsAndDesc, physSystDescription, goalStmts])
      , SSDSolChSpec 
        (SCSProg
          [ Assumptions
          , TMs ([Label] ++ stdFields) [pbIsSafe, lrIsSafe]
          , GDs [] [] HideDerivation -- No Gen Defs for GlassBR
          , DDs' ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
          , IMs ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) [probOfBreak, calofCapacity, calofDemand] HideDerivation
          , Constraints EmptyS dataConstraintUncertainty
                        (foldlSent [(midRef SRS.valsOfAuxConsLabel), S "gives", (plural value `ofThe` S "specification"), 
                        plural parameter, S "used in", (makeRef inputDataConstraints)])
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
  _quants      = this_symbols,
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = (map (relToQD gbSymbMap) iModels {-[RelationConcept]-}) ++ 
                 (map (relToQD gbSymbMap) tModels {-[RelationConcept]-}) ++
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
  _refdb       = gbRefDB
}
  --FIXME: All named ideas, not just acronyms.

glassBR_code :: CodeSpec
glassBR_code = codeSpec glassSystInfo allMods

termsAndDesc, physSystDescription, goalStmts :: Section

prodUseCaseTable, physSystDescriptionList, appdxIntro :: Contents

inputDataConstraints, outputDataConstraints, traceMatsAndGraphsTable1, traceMatsAndGraphsTable2, 
  traceMatsAndGraphsTable3, fig_glassbr, fig_2, fig_3, fig_4, fig_5, fig_6 :: LabelledContent

funcReqsList, traceMatsAndGraphsIntro2 :: [Contents]

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
traceyMatrices, traceyGraphs :: [LabelledContent]
traceyMatrices = [traceMatsAndGraphsTable1, traceMatsAndGraphsTable2, traceMatsAndGraphsTable3]
traceyGraphs = [fig_2, fig_3, fig_4]

solChSpecSubsections :: [CI]
solChSpecSubsections = [thModel, inModel, dataDefn, dataConst]

--Used in "Values of Auxiliary Constants" Section--
auxiliaryConstants :: [QDefinition]
auxiliaryConstants = assumptionConstants ++ gBRSpecParamVals

--Used in "Functional Requirements" Section--
requiredInputs :: [QuantityDict]
requiredInputs = (map qw [plate_len, plate_width, char_weight])
  ++ (map qw [pb_tol, tNT]) ++ (map qw [sdx, sdy, sdz])
  ++ (map qw [glass_type, nom_thick])

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
  plural computerApp `sIn` phrase civilEng]
appStanddIR = foldlSent [S " In addition" `sC` plural reviewer, -- FIXME: space before "In" is a hack to get proper spacing
  S "should be familiar with the applicable", plural standard,
  S "for constructions using glass from",
  sSqBr (S "1-3" {-astm2009, astm2012, astm2016-}) `sIn`
  (midRef SRS.referenceLabel)]
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
  S "for", phrase sciCompS, S "proposed by" +:+ cite gbRefDB koothoor2013
  `sAnd` cite gbRefDB smithLai2005 `sC` S "with some", 
  plural aspect, S "taken from Volere", phrase template,
  S "16", cite gbRefDB rbrtsn2012]

orgOfDocIntroEnd = foldl (+:+) EmptyS [(at_start' $ the dataDefn),
  S "are used to support", (plural definition `ofThe` S "different"),
  plural model]

{--STAKEHOLDERS--}

{--The Client--}
{--The Customer--}

{--GENERAL SYSTEM DESCRIPTION--}

{--User Characteristics--}

userCharsBullets :: (NamedIdea n1, NamedIdea n, NamedIdea n2, NamedIdea n3,
  NamedIdea n4, NamedIdea n5, Idea c, NamedIdea n6) =>
  n6 -> c -> n5 -> n4 -> n3 -> n2 -> n1 -> n -> Contents
userCharsBullets intendedIndvdl progName yr degreeType prog1 prog2 undrstd1 undrstd2
  = enumBullet [foldlSent [(phrase intendedIndvdl `sOf` short progName)
  `isExpctdToHv` S "completed at least", (S "equivalent" `ofThe` (phrase yr)),
  S "of an", phrase degreeType `sIn` phrase prog1 `sOr` phrase prog2],
  (phrase intendedIndvdl `isExpctdToHv` S "an understanding of" +:+.
  rdrKnldgbleIn undrstd1 undrstd2), foldlSent [phrase intendedIndvdl
  `isExpctdToHv` S "basic", phrase computerLiteracy, S "to handle the",
  phrase software]]

{--System Constraints--}

{--SCOPE OF THE PROJECT-}

{--Product Use Case Table--}

prodUseCaseTable = LlC $ prodUCTbl [prodUseCaseTableUC1, prodUseCaseTableUC2]

prodUseCaseTableUC1, prodUseCaseTableUC2 :: [Sentence]

prodUseCaseTableUC1 = [titleize user, titleize' characteristic +:+ S "of the"
  +:+ phrase glaSlab `sAnd` S "of the" +:+. phrase blast +:+ S "Details in"
  +:+ midRef SRS.indPRCaseLabel]

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
  S "are extracted from" +:+ (sSqBrNum 1 {-astm2009-}) `sIn`
  (midRef SRS.referenceLabel))) [termsAndDescBullets]

{--Physical System Description--}

physSystDescription = physSystDesc (short gLassBR) fig_glassbr 
  [physSystDescriptionList, LlC fig_glassbr]

fig_glassbr = llcc (mkLabelRA'' "physSystImage") $ figWithWidth 
  (at_start $ the physicalSystem) (resourcePath ++ "physicalsystimage.png") 30
  "physSystImage"

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

funcReqsList = funcReqsListOfReqs ++ funcReqsR6 ++ [LlC funcReqsR1Table]

funcReqsR1, funcReqsR2, funcReqsR3, funcReqsR4, funcReqsR5 :: LabelledContent
req1Desc, req2Desc, req3Desc, req4Desc :: Sentence
req5Desc :: NamedChunk -> Sentence
funcReqsR6 :: [Contents] --FIXME: Issue #327

funcReqsListOfReqs :: [Contents]
funcReqsListOfReqs = map LlC $ [funcReqsR1, funcReqsR2, funcReqsR3, funcReqsR4, funcReqsR5]

funcReqsR1 = mkRequirement "funcReqsR1" req1Desc "Input-Glass-Props"
funcReqsR2 = mkRequirement "funcReqsR2" req2Desc "System-Set-Values-Following-Assumptions"
funcReqsR3 = mkRequirement "funcReqsR3" req3Desc "Check-Input-with-Data_Constraints"
funcReqsR4 = mkRequirement "funcReqsR4" req4Desc "Output-Values-and-Known-Quantities"
funcReqsR5 = mkRequirement "funcReqsR5" (req5Desc (output_)) "Check-Glass-Safety"

req1Desc = foldlSent [at_start input_, S "the", plural quantity, S "from",
  makeRef funcReqsR1Table `sC` S "which define the", phrase glass,
  plural dimension `sC` (glassTy ^. defn) `sC` S "tolerable",
  phrase probability `sOf` phrase failure, S "and",
  (plural characteristic `ofThe` phrase blast), S "Note:",
  ch plate_len `sAnd` ch plate_width,
  S "will be input in terms of", plural millimetre `sAnd`
  S "will be converted to the equivalent value in", plural metre]

funcReqsR1Table :: LabelledContent
funcReqsR1Table = llcc (mkLabelRA'' "R1ReqInputs") $ 
  Table
  [at_start symbol_, at_start description, S "Units"]
  (mkTable
  [ch,
   at_start, unitToSentence] requiredInputs)
  (S "Required Inputs following R1") True "R1ReqInputs"

req2Desc = foldlSent [S "The", phrase system,
  S "shall set the known", plural value +: S "as follows",
  foldlList Comma List [(foldlsC (map ch (take 4 assumptionConstants)) `followA` 4),
  ((ch constant_LoadDF) `followA` 8), (short lShareFac `followA` 5),
  (ch hFromt) +:+ sParen (S "from" +:+ (makeRef hFromt)), 
  (ch glaTyFac) +:+ sParen (S "from" +:+ (makeRef glaTyFac)),
  (ch standOffDis) +:+ sParen (S "from" +:+ (makeRef standOffDis))]]

--ItemType
{-funcReqsR2 = (Nested (S "The" +:+ phrase system +:+
   S "shall set the known" +:+ plural value +: S "as follows")
    (Bullet $ map Flat
     [foldlsC (map getS (take 4 assumptionConstants)) `followA` 4,
     (getS loadDF) `followA` 8,
     short lShareFac `followA` 5]))
-}
--FIXME:should constants, LDF, and LSF have some sort of field that holds
-- the assumption(s) that're being followed? (Issue #349)

req3Desc = foldlSent [S "The", phrase system, S "shall check the entered",
  plural inValue, S "to ensure that they do not exceed the",
  plural datumConstraint, S "mentioned in" +:+. midRef
  SRS.datConLabel, S "If any" `sOf` S "the", plural inParam,
  S "is out" `sOf` S "bounds" `sC` S "an", phrase errMsg, S "is displayed"
  `andThe` plural calculation, S "stop"]

req4Desc = foldlSent [titleize output_, S "the", plural inQty,
  S "from", acroR 1 `andThe` S "known", plural quantity,
  S "from", acroR 2]

req5Desc cmd = foldlSent_ [S "If", (ch is_safe1), S "∧", (ch is_safe2),
  sParen (S "from" +:+ (makeRef (reldefn pbSafetyReq))
  `sAnd` (makeRef (reldefn lrSafetyReq))), S "are true" `sC`
  phrase cmd, S "the", phrase message, Quote (safeMessage ^. defn),
  S "If the", phrase condition, S "is false, then", phrase cmd,
  S "the", phrase message, Quote (notSafe ^. defn)]

testing :: [QuantityDict]
testing = qw prob_br : qw lRe : qw demand : [] -- all different types!
testing1 :: [RelationConcept]
testing1 = [probOfBr, calOfCap, calOfDe]
--FIXME: rename or find better implementation?

funcReqsR6_pulledList :: [QDefinition]
funcReqsR6_pulledList = [risk, strDisFac, nonFL, glaTyFac, dimLL, 
  tolPre, tolStrDisFac, hFromt, aspRat]

funcReqsR6 = map (UlC . ulcc) [Enumeration $ Simple $ [(acroR 6, Nested (titleize output_ +:+
  S "the following" +: plural quantity)
  $ Bullet $ noRefs $
    map (\(a, d) -> Flat $ at_start a +:+ sParen (ch a) +:+
    sParen (makeRef $ reldefn d)) (zip testing testing1)
    ++
    map (\d -> Flat $ at_start d +:+ sParen (ch d) +:+
    sParen (makeRef $ datadefn d)) funcReqsR6_pulledList
    , Nothing)]]

{--Nonfunctional Requirements--}

{--LIKELY CHANGES--}

likelyChgsList :: [Contents]
likelyChgsList = likelyChanges_SRS 

{--UNLIKELY CHANGES--}
unlikelyChgsList :: [Contents]
unlikelyChgsList = unlikelyChanges_SRS

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
traceMatsAndGraphsTRef = map (refFromType Theory) tModels

traceMatsAndGraphsIM = ["IM1", "IM2", "IM3"]
traceMatsAndGraphsIMRef = map (refFromType Theory) iModels

traceMatsAndGraphsDD =  ["DD1", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8"]
traceMatsAndGraphsDDRef = map (refFromType Data') dataDefns

traceMatsAndGraphsDataCons  = ["Data Constraints"]
traceMatsAndGraphsDataConsRef = [midRef SRS.datConLabel]

traceMatsAndGraphsFuncReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
traceMatsAndGraphsFuncReqRef = makeListRef traceMatsAndGraphsFuncReq (SRS.funcReq [] []) --Fixme:replace with actual FRs!

traceMatsAndGraphsA = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"]
traceMatsAndGraphsARef = makeListRef traceMatsAndGraphsA (SRS.assumpt [] []) --Fixme:replace with actual assumptions!

traceMatsAndGraphsLC = ["LC1", "LC2", "LC3", "LC4", "LC5"]
traceMatsAndGraphsLCRef = makeListRef traceMatsAndGraphsLC (SRS.likeChg [] []) --Fixme:replace with actual LCs!

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

traceMatsAndGraphsTable1 = llcc (mkLabelRA'' "TraceyItemSecs") $ Table
  (EmptyS:traceMatsAndGraphsRowHdrT1)
  (makeTMatrix traceMatsAndGraphsRowHdrT1 traceMatsAndGraphsColsT1 traceMatsAndGraphsRowT1)
  (showingCxnBw traceyMatrix
  (titleize' item +:+ S "of Different" +:+ titleize' section_)) True "TraceyItemSecs"

--

traceMatsAndGraphsRowT2 :: [String]
traceMatsAndGraphsRowT2 = traceMatsAndGraphsRowT1 ++ traceMatsAndGraphsDataCons ++ traceMatsAndGraphsFuncReq

traceMatsAndGraphsRowHdrT2, traceMatsAndGraphsColHdrT2 :: [Sentence]
traceMatsAndGraphsRowHdrT2 = traceMatsAndGraphsRowHdrT1 ++
  (zipWith itemRefToSent (traceMatsAndGraphsDataCons ++ traceMatsAndGraphsFuncReq) (traceMatsAndGraphsDataConsRef ++ traceMatsAndGraphsFuncReqRef))

traceMatsAndGraphsColHdrT2 = zipWith (\x y -> (S x) +:+ (sParen (S "in" +:+ y)))
  traceMatsAndGraphsFuncReq traceMatsAndGraphsFuncReqRef

traceMatsAndGraphsColsT2_R1, traceMatsAndGraphsColsT2_R2, traceMatsAndGraphsColsT2_R3, traceMatsAndGraphsColsT2_R4, traceMatsAndGraphsColsT2_R5,
  traceMatsAndGraphsColsT2_R6 :: [String]

traceMatsAndGraphsColsT2 :: [[String]]
traceMatsAndGraphsColsT2 = [traceMatsAndGraphsColsT2_R1, traceMatsAndGraphsColsT2_R2, traceMatsAndGraphsColsT2_R3, traceMatsAndGraphsColsT2_R4, traceMatsAndGraphsColsT2_R5, traceMatsAndGraphsColsT2_R6]
traceMatsAndGraphsColsT2_R1 = []
traceMatsAndGraphsColsT2_R2 = []
traceMatsAndGraphsColsT2_R3 = ["Data Constraints"]
traceMatsAndGraphsColsT2_R4 = ["R1", "R2"]
traceMatsAndGraphsColsT2_R5 = ["T1", "T2"]
traceMatsAndGraphsColsT2_R6 = ["IM1", "IM2", "IM3", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8"]

traceMatsAndGraphsTable2 = llcc (mkLabelRA'' "TraceyReqsItems") $ Table
  (EmptyS:traceMatsAndGraphsRowHdrT2)
  (makeTMatrix traceMatsAndGraphsColHdrT2 traceMatsAndGraphsColsT2 traceMatsAndGraphsRowT2)
  (showingCxnBw traceyMatrix (titleize' requirement `sAnd` S "Other" +:+
  titleize' item)) True "TraceyReqsItems"

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

traceMatsAndGraphsTable3 = llcc (mkLabelRA'' "TraceyAssumpsOthers") $ Table
  (EmptyS:traceMatsAndGraphsRowHdr3)
  (makeTMatrix traceMatsAndGraphsColHdr3 traceMatsAndGraphsColsT3 traceMatsAndGraphsRowT3)
  (showingCxnBw traceyMatrix (titleize' assumption `sAnd` S "Other"
  +:+ titleize' item)) True "TraceyAssumpsOthers"

--

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
  sParen ((makeRef fig_5) `sAnd` (makeRef fig_6)),
  S "used for interpolating", plural value, S "needed in the", plural model]

fig_5 = llcc (mkLabelRA'' "demandVSsod") $ fig (titleize figure +: S "5" +:+ (demandq ^. defn) +:+
  sParen (ch demand) `sVersus` at_start sD +:+ sParen (getAcc stdOffDist)
  `sVersus` at_start char_weight +:+ sParen (ch char_weight))
  (resourcePath ++ "ASTM_F2248-09.png") "demandVSsod"

fig_6 = llcc (mkLabelRA'' "dimlessloadVSaspect") $ fig (titleize figure +: S "6" +:+ S "Non dimensional" +:+
  phrase lateralLoad +:+ sParen (ch dimlessLoad)
  `sVersus` titleize aspect_ratio +:+ sParen (getAcc aR)
  `sVersus` at_start stressDistFac +:+ sParen (ch stressDistFac))
  (resourcePath ++ "ASTM_F2248-09_BeasonEtAl.png") "dimlessloadVSaspect"

blstRskInvWGlassSlab :: Sentence
blstRskInvWGlassSlab = phrase blastRisk +:+ S "involved with the" +:+
  phrase glaSlab
