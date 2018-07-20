module Drasil.GlassBR.Body where

import Control.Lens ((^.))
import Data.List (nub)

import Language.Drasil hiding (organization)
import Language.Drasil.Code (CodeSpec, codeSpec, relToQD)
import qualified Drasil.DocLang.SRS as SRS

import Drasil.DocLang (AppndxSec(..), AuxConstntSec(..), DerivationDisplay(..), 
  DocDesc, DocSection(..), Field(..), Fields, GSDSec(GSDProg2), 
  GSDSub(UsrChars, SystCons), InclUnits(IncludeUnits), IntroSec(IntroProg),
  IntroSub(IChar, IOrgSec, IPurpose, IScope), LCsSec(..), ProblemDescription(..), 
  RefSec(RefProg), RefTab(TAandA, TUnits), ReqrmntSec(..), 
  ReqsSub(FReqsSub, NonFReqsSub), ScpOfProjSec(ScpOfProjProg), SCSSub(..), 
  SSDSec(..), SSDSub(..), SolChSpec(..), StkhldrSec(StkhldrProg2), 
  StkhldrSub(Client, Cstmr), TraceabilitySec(TraceabilityProg), 
  TSIntro(SymbOrder, TSPurpose), UCsSec(..), Verbosity(Verbose), cite, 
  dataConstraintUncertainty, goalStmtF, inDataConstTbl, intro, mkDoc, 
  mkRequirement, outDataConstTbl, physSystDesc, probDescF, termDefnF, 
  traceGIntro, tsymb)

import Data.Drasil.Concepts.Computation (computerApp, inParam,
  computerLiteracy, inValue, inQty)
import Data.Drasil.Concepts.Documentation as Doc (analysis, appendix, aspect, 
  assumption, characteristic, class_, code, company, condition, content, dataConst, 
  dataDefn, datum, datumConstraint, definition, description, document, emphasis, 
  endUser, failure, figure, goal, goalStmt, implementation, information, inModel, 
  input_, interface, item, likelyChg, message, model, organization, output_, 
  physicalSystem, physSyst, problem, purpose, quantity, reference, requirement, 
  reviewer, section_, software, srs, standard, symbol_, system, template, term_, 
  theory, thModel, traceyMatrix, user, userInput, value)
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
  foldlSent, foldlSent_, figureLabel, foldlList, showingCxnBw,
  foldlsC, sOf, followA, ofThe, sIn, isThe, isExpctdToHv, sOr, underConsidertn,
  tAndDWAcc, tAndDOnly, tAndDWSym, andThe)
import Data.Drasil.Software.Products (sciCompS)
import Data.Drasil.Utils (makeTMatrix, makeListRef, itemRefToSent,
  refFromType, enumSimple, enumBullet, prodUCTbl)

import Drasil.GlassBR.Assumptions (assumptionConstants, assumptionDescs,
  gbRefDB, newAssumptions)
import Drasil.GlassBR.Changes (likelyChanges_SRS, unlikelyChanges_SRS)
import Drasil.GlassBR.Concepts (aR, lShareFac, gLassBR, stdOffDist, glaSlab, 
  blastRisk, glass, glaPlane, glassBRProg, ptOfExplsn, acronyms)
import Drasil.GlassBR.DataDefs (aspRat, dataDefns, gbQDefns, hFromt, strDisFac, nonFL, 
  dimLL, glaTyFac, tolStrDisFac, tolPre, risk, standOffDis)
import Drasil.GlassBR.ModuleDefs (allMods)
import Drasil.GlassBR.References (rbrtsn2012)
import Drasil.GlassBR.Symbols (this_symbols)
import Drasil.GlassBR.TMods (tModels, t1SafetyReq, t2SafetyReq, t1IsSafe, t2IsSafe)
import Drasil.GlassBR.IMods (iModels, calOfCap, calOfDe, probOfBr, probOfBreak, 
  calofCapacity, calofDemand)

import Drasil.GlassBR.Unitals (stressDistFac, aspectR, dimlessLoad, 
  lateralLoad, char_weight, sD, demand, demandq, aspectR, lRe, wtntWithEqn, 
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

resourcePath :: String
resourcePath = "../../../datafiles/GlassBR/"

glassBR_srs :: Document
glassBR_srs = mkDoc mkSRS (for'' titleize phrase) glassSystInfo

mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA]) :
  IntroSec (
    IntroProg (startIntro software blstRskInvWGlassSlab gLassBR)
      (short gLassBR)
    [IPurpose (purpose_of_document_intro_p1 document gLassBR glaSlab),
     IScope incScoR endScoR,
     IChar (rdrKnldgbleIn glBreakage blastRisk) undIR appStanddIR,
     IOrgSec org_of_doc_intro dataDefn (SRS.dataDefn SRS.missingP []) org_of_doc_intro_end]) :
  StkhldrSec
    (StkhldrProg2
      [Client gLassBR (S "a" +:+ phrase company
        +:+ S "named Entuitive. It is developed by Dr." +:+ (S $ name mCampidelli)),
      Cstmr gLassBR]) :
  GSDSec (GSDProg2 [UsrChars [user_characteristics_bullets endUser gLassBR secondYear
    undergradDegree civilEng structuralEng glBreakage blastRisk],
    SystCons [] []]) :
  ScpOfProjSec (ScpOfProjProg (short gLassBR) product_use_case_table (individual_product_use_case glaSlab
    capacity demandq probability)) :
  SSDSec 
    (SSDProg
      [SSDProblem  (PDProg start gLassBR ending [terminology_and_description, physical_system_description, goal_statements])
      , SSDSolChSpec 
        (SCSProg
          [ Assumptions
          , TMs ([Label] ++ stdFields) [t1IsSafe, t2IsSafe]
          , GDs [] [] HideDerivation -- No Gen Defs for GlassBR
          , DDs' ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
          , IMs ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) [probOfBreak, calofCapacity, calofDemand] HideDerivation
          , Constraints EmptyS dataConstraintUncertainty
                        (foldlSent [(makeRef (SRS.valsOfAuxCons SRS.missingP [])), S "gives", (plural value `ofThe` S "specification"), 
                        plural parameter, S "used in", (makeRef inputDataConstraints)])
                        [inputDataConstraints, outputDataConstraints]
          ]
        )
      ]
    ) :
  ReqrmntSec (ReqsProg [
    FReqsSub functional_requirements_list, 
    NonFReqsSub [performance] (gBRpriorityNFReqs)
    (S "This problem is small in size and relatively simple")
    (S "Any reasonable" +:+ phrase implementation +:+.
    (S "will be very quick" `sAnd` S "use minimal storage"))]) :
  LCsSec (LCsProg likely_changes_list) :
  UCsSec (UCsProg unlikely_change_list) :
  TraceabilitySec
    (TraceabilityProg traceyMatrices [traceability_matrices_and_graphs_table1Desc, traceability_matrices_and_graphs_table2Desc, traceability_matrices_and_graphs_table3Desc]
    (traceyMatrices ++ traceability_matrices_and_graphs_intro2 ++ traceyGraphs) []) :
  AuxConstntSec (AuxConsProg gLassBR auxiliaryConstants) :
  Bibliography :
  AppndxSec (AppndxProg [appendix_intro, fig_5, fig_6]) : []
 
stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

glassSystInfo :: SystemInformation
glassSystInfo = SI {
  _sys         = glassBRProg,
  _kind        = srs,
  _authors     = [nikitha, spencerSmith],
  _units       = map unitWrapper [metre, second, kilogram] ++ map unitWrapper [pascal, newton],
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

problem_description, terminology_and_description, 
  physical_system_description, goal_statements :: Section

product_use_case_table,
  physical_system_description_list, inputDataConstraints,
  outputDataConstraints, traceability_matrices_and_graphs_table1,
  traceability_matrices_and_graphs_table2, traceability_matrices_and_graphs_table3, appendix_intro,
  fig_glassbr, fig_2, fig_3, fig_4, fig_5,
  fig_6 :: Contents

functional_requirements_list, traceability_matrices_and_graphs_intro2 :: [Contents]

--------------------------------------------------------------------------------
terminology_and_description_bullets :: Contents
terminology_and_description_bullets = Enumeration $ (Numeric $
  map tAndDOnly termsWithDefsOnly
  ++
  terminology_and_description_bullets_glTySubSec
  ++
  terminology_and_description_bullets_loadSubSec
  ++
  map tAndDWAcc termsWithAccDefn
  ++
  [tAndDWSym probBreak prob_br])
   --FIXME: merge? Needs 2 arguments because there is no instance for (SymbolForm ConceptChunk)...

terminology_and_description_bullets_glTySubSec, terminology_and_description_bullets_loadSubSec :: [ItemType]

terminology_and_description_bullets_glTySubSec = [Nested ((titleize glassTy) :+: S ":")
  (Bullet $ map tAndDWAcc glassTypes)]

terminology_and_description_bullets_loadSubSec = [Nested ((at_start load) +:+ S "-" +:+ (load ^. defn))
  (Bullet $ map tAndDWAcc (take 2 loadTypes)
  ++
  map tAndDOnly (drop 2 loadTypes))]

--Used in "Goal Statements" Section--
goal_statements_list :: Contents
goal_statements_list = enumSimple 1 (short goalStmt) goal_statements_list_goalStmt1

--Used in "Traceability Matrices and Graphs" Section--
traceyMatrices, traceyGraphs :: [Contents]
traceyMatrices = [traceability_matrices_and_graphs_table1, traceability_matrices_and_graphs_table2, traceability_matrices_and_graphs_table3]
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

functional_requirements_req6_pulledList :: [QDefinition]
functional_requirements_req6_pulledList = [risk, strDisFac, nonFL, glaTyFac, dimLL, 
  tolPre, tolStrDisFac, hFromt, aspRat]

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
undIR = foldlList [phrase scndYrCalculus, phrase structuralMechanics,
  plural computerApp `sIn` phrase civilEng]
appStanddIR = foldlSent [S " In addition" `sC` plural reviewer, -- FIXME: space before "In" is a hack to get proper spacing
  S "should be familiar with the applicable", plural standard,
  S "for constructions using glass from",
  sSqBr (S "1-3" {-astm2009, astm2012, astm2016-}) `sIn`
  (makeRef (SRS.reference SRS.missingP []))]
incScoR = foldl (+:+) EmptyS [S "getting all", plural inParam,
  S "related to the", phrase glaSlab `sAnd` S "also the", plural parameter,
  S "related to", phrase blastTy]
endScoR = foldl (+:+) EmptyS [S "predicts whether a", phrase glaSlab, 
  S "is safe" `sOr` S "not"]

{--Purpose of Document--}

purpose_of_document_intro_p1 :: NamedChunk -> CI -> NamedChunk -> Sentence
purpose_of_document_intro_p1 typeOf progName gvnVar = foldlSent [S "The main", phrase purpose,
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

org_of_doc_intro_end, org_of_doc_intro :: Sentence
org_of_doc_intro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the", phrase template, S "for an", short srs,
  S "for", phrase sciCompS, S "proposed by" +:+ cite gbRefDB koothoor2013
  `sAnd` cite gbRefDB smithLai2005 `sC` S "with some", 
  plural aspect, S "taken from Volere", phrase template,
  S "16", cite gbRefDB rbrtsn2012]

org_of_doc_intro_end = foldl (+:+) EmptyS [(at_start' $ the dataDefn),
  S "are used to support", (plural definition `ofThe` S "different"),
  plural model]

{--STAKEHOLDERS--}

{--The Client--}
{--The Customer--}

{--GENERAL SYSTEM DESCRIPTION--}

{--User Characteristics--}

user_characteristics_bullets :: (NamedIdea n1, NamedIdea n, NamedIdea n2, NamedIdea n3,
  NamedIdea n4, NamedIdea n5, Idea c, NamedIdea n6) =>
  n6 -> c -> n5 -> n4 -> n3 -> n2 -> n1 -> n -> Contents
user_characteristics_bullets intendedIndvdl progName yr degreeType prog1 prog2 undrstd1 undrstd2
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

product_use_case_table = prodUCTbl [product_use_case_table_UC1, product_use_case_table_UC2]

product_use_case_table_UC1, product_use_case_table_UC2 :: [Sentence]

product_use_case_table_UC1 = [titleize user, titleize' characteristic +:+ S "of the"
  +:+ phrase glaSlab `sAnd` S "of the" +:+. phrase blast +:+ S "Details in"
  +:+ makeRef (SRS.indPRCase SRS.missingP [])]

product_use_case_table_UC2 = [short gLassBR, S "Whether" `sOr` S "not the" +:+
  phrase glaSlab +:+ S "is safe for the" +:+ S "calculated" +:+ phrase load
  `sAnd` S "supporting calculated" +:+ plural value]

{--Individual Product Use Case--}

individual_product_use_case :: NamedChunk -> ConceptChunk -> ConceptChunk -> ConceptChunk ->
  Contents
individual_product_use_case mainObj compare1 compare2 factorOfComparison =
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

start, ending :: Sentence
start = foldlSent [S "A", phrase system,
  S "is needed to efficiently" `sAnd` S "correctly predict the",
  phrase blastRisk +:+ S "involved with the glass"]
ending = foldl (+:+) EmptyS [S "interpret the", plural input_,
  S "to give out the", plural output_,
  S "which predict whether the", phrase glaSlab,
  S "can withstand the", phrase blast, S "under the",
  plural condition]

problem_description = probDescF start gLassBR ending [terminology_and_description, physical_system_description, goal_statements]

{--Terminology and Definitions--}

terminology_and_description = termDefnF (Just (S "All" `sOf` S "the" +:+ plural term_ +:+
  S "are extracted from" +:+ (sSqBrNum 1 {-astm2009-}) `sIn`
  (makeRef (SRS.reference SRS.missingP [])))) [terminology_and_description_bullets]

{--Physical System Description--}

physical_system_description = physSystDesc (short gLassBR) fig_glassbr [physical_system_description_list, fig_glassbr]

fig_glassbr = figWithWidth (at_start $ the physicalSystem) (resourcePath ++ "physicalsystimage.png") 30
  "physSystImage"

physical_system_description_list = enumSimple 1 (short physSyst) physical_system_description_list_physys

--"Dead" knowledge?
physical_system_description_list_physys :: [Sentence]
physical_system_description_list_physys1 :: Sentence
physical_system_description_list_physys2 :: NamedIdea n => n -> Sentence

physical_system_description_list_physys = [physical_system_description_list_physys1, physical_system_description_list_physys2 (ptOfExplsn)]

physical_system_description_list_physys1 = S "The" +:+. phrase glaSlab

physical_system_description_list_physys2 imprtntElem = foldlSent [S "The"
  +:+. phrase imprtntElem, S "Where the", phrase bomb `sC`
  S "or", (blast ^. defn) `sC` S "is located. The", phrase sD
  `isThe` phrase distance, S "between the", phrase imprtntElem `sAnd`
  S "the glass"]

{--Goal Statements--}

goal_statements = goalStmtF [foldlList [plural dimension `ofThe` phrase glaPlane,
  phrase glassTy, plural characteristic `ofThe` phrase explosion,
  S "the" +:+ phrase pb_tol]] [goal_statements_list]

goal_statements_list_goalStmt1 :: [Sentence]
goal_statements_list_goalStmt1 = [foldlSent [S "Analyze" `sAnd` S "predict whether",
  S "the", phrase glaSlab, S "under consideration will be able to withstand",
  S "the", phrase explosion `sOf` S "a certain", phrase degree_',
  S "which is calculated based on", phrase userInput]]

{--SOLUTION CHARACTERISTICS SPECIFICATION--}

--Automatically generated

{--Assumptions--}

assumptions_list :: [Contents]
assumptions_list = assumpList newAssumptions

assumpList :: [AssumpChunk] -> [Contents]
assumpList = map Assumption

assumptions :: [Contents] -- FIXME: Remove this entirely and use new refs + docLang.
assumptions = fst (foldr (\s (ls, n) -> ((Assumption $ assump ("A" ++ show n) s ("A" ++ show n)) : ls, n-1))
 ([], (length assumptionDescs)::Int) assumptionDescs)
-- These correspond to glassTyAssumps, glassCondition, explsnScenario,
-- standardValues, glassLiteAssmp, bndryConditions, responseTyAssump, ldfConstant

{--Theoretical Models--}

{--Data Definitions--}

{--Data Constraints--}

{-input and output tables-}

inputDataConstraints = inDataConstTbl gbInputDataConstraints
outputDataConstraints = outDataConstTbl [prob_br]

{--REQUIREMENTS--}

{--Functional Requirements--}

functional_requirements_list = functional_requirements_listOfReqs ++ functional_requirements_req6 ++ [functional_requirements_req1Table]

functional_requirements_req1, functional_requirements_req2, functional_requirements_req3, functional_requirements_req4, functional_requirements_req5 :: Contents
req1Desc, req2Desc, req3Desc, req4Desc :: Sentence
req5Desc :: NamedChunk -> Sentence
functional_requirements_req6 :: [Contents] --FIXME: Issue #327

functional_requirements_listOfReqs :: [Contents]
functional_requirements_listOfReqs = [functional_requirements_req1, functional_requirements_req2, functional_requirements_req3, functional_requirements_req4, functional_requirements_req5]

functional_requirements_req1 = mkRequirement "functional_requirements_req1" req1Desc "Input-Glass-Props"
functional_requirements_req2 = mkRequirement "functional_requirements_req2" req2Desc "System-Set-Values-Following-Assumptions"
functional_requirements_req3 = mkRequirement "functional_requirements_req3" req3Desc "Check-Input-with-Data_Constraints"
functional_requirements_req4 = mkRequirement "functional_requirements_req4" req4Desc "Output-Values-and-Known-Quantities"
functional_requirements_req5 = mkRequirement "functional_requirements_req5" (req5Desc (output_)) "Check-Glass-Safety"

req1Desc = foldlSent [at_start input_, S "the", plural quantity, S "from",
  makeRef functional_requirements_req1Table `sC` S "which define the", phrase glass,
  plural dimension `sC` (glassTy ^. defn) `sC` S "tolerable",
  phrase probability `sOf` phrase failure, S "and",
  (plural characteristic `ofThe` phrase blast), S "Note:",
  ch plate_len `sAnd` ch plate_width,
  S "will be input in terms of", plural millimetre `sAnd`
  S "will be converted to the equivalent value in", plural metre]

functional_requirements_req1Table :: Contents
functional_requirements_req1Table = Table
  [at_start symbol_, at_start description, S "Units"]
  (mkTable
  [ch,
   at_start, unitToSentence] requiredInputs)
  (S "Required Inputs following R1") True "R1ReqInputs"

req2Desc = foldlSent [S "The", phrase system,
  S "shall set the known", plural value +: S "as follows",
  foldlList [(foldlsC (map ch (take 4 assumptionConstants)) `followA` 4),
  ((ch constant_LoadDF) `followA` 8), (short lShareFac `followA` 5),
  (ch hFromt) +:+ sParen (S "from" +:+ (makeRef hFromt)), 
  (ch glaTyFac) +:+ sParen (S "from" +:+ (makeRef glaTyFac)),
  (ch standOffDis) +:+ sParen (S "from" +:+ (makeRef standOffDis))]]

--ItemType
{-functional_requirements_req2 = (Nested (S "The" +:+ phrase system +:+
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
  plural datumConstraint, S "mentioned in" +:+. makeRef
  (SRS.datCon SRS.missingP []), S "If any" `sOf` S "the", plural inParam,
  S "is out" `sOf` S "bounds" `sC` S "an", phrase errMsg, S "is displayed"
  `andThe` plural calculation, S "stop"]

req4Desc = foldlSent [titleize output_, S "the", plural inQty,
  S "from", acroR 1 `andThe` S "known", plural quantity,
  S "from", acroR 2]

req5Desc cmd = foldlSent_ [S "If", (ch is_safe1), S "∧", (ch is_safe2),
  sParen (S "from" +:+ (makeRef (reldefn t1SafetyReq))
  `sAnd` (makeRef (reldefn t2SafetyReq))), S "are true" `sC`
  phrase cmd, S "the", phrase message, Quote (safeMessage ^. defn),
  S "If the", phrase condition, S "is false, then", phrase cmd,
  S "the", phrase message, Quote (notSafe ^. defn)]

testing :: [QuantityDict]
testing = qw prob_br : qw lRe : qw demand : [] -- all different types!
testing1 :: [RelationConcept]
testing1 = [probOfBr, calOfCap, calOfDe]
--FIXME: rename or find better implementation?

functional_requirements_req6 = [(Enumeration $ Simple $ [(acroR 6, Nested (titleize output_ +:+
  S "the following" +: plural quantity)
  (Bullet $
    map (\(a, d) -> Flat $ (at_start a) +:+ sParen (ch a) +:+
    sParen (makeRef (reldefn d))) (zip testing testing1)
    ++
    map (\d -> Flat $ (at_start d) +:+ sParen (ch d) +:+
    sParen (makeRef (datadefn d))) functional_requirements_req6_pulledList))])]

{--Nonfunctional Requirements--}

{--LIKELY CHANGES--}

likely_changes_list :: [Contents]
likely_changes_list = likelyChanges_SRS 

{--UNLIKELY CHANGES--}
unlikely_change_list :: [Contents]
unlikely_change_list = unlikelyChanges_SRS

{--TRACEABLITY MATRICES AND GRAPHS--}

traceability_matrices_and_graphs_table1Desc :: Sentence
traceability_matrices_and_graphs_table1Desc = foldlList (map plural (take 3 solChSpecSubsections)) +:+.
  S "with each other"

traceability_matrices_and_graphs_table2Desc :: Sentence
traceability_matrices_and_graphs_table2Desc = plural requirement +:+ S "on" +:+. foldlList
  (map plural solChSpecSubsections)

traceability_matrices_and_graphs_table3Desc :: Sentence
traceability_matrices_and_graphs_table3Desc = foldlsC (map plural (take 3 solChSpecSubsections)) `sC`
  plural likelyChg `sAnd` plural requirement +:+ S "on the" +:+
  plural assumption

traceability_matrices_and_graphs_theorys, traceability_matrices_and_graphs_instaModel, traceability_matrices_and_graphs_dataDef, traceability_matrices_and_graphs_data, traceability_matrices_and_graphs_funcReq, traceability_matrices_and_graphs_assump,
  traceability_matrices_and_graphs_likelyChg :: [String]

traceability_matrices_and_graphs_theorysRef, traceability_matrices_and_graphs_instaModelRef, traceability_matrices_and_graphs_dataDefRef, traceability_matrices_and_graphs_dataRef, traceability_matrices_and_graphs_funcReqRef,
  traceability_matrices_and_graphs_assumpRef, traceability_matrices_and_graphs_likelyChgRef :: [Sentence]

traceability_matrices_and_graphs_theorys = ["T1", "T2"]
traceability_matrices_and_graphs_theorysRef = map (refFromType Theory) tModels

traceability_matrices_and_graphs_instaModel = ["IM1", "IM2", "IM3"]
traceability_matrices_and_graphs_instaModelRef = map (refFromType Theory) iModels

traceability_matrices_and_graphs_dataDef =  ["DD1", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8"]
traceability_matrices_and_graphs_dataDefRef = map (refFromType Data') dataDefns

traceability_matrices_and_graphs_data  = ["Data Constraints"]
traceability_matrices_and_graphs_dataRef = [makeRef (SRS.datCon SRS.missingP [])]

traceability_matrices_and_graphs_funcReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
traceability_matrices_and_graphs_funcReqRef = makeListRef traceability_matrices_and_graphs_funcReq (SRS.funcReq SRS.missingP [])

traceability_matrices_and_graphs_assump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"]
traceability_matrices_and_graphs_assumpRef = makeListRef traceability_matrices_and_graphs_assump (SRS.assumpt SRS.missingP [])

traceability_matrices_and_graphs_likelyChg = ["LC1", "LC2", "LC3", "LC4", "LC5"]
traceability_matrices_and_graphs_likelyChgRef = makeListRef traceability_matrices_and_graphs_likelyChg (SRS.likeChg SRS.missingP [])

traceability_matrices_and_graphs_row_t1 :: [String]
traceability_matrices_and_graphs_row_t1 = traceability_matrices_and_graphs_theorys ++ traceability_matrices_and_graphs_instaModel ++ traceability_matrices_and_graphs_dataDef

-- The headers for the first row, and column
traceability_matrices_and_graphs_row_header_t1 :: [Sentence]
traceability_matrices_and_graphs_row_header_t1 = zipWith itemRefToSent traceability_matrices_and_graphs_row_t1 (traceability_matrices_and_graphs_theorysRef ++
  traceability_matrices_and_graphs_instaModelRef ++ traceability_matrices_and_graphs_dataDefRef)

-- list of columns and their rows for traceability matrix
traceability_matrices_and_graphs_columns_t1 :: [[String]]
traceability_matrices_and_graphs_columns_t1 = [traceability_matrices_and_graphs_t1_T1, traceability_matrices_and_graphs_t1_T2, traceability_matrices_and_graphs_t1_IM1, traceability_matrices_and_graphs_t1_IM2, traceability_matrices_and_graphs_t1_IM3,
  traceability_matrices_and_graphs_t1_DD1, traceability_matrices_and_graphs_t1_DD2, traceability_matrices_and_graphs_t1_DD3, traceability_matrices_and_graphs_t1_DD4, traceability_matrices_and_graphs_t1_DD5, traceability_matrices_and_graphs_t1_DD6, traceability_matrices_and_graphs_t1_DD7,
  traceability_matrices_and_graphs_t1_DD8]

traceability_matrices_and_graphs_t1_T1, traceability_matrices_and_graphs_t1_T2, traceability_matrices_and_graphs_t1_IM1, traceability_matrices_and_graphs_t1_IM2, traceability_matrices_and_graphs_t1_IM3, traceability_matrices_and_graphs_t1_DD1, traceability_matrices_and_graphs_t1_DD2,
  traceability_matrices_and_graphs_t1_DD3, traceability_matrices_and_graphs_t1_DD4, traceability_matrices_and_graphs_t1_DD5, traceability_matrices_and_graphs_t1_DD6, traceability_matrices_and_graphs_t1_DD7, traceability_matrices_and_graphs_t1_DD8 :: [String]

-- list of each item that "this" item requires for traceability matrix
traceability_matrices_and_graphs_t1_T1  = ["T2", "IM1"]
traceability_matrices_and_graphs_t1_T2  = ["T1", "IM2", "IM3"]
traceability_matrices_and_graphs_t1_IM1 = ["DD1", "DD2", "DD3"]
traceability_matrices_and_graphs_t1_IM2 = ["DD4", "DD5"]
traceability_matrices_and_graphs_t1_IM3 = []
traceability_matrices_and_graphs_t1_DD1 = []
traceability_matrices_and_graphs_t1_DD2 = []
traceability_matrices_and_graphs_t1_DD3 = ["DD6"]
traceability_matrices_and_graphs_t1_DD4 = ["DD2", "DD6"]
traceability_matrices_and_graphs_t1_DD5 = []
traceability_matrices_and_graphs_t1_DD6 = ["IM3", "DD2", "DD5"]
traceability_matrices_and_graphs_t1_DD7 = ["DD8"]
traceability_matrices_and_graphs_t1_DD8 = ["DD2"]

traceability_matrices_and_graphs_table1 = Table (EmptyS:traceability_matrices_and_graphs_row_header_t1)
  (makeTMatrix traceability_matrices_and_graphs_row_header_t1 traceability_matrices_and_graphs_columns_t1 traceability_matrices_and_graphs_row_t1)
  (showingCxnBw traceyMatrix
  (titleize' item +:+ S "of Different" +:+ titleize' section_)) True "TraceyItemSecs"

--

traceability_matrices_and_graphs_row_t2 :: [String]
traceability_matrices_and_graphs_row_t2 = traceability_matrices_and_graphs_row_t1 ++ traceability_matrices_and_graphs_data ++ traceability_matrices_and_graphs_funcReq

traceability_matrices_and_graphs_row_header_t2, traceability_matrices_and_graphs_col_header_t2 :: [Sentence]
traceability_matrices_and_graphs_row_header_t2 = traceability_matrices_and_graphs_row_header_t1 ++
  (zipWith itemRefToSent (traceability_matrices_and_graphs_data ++ traceability_matrices_and_graphs_funcReq) (traceability_matrices_and_graphs_dataRef ++ traceability_matrices_and_graphs_funcReqRef))

traceability_matrices_and_graphs_col_header_t2 = zipWith (\x y -> (S x) +:+ (sParen (S "in" +:+ y)))
  traceability_matrices_and_graphs_funcReq traceability_matrices_and_graphs_funcReqRef

traceability_matrices_and_graphs_t2_r1, traceability_matrices_and_graphs_t2_r2, traceability_matrices_and_graphs_t2_r3, traceability_matrices_and_graphs_t2_r4, traceability_matrices_and_graphs_t2_r5,
  traceability_matrices_and_graphs_t2_r6 :: [String]

traceability_matrices_and_graphs_columns_t2 :: [[String]]
traceability_matrices_and_graphs_columns_t2 = [traceability_matrices_and_graphs_t2_r1, traceability_matrices_and_graphs_t2_r2, traceability_matrices_and_graphs_t2_r3, traceability_matrices_and_graphs_t2_r4, traceability_matrices_and_graphs_t2_r5, traceability_matrices_and_graphs_t2_r6]
traceability_matrices_and_graphs_t2_r1 = []
traceability_matrices_and_graphs_t2_r2 = []
traceability_matrices_and_graphs_t2_r3 = ["Data Constraints"]
traceability_matrices_and_graphs_t2_r4 = ["R1", "R2"]
traceability_matrices_and_graphs_t2_r5 = ["T1", "T2"]
traceability_matrices_and_graphs_t2_r6 = ["IM1", "IM2", "IM3", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8"]

traceability_matrices_and_graphs_table2 = Table (EmptyS:traceability_matrices_and_graphs_row_header_t2)
  (makeTMatrix traceability_matrices_and_graphs_col_header_t2 traceability_matrices_and_graphs_columns_t2 traceability_matrices_and_graphs_row_t2)
  (showingCxnBw traceyMatrix (titleize' requirement `sAnd` S "Other" +:+
  titleize' item)) True "TraceyReqsItems"

--

traceability_matrices_and_graphs_row_t3 :: [String]
traceability_matrices_and_graphs_row_t3 = traceability_matrices_and_graphs_assump

traceability_matrices_and_graphs_row_header_t3, traceability_matrices_and_graphs_col_header_t3 :: [Sentence]
traceability_matrices_and_graphs_row_header_t3 = zipWith itemRefToSent traceability_matrices_and_graphs_assump traceability_matrices_and_graphs_assumpRef

traceability_matrices_and_graphs_col_header_t3 = traceability_matrices_and_graphs_row_header_t1 ++ (zipWith itemRefToSent
  (traceability_matrices_and_graphs_likelyChg ++ traceability_matrices_and_graphs_funcReq) (traceability_matrices_and_graphs_likelyChgRef ++ traceability_matrices_and_graphs_funcReqRef))

traceability_matrices_and_graphs_columns_t3 :: [[String]]
traceability_matrices_and_graphs_columns_t3 = [traceability_matrices_and_graphs_t3_T1, traceability_matrices_and_graphs_t3_T2, traceability_matrices_and_graphs_t3_IM1, traceability_matrices_and_graphs_t3_IM2, traceability_matrices_and_graphs_t3_IM3, traceability_matrices_and_graphs_t3_DD1,
  traceability_matrices_and_graphs_t3_DD2, traceability_matrices_and_graphs_t3_DD3, traceability_matrices_and_graphs_t3_DD4, traceability_matrices_and_graphs_t3_DD5, traceability_matrices_and_graphs_t3_DD6, traceability_matrices_and_graphs_t3_DD7, traceability_matrices_and_graphs_t3_DD8,
  traceability_matrices_and_graphs_t3_lc1, traceability_matrices_and_graphs_t3_lc2, traceability_matrices_and_graphs_t3_lc3, traceability_matrices_and_graphs_t3_lc4, traceability_matrices_and_graphs_t3_lc5, traceability_matrices_and_graphs_t3_r1, traceability_matrices_and_graphs_t3_r2,
  traceability_matrices_and_graphs_t3_r3, traceability_matrices_and_graphs_t3_r4, traceability_matrices_and_graphs_t3_r5, traceability_matrices_and_graphs_t3_r6]

traceability_matrices_and_graphs_t3_T1, traceability_matrices_and_graphs_t3_T2, traceability_matrices_and_graphs_t3_IM1, traceability_matrices_and_graphs_t3_IM2, traceability_matrices_and_graphs_t3_IM3, traceability_matrices_and_graphs_t3_DD1, traceability_matrices_and_graphs_t3_DD2,
  traceability_matrices_and_graphs_t3_DD3, traceability_matrices_and_graphs_t3_DD4, traceability_matrices_and_graphs_t3_DD5, traceability_matrices_and_graphs_t3_DD6, traceability_matrices_and_graphs_t3_DD7, traceability_matrices_and_graphs_t3_DD8,
  traceability_matrices_and_graphs_t3_lc1, traceability_matrices_and_graphs_t3_lc2, traceability_matrices_and_graphs_t3_lc3, traceability_matrices_and_graphs_t3_lc4, traceability_matrices_and_graphs_t3_lc5, traceability_matrices_and_graphs_t3_r1,
  traceability_matrices_and_graphs_t3_r2, traceability_matrices_and_graphs_t3_r3, traceability_matrices_and_graphs_t3_r4, traceability_matrices_and_graphs_t3_r5, traceability_matrices_and_graphs_t3_r6 :: [String]

-- list of each item that "this" item requires for traceability matrix
traceability_matrices_and_graphs_t3_T1  = []
traceability_matrices_and_graphs_t3_T2  = []
traceability_matrices_and_graphs_t3_IM1 = ["A4", "A6", "A7"]
traceability_matrices_and_graphs_t3_IM2 = ["A1", "A2", "A5"]
traceability_matrices_and_graphs_t3_IM3 = []
traceability_matrices_and_graphs_t3_DD1 = []
traceability_matrices_and_graphs_t3_DD2 = []
traceability_matrices_and_graphs_t3_DD3 = []
traceability_matrices_and_graphs_t3_DD4 = ["A4"]
traceability_matrices_and_graphs_t3_DD5 = []
traceability_matrices_and_graphs_t3_DD6 = ["A5"]
traceability_matrices_and_graphs_t3_DD7 = []
traceability_matrices_and_graphs_t3_DD8 = ["A4"]
traceability_matrices_and_graphs_t3_lc1 = ["A3"]
traceability_matrices_and_graphs_t3_lc2 = ["A4", "A8"]
traceability_matrices_and_graphs_t3_lc3 = ["A5"]
traceability_matrices_and_graphs_t3_lc4 = ["A6"]
traceability_matrices_and_graphs_t3_lc5 = ["A7"]
traceability_matrices_and_graphs_t3_r1  = []
traceability_matrices_and_graphs_t3_r2  = ["A4", "A5", "A8"]
traceability_matrices_and_graphs_t3_r3  = []
traceability_matrices_and_graphs_t3_r4  = []
traceability_matrices_and_graphs_t3_r5  = []
traceability_matrices_and_graphs_t3_r6  = []

traceability_matrices_and_graphs_table3 = Table (EmptyS:traceability_matrices_and_graphs_row_header_t3)
  (makeTMatrix traceability_matrices_and_graphs_col_header_t3 traceability_matrices_and_graphs_columns_t3 traceability_matrices_and_graphs_row_t3)
  (showingCxnBw traceyMatrix (titleize' assumption `sAnd` S "Other"
  +:+ titleize' item)) True "TraceyAssumpsOthers"

--

traceability_matrices_and_graphs_intro2 = traceGIntro traceyGraphs
  [(foldlList (map plural (take 3 solChSpecSubsections)) +:+.
  S "on each other"), (plural requirement +:+ S "on" +:+. foldlList
  (map plural solChSpecSubsections)),
  (foldlList ((map plural (take 3 solChSpecSubsections))++
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

appendix_intro = foldlSP [
  S "This", phrase appendix, S "holds the", plural graph,
  sParen ((makeRef fig_5) `sAnd` (makeRef fig_6)),
  S "used for interpolating", plural value, S "needed in the", plural model]

fig_5 = fig (titleize figure +: S "5" +:+ (demandq ^. defn) +:+
  sParen (ch demand) `sVersus` at_start sD +:+ sParen (getAcc stdOffDist)
  `sVersus` at_start char_weight +:+ sParen (ch char_weight))
  (resourcePath ++ "ASTM_F2248-09.png") "demandVSsod"

fig_6 = fig (titleize figure +: S "6" +:+ S "Non dimensional" +:+
  phrase lateralLoad +:+ sParen (ch dimlessLoad)
  `sVersus` titleize aspectR +:+ sParen (getAcc aR)
  `sVersus` at_start stressDistFac +:+ sParen (ch stressDistFac))
  (resourcePath ++ "ASTM_F2248-09_BeasonEtAl.png") "dimlessloadVSaspect"

blstRskInvWGlassSlab :: Sentence
blstRskInvWGlassSlab = phrase blastRisk +:+ S "involved with the" +:+
  phrase glaSlab
