module Drasil.GlassBR.Body where
import Control.Lens ((^.))
import Language.Drasil hiding (organization)
import qualified Drasil.SRS as SRS

import Drasil.DocumentLanguage (AppndxSec(..), AuxConstntSec(..),
  DocSection(..), GSDSec(GSDProg2), GSDSub(UsrChars, SystCons), --DocSection uses everything but Verbatim
  IntroSec(IntroProg), IntroSub(IChar, IOrgSec, IPurpose, IScope), LCsSec(..), 
  UCsSec(..), RefSec(RefProg), RefTab(TAandA, TUnits), ReqrmntSec(..), 
  ReqsSub(FReqsSub, NonFReqsSub), ScpOfProjSec(ScpOfProjProg), SSDSec(SSDVerb), 
  StkhldrSec(StkhldrProg2), StkhldrSub(Client, Cstmr), 
  TraceabilitySec(TraceabilityProg), TSIntro(SymbOrder, TSPurpose), DocDesc, 
  mkDoc, mkRequirement, tsymb)
import Drasil.DocumentLanguage.Definitions (Field
  (DefiningEquation, Description, RefBy, Source), InclUnits(IncludeUnits), 
  Verbosity(Verbose), Fields)
import Drasil.DocumentLanguage.RefHelpers (cite)

import Data.Drasil.Concepts.Computation (computerApp, inParam,
  computerLiteracy, inValue, inQty)
import Data.Drasil.Concepts.Documentation as Doc (analysis, appendix, aspect,
  characteristic, class_, code, condition, constraint, content,
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
  foldlSent, foldlSent_, figureLabel, foldlList, showingCxnBw,
  foldlsC, sOf, followA, ofThe, sIn, isThe, isExpctdToHv, sOr, underConsidertn,
  tAndDWAcc, tAndDOnly, tAndDWSym, andThe)
import Data.Drasil.Software.Products (sciCompS)
import Data.Drasil.Utils (getES, makeTMatrix, makeListRef, itemRefToSent,
  refFromType, enumSimple, enumBullet, prodUCTbl)

import Drasil.GlassBR.Assumptions (assumptionConstants, assumptionDescs,
  gbRefDB, newAssumptions)
import Drasil.GlassBR.Changes (likelyChanges_SRS, unlikelyChanges_SRS)
import Drasil.GlassBR.Concepts (aR, lShareFac, gLassBR, stdOffDist, glaSlab, 
  blastRisk, glass, glaPlane, glassBRProg, ptOfExplsn, acronyms)
import Drasil.GlassBR.DataDefs (dataDefns, gbQDefns, hFromt, strDisFac, nonFL, 
  dimLL, glaTyFac, tolStrDisFac, tolPre, risk)
import Drasil.GlassBR.IMods (iModels, calOfCap, calOfDe, probOfBr)
import Drasil.GlassBR.ModuleDefs (allMods)
import Drasil.GlassBR.References (rbrtsn2012)
import Drasil.GlassBR.Symbols (this_symbols)
import Drasil.GlassBR.TMods (tModels, t1SafetyReq, t2SafetyReq)
import Drasil.GlassBR.Unitals (stressDistFac, aspectR, dimlessLoad, 
  lateralLoad, sflawParamM, char_weight, sD, demand, demandq, 
  aspectRWithEqn, aspectR, lRe, wtntWithEqn, sdWithEqn, prob_br, notSafe, 
  safeMessage, is_safe1, is_safe2, plate_width, plate_len, blast, glassTy, 
  gbInputDataConstraints, explosion, pb_tol, blast, bomb, blastTy, 
  glassGeo, glass_type, nom_thick, sdx, sdy, sdz, tNT, gBRSpecParamVals,
  loadTypes, load, glassTypes, probBreak, termsWithAccDefn, termsWithDefsOnly,
  gbConstants, gbConstrained, gbOutputs, gbInputs, glBreakage, capacity, 
  constant_LoadDF)

import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.SpecificSystemDescription (solChSpecF,
  inDataConstTbl, outDataConstTbl, dataConstraintUncertainty, goalStmtF,
  physSystDesc, termDefnF, probDescF, specSysDesF)
import Drasil.Sections.TraceabilityMandGs (traceGIntro)
import Data.Drasil.Citations (koothoor2013, smithLai2005)
import Data.Drasil.People (spencerSmith, nikitha, mCampidelli)
import Data.Drasil.Phrase(for'')
import Data.Drasil.SI_Units (kilogram, metre, millimetre, newton, pascal, 
  second)

{--}

gbSymbMap :: ChunkDB
gbSymbMap =
  cdb this_symbols (map nw acronyms ++ map nw this_symbols) ([] :: [ConceptChunk])
      (map unitWrapper [metre, second, kilogram] ++ map unitWrapper [pascal, newton])

resourcePath :: String
resourcePath = "../../../datafiles/GlassBR/"

glassBR_srs :: Document
glassBR_srs = mkDoc mkSRS (for'' titleize phrase) glassSystInfo

mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA]) :
 IntroSec (
  IntroProg (startIntro software blstRskInvWGlassSlab gLassBR) (short gLassBR)
   [IPurpose (purpose_intro_p1 document gLassBR glaSlab),
   IScope incScoR endScoR,
   IChar (rdrKnldgbleIn glBreakage blastRisk) undIR appStanddIR,
   IOrgSec intendedReaderIntro dataDefn SRS.dataDefn intendedReaderIntro_end]):
 StkhldrSec (StkhldrProg2 [Client gLassBR (S "a" +:+ phrase company +:+ 
     S "named Entuitive. It is developed by Dr." +:+ (S $ name mCampidelli)), 
   Cstmr gLassBR]) :
 GSDSec (GSDProg2 
   [UsrChars [user_chars_bullets endUser gLassBR secondYear undergradDegree 
    civilEng structuralEng glBreakage blastRisk], SystCons [] []]) :
 ScpOfProjSec (ScpOfProjProg (short gLassBR) (prod_use_case_table) 
           (indiv_prod_use_case (glaSlab) (capacity) (demandq) (probability))) :
 SSDSec (SSDProg [SSDProblem (
   PDProg start gLassBR ending [terms_defs, phys_sys_desc, goals])],
   [ SSDSolChSpec (
      SCSProg
       [ Assumptions
       , TMs ([Label] ++ stdFields) [t1IsSafe]
       , GDs [] [] HideDerivation -- No Gen Defs for GlassBR
       , DDs ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
       , IMs ([Label, Input, Output, InConstraints, OutConstraints] ++ 
               stdFields) [probOfBreak, testIMFromQD] HideDerivation
       ]
      )
   ]
  ) :
  ReqrmntSec (ReqsProg [
    FReqsSub func_reqs_list,
    NonFReqsSub [performance] (gBRpriorityNFReqs)
    (S "This problem is small in size and relatively simple")
    (S "Any reasonable" +:+ phrase implementation +:+.
    (S "will be very quick" `sAnd` S "use minimal storage"))]) :
  LCsSec (LCsProg likely_change_list) :
  UCsSec (UCsProg unlikely_change_list) :
  TraceabilitySec
    (TraceabilityProg traceyMatrices [trace_table1Desc, trace_table2Desc, trace_table3Desc]
    (traceyMatrices ++ trace_intro2 ++ traceyGraphs) []) :
  AuxConstntSec (AuxConsProg gLassBR auxiliaryConstants) :
  Bibliography :
  AppndxSec (AppndxProg [appendix_intro, fig_5, fig_6]) : []
 
stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Source, RefBy]

glassSystInfo :: SystemInformation
glassSystInfo = SI {
  _sys         = glassBRProg,
  _kind        = srs,
  _authors     = [nikitha, spencerSmith],
  _units       = map unitWrapper [metre, second, kilogram] ++ map unitWrapper [pascal, newton],
  _quants      = this_symbols,
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = dataDefns ++ 
                 (map (relToQD gbSymbMap) iModels {-[RelationConcept]-}) ++ 
                 (map (relToQD gbSymbMap) tModels {-[RelationConcept]-}) ++
                  [wtntWithEqn, sdWithEqn],  -- wtntWithEqn is defined in Unitals but only appears
                                             -- in the description of the Calculation of Demand instance model;
                                             -- should this be included as a Data Definition?
                                             -- (same for sdWithEqn)
  _inputs      = map qw gbInputs,
  _outputs     = map qw gbOutputs,
  _defSequence = gbQDefns,
  _constraints = gbConstrained,
  _constants   = gbConstants,
  _sysinfodb   = gbSymbMap,
  _refdb       = gbRefDB
}
  --FIXME: All named ideas, not just acronyms.

testIMFromQD :: InstanceModel
testIMFromQD = imQD gbSymbMap risk EmptyS [] [] "riskFun" --shortname
glassBR_code :: CodeSpec
glassBR_code = codeSpec glassSystInfo allMods


spec_sys_desc, prob_desc, terms_defs, phys_sys_desc, goals, sol_chars_spec :: Section

prod_use_case_table,
  phys_sys_desc_list, sol_chars_spec_intro, data_consts_table1,
  data_consts_table2, trace_table1,
  trace_table2, trace_table3, appendix_intro,
  fig_glassbr, fig_2, fig_3, fig_4, fig_5,
  fig_6 :: Contents

func_reqs_list, trace_intro2 :: [Contents]

--------------------------------------------------------------------------------
terms_defs_bullets :: Contents
terms_defs_bullets = Enumeration $ (Numeric $
  map tAndDOnly termsWithDefsOnly
  ++
  terms_defs_bullets_glTySubSec
  ++
  terms_defs_bullets_loadSubSec
  ++
  map tAndDWAcc termsWithAccDefn
  ++
  [tAndDWSym probBreak prob_br])
   --FIXME: merge? Needs 2 arguments because there is no instance for (SymbolForm ConceptChunk)...

terms_defs_bullets_glTySubSec, terms_defs_bullets_loadSubSec :: [ItemType]

terms_defs_bullets_glTySubSec = [Nested (((titleize glassTy) :+: S ":"))
  (Bullet $ map tAndDWAcc glassTypes)]

terms_defs_bullets_loadSubSec = [Nested (((at_start load) :+: S ":"))
  (Bullet $ map tAndDWAcc (take 2 loadTypes)
  ++
  map tAndDOnly (drop 2 loadTypes))]

--Used in "Goal Statements" Section--
goals_list :: Contents
goals_list = enumSimple 1 (short goalStmt) goals_list_goalStmt1

--Used in "Traceability Matrices and Graphs" Section--
traceyMatrices, traceyGraphs :: [Contents]
traceyMatrices = [trace_table1, trace_table2, trace_table3]
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

func_reqs_req6_pulledList :: [QDefinition]
func_reqs_req6_pulledList = [nonFL, glaTyFac, dimLL, tolPre,
  tolStrDisFac, strDisFac, hFromt]

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
  S "The", phrase prgm `sC` S "herein called", short progName,
  S "aims to predict the", sfwrPredicts, S "using an intuitive",
  phrase interface]

rdrKnldgbleIn :: (NamedIdea n, NamedIdea n1) => n1 -> n -> Sentence
rdrKnldgbleIn undrstd1 undrstd2 = (phrase theory +:+ S "behind" +:+
  phrase undrstd1 `sAnd` phrase undrstd2)

undIR, appStanddIR, incScoR, endScoR :: Sentence
undIR = foldlList [phrase scndYrCalculus, phrase structuralMechanics,
  plural computerApp `sIn` phrase civilEng]
appStanddIR = foldlSent [S "In addition" `sC` plural reviewer,
  S "should be familiar with the applicable", plural standard,
  S "for constructions using glass from",
  sSqBr (S "4-6" {-astm_LR2009, astm_C1036, astm_C1048-}) `sIn`
  (makeRef (SRS.reference SRS.missingP []))]
incScoR = foldl (+:+) EmptyS [S "getting all", plural inParam,
  S "related to the", phrase glaSlab `sAnd` S "also the", plural parameter,
  S "related to", phrase blastTy]
endScoR = foldl (+:+) EmptyS [S "use the", plural datum `sAnd`
  S "predict whether the", phrase glaSlab, S "is safe to use" `sOr`
  S "not"]

{--Purpose of Document--}

purpose_intro_p1 :: NamedChunk -> CI -> NamedChunk -> Sentence
purpose_intro_p1 typeOf progName gvnVar = foldlSent [S "The main", phrase purpose,
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

intendedReaderIntro_end, intendedReaderIntro :: Sentence
intendedReaderIntro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the", phrase template, S "for an", short srs,
  S "for", phrase sciCompS, S "proposed by" +:+ cite gbRefDB koothoor2013
  `sAnd` cite gbRefDB smithLai2005 `sC` S "with some", 
  plural aspect, S "taken from Volere", phrase template,
  S "16", cite gbRefDB rbrtsn2012]

intendedReaderIntro_end = foldl (+:+) EmptyS [(at_start' $ the dataDefn),
  S "are used to support", (plural definition `ofThe` S "different"),
  plural model]

{--STAKEHOLDERS--}

{--The Client--}
{--The Customer--}

{--GENERAL SYSTEM DESCRIPTION--}

{--User Characteristics--}

user_chars_bullets :: (NamedIdea n1, NamedIdea n, NamedIdea n2, NamedIdea n3,
  NamedIdea n4, NamedIdea n5, Idea c, NamedIdea n6) =>
  n6 -> c -> n5 -> n4 -> n3 -> n2 -> n1 -> n -> Contents
user_chars_bullets intendedIndvdl progName yr degreeType prog1 prog2 undrstd1 undrstd2
  = enumBullet [foldlSent [(phrase intendedIndvdl `sOf` short progName)
  `isExpctdToHv` S "completed at least", (S "equivalent" `ofThe` (phrase yr)),
  S "of an", phrase degreeType `sIn` phrase prog1 `sOr` phrase prog2],
  (phrase intendedIndvdl `isExpctdToHv` S "an understanding of" +:+.
  rdrKnldgbleIn (undrstd1) (undrstd2)), foldlSent [phrase intendedIndvdl
  `isExpctdToHv` S "basic", phrase computerLiteracy, S "to handle the",
  phrase software]]

{--System Constraints--}

{--SCOPE OF THE PROJECT-}

{--Product Use Case Table--}

prod_use_case_table = prodUCTbl [prod_use_case_table_UC1, prod_use_case_table_UC2]

prod_use_case_table_UC1, prod_use_case_table_UC2 :: [Sentence]

prod_use_case_table_UC1 = [titleize user, titleize' characteristic +:+ S "of the"
  +:+ phrase glaSlab `sAnd` S "of the" +:+. phrase blast +:+ S "Details in"
  +:+ makeRef (SRS.indPRCase SRS.missingP [])]

prod_use_case_table_UC2 = [short gLassBR, S "Whether" `sOr` S "not the" +:+
  phrase glaSlab +:+ S "is safe for the" +:+ S "calculated" +:+ phrase load
  `sAnd` S "supporting calculated" +:+ plural value]

{--Individual Product Use Case--}

indiv_prod_use_case :: NamedChunk -> ConceptChunk -> ConceptChunk -> ConceptChunk ->
  Contents
indiv_prod_use_case mainObj compare1 compare2 factorOfComparison =
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
  sParen (getES prob_br), S "is less than the tolerable",
  phrase factorOfComparison, sParen (getES pb_tol),
  S "which is obtained from the", phrase user, S "as an" +:+. phrase input_,
  S "If both", plural condition, S "return true then it's shown that the",
  phrase mainObj, S "is safe to use" `sC`
  S "else if both return false then the", phrase mainObj +:+.
  S "is considered unsafe", S "All the supporting calculated", plural value,
  S "are also displayed as", phrase output_]

{--SPECIFIC SYSTEM DESCRIPTION--}

spec_sys_desc = specSysDesF (S "and" +:+ plural definition) [prob_desc, sol_chars_spec]

{--PROBLEM DESCRIPTION--}

start, ending :: Sentence
start = foldlSent [S "A", phrase system,
  S "is needed to efficiently" `sAnd` S "correctly predict the",
  phrase blastRisk +:+ S "involved with the glass"]
ending = foldl (+:+) EmptyS [S "interpret the", plural input_,
  S "to give out the", plural output_,
  S "which predicts whether the", phrase glaSlab,
  S "can withstand the", phrase blast, S "under the",
  plural condition]

prob_desc = probDescF start gLassBR ending [terms_defs, phys_sys_desc, goals]

{--Terminology and Definitions--}

terms_defs = termDefnF (Just (S "All" `sOf` S "the" +:+ plural term_ +:+
  S "are extracted from" +:+ (sSqBrNum 4 {-astm_LR2009-}) `sIn`
  (makeRef (SRS.reference SRS.missingP [])))) [terms_defs_bullets]

{--Physical System Description--}

phys_sys_desc = physSystDesc (short gLassBR) (fig_glassbr) [phys_sys_desc_list, fig_glassbr]

fig_glassbr = fig (at_start $ the physicalSystem) (resourcePath ++ "physicalsystimage.png")
  "physSystImage"

phys_sys_desc_list = enumSimple 1 (short physSyst) phys_sys_desc_list_physys

--"Dead" knowledge?
phys_sys_desc_list_physys :: [Sentence]
phys_sys_desc_list_physys1 :: Sentence
phys_sys_desc_list_physys2 :: NamedIdea n => n -> Sentence

phys_sys_desc_list_physys = [phys_sys_desc_list_physys1, phys_sys_desc_list_physys2 (ptOfExplsn)]

phys_sys_desc_list_physys1 = at_start glaSlab

phys_sys_desc_list_physys2 imprtntElem = foldlSent [S "The"
  +:+. phrase imprtntElem, S "Where the", phrase bomb `sC`
  S "or", (blast ^. defn) `sC` S "is located. The", phrase sD
  `isThe` phrase distance, S "between the", phrase imprtntElem `sAnd`
  S "the glass"]

{--Goal Statements--}

goals = goalStmtF [foldlList [plural dimension `ofThe` phrase glaPlane,
  phrase glassTy, plural characteristic `ofThe` phrase explosion,
  S "the" +:+ phrase pb_tol]] [goals_list]

goals_list_goalStmt1 :: [Sentence]
goals_list_goalStmt1 = [foldlSent [S "Analyze" `sAnd` S "predict whether",
  S "the", phrase glaSlab, S "under consideration will be able to withstand",
  S "the", phrase explosion `sOf` S "a certain", phrase degree_',
  S "which is calculated based on", phrase userInput]]

{--SOLUTION CHARACTERISTICS SPECIFICATION--}

sol_chars_spec = solChSpecF gLassBR (prob_desc, (SRS.likeChg SRS.missingP []), (SRS.unlikeChg SRS.missingP [])) EmptyS
 (EmptyS, dataConstraintUncertainty, end)
 (assumps_list, map reldefn tModels, [], map datadefn dataDefns,
  map reldefn iModels,
  [data_consts_table1, data_consts_table2]) []
  where
    end = foldlSent [(makeRef (SRS.valsOfAuxCons SRS.missingP [])),
      S "gives", (plural value `ofThe` S "specification"),
      plural parameter, S "used in", (makeRef data_consts_table1)]
      +:+ data_consts_intro2

sol_chars_spec_intro = foldlSP [S "This", phrase section_, S "explains all the",
  plural assumption, S "considered" `sAnd` S "the", plural thModel,
  S "which are supported by the", plural dataDefn]

{--Assumptions--}

assumps_list :: [Contents]
assumps_list = assumpList newAssumptions

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

data_consts_table1 = inDataConstTbl gbInputDataConstraints
data_consts_table2 = outDataConstTbl [prob_br]

data_consts_intro2 :: Sentence
data_consts_intro2 = foldlSent [makeRef data_consts_table2, S "shows the",
  plural constraint, S "that must be satisfied by the", phrase output_]

{--REQUIREMENTS--}

{--Functional Requirements--}

func_reqs_list = (func_reqs_listOfReqs) ++ func_reqs_req6 ++ [func_reqs_req1Table]

func_reqs_req1, func_reqs_req2, func_reqs_req3, func_reqs_req4, func_reqs_req5 :: Contents
req1Desc, req2Desc, req3Desc, req4Desc :: Sentence
req5Desc :: NamedChunk -> Sentence
func_reqs_req6 :: [Contents] --FIXME: Issue #327

func_reqs_listOfReqs :: [Contents]
func_reqs_listOfReqs = [func_reqs_req1, func_reqs_req2, func_reqs_req3, func_reqs_req4, func_reqs_req5]

func_reqs_req1 = mkRequirement "func_reqs_req1" req1Desc "Input-Glass-Props"
func_reqs_req2 = mkRequirement "func_reqs_req2" req2Desc "System-Set-Values-Following-Assumptions"
func_reqs_req3 = mkRequirement "func_reqs_req3" req3Desc "Check-Input-with-Data_Constraints"
func_reqs_req4 = mkRequirement "func_reqs_req4" req4Desc "Output-Values-and-Known-Quantities"
func_reqs_req5 = mkRequirement "func_reqs_req5" (req5Desc (output_)) "Check-Glass-Safety"

req1Desc = foldlSent [at_start input_, S "the", plural quantity, S "from",
  makeRef func_reqs_req1Table `sC` S "which define the", phrase glass,
  plural dimension `sC` (glassTy ^. defn) `sC` S "tolerable",
  phrase probability `sOf` phrase failure, S "and",
  (plural characteristic `ofThe` phrase blast), S "Note:",
  getES plate_len `sAnd` getES plate_width,
  S "will be input in terms of", plural millimetre `sAnd`
  S "will be converted to the equivalent value in", plural metre]

func_reqs_req1Table :: Contents
func_reqs_req1Table = Table
  [at_start symbol_, at_start description, S "Units"]
  (mkTable
  [getES,
   at_start, unit'2Contents] requiredInputs)
  (S "Required Inputs following R1") True "R1ReqInputs"

req2Desc = foldlSent [S "The", phrase system,
  S "shall set the known", plural value +: S "as follows",
  foldlList [(foldlsC (map getES (take 4 assumptionConstants)) `followA` 4),
  ((getES constant_LoadDF) `followA` 8), (short lShareFac `followA` 5),
  (getES hFromt) +:+ sParen (S "from" +:+ (makeRef hFromt)), 
  (getES glaTyFac) +:+ sParen (S "from" +:+ (makeRef glaTyFac))]]

--ItemType
{-func_reqs_req2 = (Nested (S "The" +:+ phrase system +:+
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

req5Desc cmd = foldlSent_ [S "If", (getES is_safe1) `sAnd` (getES is_safe2),
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

func_reqs_req6 = [(Enumeration $ Simple $ [(acroR 6, Nested (titleize output_ +:+
  S "the following" +: plural quantity)
  (Bullet $
    map (\(a, d) -> Flat $ (at_start a) +:+ sParen (getES a) +:+
    sParen (makeRef (reldefn d))) (zip testing testing1)
    ++
    map (\d -> Flat $ (at_start d) +:+ sParen (getES d) +:+
    sParen (makeRef (datadefn d))) func_reqs_req6_pulledList
    ++
    [Flat $ (titleize aspectR) +:+ sParen (getES aspectR) +:+
    E (aspectRWithEqn^.equat)]
    ))])]

{--Nonfunctional Requirements--}

{--LIKELY CHANGES--}

likely_change_list :: [Contents]
likely_change_list = likelyChanges_SRS 

{--UNLIKELY CHANGES--}

unlikely_change_list :: [Contents]
unlikely_change_list = unlikelyChanges_SRS 

{--TRACEABLITY MATRICES AND GRAPHS--}

trace_table1Desc :: Sentence
trace_table1Desc = foldlList (map plural (take 3 solChSpecSubsections)) +:+.
  S "with each other"

trace_table2Desc :: Sentence
trace_table2Desc = plural requirement +:+ S "on" +:+. foldlList
  (map plural solChSpecSubsections)

trace_table3Desc :: Sentence
trace_table3Desc = foldlsC (map plural (take 3 solChSpecSubsections)) `sC`
  plural likelyChg `sAnd` plural requirement +:+ S "on the" +:+
  plural assumption

trace_theorys, trace_instaModel, trace_dataDef, trace_data, trace_funcReq, trace_assump,
  trace_likelyChg :: [String]

trace_theorysRef, trace_instaModelRef, trace_dataDefRef, trace_dataRef, trace_funcReqRef,
  trace_assumpRef, trace_likelyChgRef :: [Sentence]

trace_theorys = ["T1", "T2"]
trace_theorysRef = map (refFromType Theory) tModels

trace_instaModel = ["IM1", "IM2", "IM3"]
trace_instaModelRef = map (refFromType Theory) iModels

trace_dataDef =  ["DD1", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8"]
trace_dataDefRef = map (refFromType Data) dataDefns

trace_data  = ["Data Constraints"]
trace_dataRef = [makeRef (SRS.datCon SRS.missingP [])]

trace_funcReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
trace_funcReqRef = makeListRef trace_funcReq (SRS.funcReq SRS.missingP [])

trace_assump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"]
trace_assumpRef = makeListRef trace_assump (SRS.assumpt SRS.missingP [])

trace_likelyChg = ["LC1", "LC2", "LC3", "LC4", "LC5"]
trace_likelyChgRef = makeListRef trace_likelyChg (SRS.likeChg SRS.missingP [])

trace_row_t1 :: [String]
trace_row_t1 = trace_theorys ++ trace_instaModel ++ trace_dataDef

-- The headers for the first row, and column
trace_row_header_t1 :: [Sentence]
trace_row_header_t1 = zipWith itemRefToSent trace_row_t1 (trace_theorysRef ++
  trace_instaModelRef ++ trace_dataDefRef)

-- list of columns and their rows for traceability matrix
trace_columns_t1 :: [[String]]
trace_columns_t1 = [trace_t1_T1, trace_t1_T2, trace_t1_IM1, trace_t1_IM2, trace_t1_IM3,
  trace_t1_DD1, trace_t1_DD2, trace_t1_DD3, trace_t1_DD4, trace_t1_DD5, trace_t1_DD6, trace_t1_DD7,
  trace_t1_DD8]

trace_t1_T1, trace_t1_T2, trace_t1_IM1, trace_t1_IM2, trace_t1_IM3, trace_t1_DD1, trace_t1_DD2,
  trace_t1_DD3, trace_t1_DD4, trace_t1_DD5, trace_t1_DD6, trace_t1_DD7, trace_t1_DD8 :: [String]

-- list of each item that "this" item requires for traceability matrix
trace_t1_T1  = ["T2", "IM1"]
trace_t1_T2  = ["T1", "IM2", "IM3"]
trace_t1_IM1 = ["DD1", "DD2", "DD3"]
trace_t1_IM2 = ["DD4", "DD5"]
trace_t1_IM3 = []
trace_t1_DD1 = []
trace_t1_DD2 = []
trace_t1_DD3 = ["DD6"]
trace_t1_DD4 = ["DD2", "DD6"]
trace_t1_DD5 = []
trace_t1_DD6 = ["IM3", "DD2", "DD5"]
trace_t1_DD7 = ["DD8"]
trace_t1_DD8 = ["DD2"]

trace_table1 = Table (EmptyS:trace_row_header_t1)
  (makeTMatrix trace_row_header_t1 trace_columns_t1 trace_row_t1)
  (showingCxnBw (traceyMatrix)
  (titleize' item +:+ S "of Different" +:+ titleize' section_)) True "TraceyItemSecs"

--

trace_row_t2 :: [String]
trace_row_t2 = trace_row_t1 ++ trace_data ++ trace_funcReq

trace_row_header_t2, trace_col_header_t2 :: [Sentence]
trace_row_header_t2 = trace_row_header_t1 ++
  (zipWith itemRefToSent (trace_data ++ trace_funcReq) (trace_dataRef ++ trace_funcReqRef))

trace_col_header_t2 = zipWith (\x y -> (S x) +:+ (sParen (S "in" +:+ y)))
  trace_funcReq trace_funcReqRef

trace_t2_r1, trace_t2_r2, trace_t2_r3, trace_t2_r4, trace_t2_r5,
  trace_t2_r6 :: [String]

trace_columns_t2 :: [[String]]
trace_columns_t2 = [trace_t2_r1, trace_t2_r2, trace_t2_r3, trace_t2_r4, trace_t2_r5, trace_t2_r6]
trace_t2_r1 = []
trace_t2_r2 = []
trace_t2_r3 = ["Data Constraints"]
trace_t2_r4 = ["R1", "R2"]
trace_t2_r5 = ["T1", "T2"]
trace_t2_r6 = ["IM1", "IM2", "IM3", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8"]

trace_table2 = Table (EmptyS:trace_row_header_t2)
  (makeTMatrix trace_col_header_t2 trace_columns_t2 trace_row_t2)
  (showingCxnBw (traceyMatrix) (titleize' requirement `sAnd` S "Other" +:+
  titleize' item)) True "TraceyReqsItems"

--

trace_row_t3 :: [String]
trace_row_t3 = trace_assump

trace_row_header_t3, trace_col_header_t3 :: [Sentence]
trace_row_header_t3 = zipWith itemRefToSent trace_assump trace_assumpRef

trace_col_header_t3 = trace_row_header_t1 ++ (zipWith itemRefToSent
  (trace_likelyChg ++ trace_funcReq) (trace_likelyChgRef ++ trace_funcReqRef))

trace_columns_t3 :: [[String]]
trace_columns_t3 = [trace_t3_T1, trace_t3_T2, trace_t3_IM1, trace_t3_IM2, trace_t3_IM3, trace_t3_DD1,
  trace_t3_DD2, trace_t3_DD3, trace_t3_DD4, trace_t3_DD5, trace_t3_DD6, trace_t3_DD7, trace_t3_DD8,
  trace_t3_lc1, trace_t3_lc2, trace_t3_lc3, trace_t3_lc4, trace_t3_lc5, trace_t3_r1, trace_t3_r2,
  trace_t3_r3, trace_t3_r4, trace_t3_r5, trace_t3_r6]

trace_t3_T1, trace_t3_T2, trace_t3_IM1, trace_t3_IM2, trace_t3_IM3, trace_t3_DD1, trace_t3_DD2,
  trace_t3_DD3, trace_t3_DD4, trace_t3_DD5, trace_t3_DD6, trace_t3_DD7, trace_t3_DD8,
  trace_t3_lc1, trace_t3_lc2, trace_t3_lc3, trace_t3_lc4, trace_t3_lc5, trace_t3_r1,
  trace_t3_r2, trace_t3_r3, trace_t3_r4, trace_t3_r5, trace_t3_r6 :: [String]

-- list of each item that "this" item requires for traceability matrix
trace_t3_T1  = []
trace_t3_T2  = []
trace_t3_IM1 = ["A4", "A6", "A7"]
trace_t3_IM2 = ["A1", "A2", "A5"]
trace_t3_IM3 = []
trace_t3_DD1 = []
trace_t3_DD2 = []
trace_t3_DD3 = []
trace_t3_DD4 = ["A4"]
trace_t3_DD5 = []
trace_t3_DD6 = ["A5"]
trace_t3_DD7 = []
trace_t3_DD8 = ["A4"]
trace_t3_lc1 = ["A3"]
trace_t3_lc2 = ["A4", "A8"]
trace_t3_lc3 = ["A5"]
trace_t3_lc4 = ["A6"]
trace_t3_lc5 = ["A7"]
trace_t3_r1  = []
trace_t3_r2  = ["A4", "A5", "A8"]
trace_t3_r3  = []
trace_t3_r4  = []
trace_t3_r5  = []
trace_t3_r6  = []

trace_table3 = Table (EmptyS:trace_row_header_t3)
  (makeTMatrix trace_col_header_t3 trace_columns_t3 trace_row_t3)
  (showingCxnBw (traceyMatrix) (titleize' assumption `sAnd` S "Other"
  +:+ titleize' item)) True "TraceyAssumpsOthers"

--

trace_intro2 = traceGIntro traceyGraphs
  [(foldlList (map plural (take 3 solChSpecSubsections)) +:+.
  S "on each other"), (plural requirement +:+ S "on" +:+. foldlList
  (map plural solChSpecSubsections)),
  (foldlList ((map plural (take 3 solChSpecSubsections))++
  [plural requirement, plural likelyChg +:+ S "on" +:+ plural assumption]))]

fig_2 = figureLabel 2 (traceyMatrix)
  (titleize' item +:+ S "of Different" +:+ titleize' section_)
  (resourcePath ++ "Trace.png") "TraceyItemSecs"

fig_3 = figureLabel 3 (traceyMatrix)
  (titleize' requirement `sAnd` S "Other" +:+ titleize' item)
  (resourcePath ++ "RTrace.png") "TraceyReqsItems"

fig_4 = figureLabel 4 (traceyMatrix)
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
  sParen (getES demand) `sVersus` at_start sD +:+ sParen (getAcc stdOffDist)
  `sVersus` at_start char_weight +:+ sParen (getES sflawParamM))
  (resourcePath ++ "ASTM_F2248-09.png") "demandVSsod"

fig_6 = fig (titleize figure +: S "6" +:+ S "Non dimensional" +:+
  phrase lateralLoad +:+ sParen (getES dimlessLoad)
  `sVersus` titleize aspectR +:+ sParen (getAcc aR)
  `sVersus` at_start stressDistFac +:+ sParen (getES stressDistFac))
  (resourcePath ++ "ASTM_F2248-09_BeasonEtAl.png") "dimlessloadVSaspect"

blstRskInvWGlassSlab :: Sentence
blstRskInvWGlassSlab = phrase blastRisk +:+ S "involved with the" +:+
  phrase glaSlab
