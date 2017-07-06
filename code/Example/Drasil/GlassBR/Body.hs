module Drasil.GlassBR.Body where
import Control.Lens ((^.))

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Authors
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Education
import Data.Drasil.Software.Products
import Data.Drasil.Concepts.Computation
import Data.Drasil.Concepts.Thermodynamics (degree_')
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Math (graph, calculation, equation,
  surface, probability, parameter)
import Data.Drasil.Concepts.Thermodynamics (heat)
import Prelude hiding (id)
import Data.Drasil.Utils
import Data.Drasil.SentenceStructures (foldlSent, foldlList, ofThe, isThe,
  showingCxnBw, figureLabel, foldlSP, sAnd, foldlsC, tAndDWAcc, tAndDWSym,
  tAndDOnly, sVersus, followA)

import Drasil.Template.MG
import Drasil.Template.DD

import qualified Drasil.SRS as SRS
import           Drasil.Sections.ReferenceMaterial

import Drasil.GlassBR.Unitals
import Drasil.GlassBR.Concepts
import Drasil.GlassBR.Changes
import Drasil.GlassBR.Modules
import Drasil.GlassBR.Reqs
import Drasil.GlassBR.TMods
import Drasil.GlassBR.IMods
import Drasil.GlassBR.DataDefs

import Drasil.DocumentLanguage
import Drasil.Sections.TraceabilityMandGs
import Drasil.Sections.Stakeholders
import Drasil.Sections.ScopeOfTheProject
import Drasil.Sections.Requirements
import Drasil.Sections.GeneralSystDesc
import Drasil.Sections.SpecificSystemDescription
import Drasil.Sections.AuxiliaryConstants

this_si :: [UnitDefn]
this_si = map UU [metre, second] ++ map UU [pascal, newton]

s3, s4, s5,
  s6, s6_1, s6_1_1, s6_1_2, s6_1_3, s6_2, 
  s7, s7_1, s7_2, s8, s9, s10, s11, s12 :: Section

s5_1_table, s5_2_bullets, 
  s6_1_2_list, s6_2_intro, s6_2_5_table1, 
  s6_2_5_table2, s7_2_intro, s9_table1,
  s9_table2, s9_table3, s11_list, s12_intro,
  fig_glassbr, fig_2, fig_3, fig_4, fig_5,
  fig_6 :: Contents

s6_2_1_list, s7_1_list, s9_intro2 :: [Contents]

mg_authors, s2_3_intro_end, s2_3_intro :: Sentence
mg_authors = manyNames [spencerSmith, thulasi]

authors :: People
authors = [nikitha, spencerSmith]

glassBR_srs' :: Document
glassBR_srs' = mkDoc' mkSRS (for'' titleize phrase) glassSystInfo

mkSRS :: DocDesc 
mkSRS = [RefSec (RefProg intro
  [TUnits,
  tsymb [TSPurpose, SymbOrder],
  TAandA])] ++
  [IntroSec 
  (IntroProg (startIntro (blstRskInvWGlassSlab) (gLassBR)) (short gLassBR)
     [IPurpose (s2_1_intro_p1 (document) (gLassBR) (glaSlab)),
     IScope incScoR endScoR,
     IChar knowIR undIR appStanddIR,
     IOrgSec s2_3_intro dataDefn (SRS.dataDefn SRS.missingP []) s2_3_intro_end])] ++
  map Verbatim [s3, s4, s5 {-, s6-}] ++
  [SSDSec (SSDVerb s6)] {-(SSDProg [SSDProblem, SSDSolChSpec])-} ++
  map Verbatim [s7, s8, s9, s10, s11, s12]
  
glassSystInfo :: SystemInformation
glassSystInfo = SI glassBRProg srs authors this_si this_symbols
  ([] :: [CQSWrapper])
  (acronyms)
  (dataDefns)
  (map qs gbInputs)
  (map qs gbOutputs)
  (gbQDefns :: [Block QDefinition])
  gbConstrained
  --FIXME: All named ideas, not just acronyms.
  
glassChoices :: Choices
glassChoices = Choices {
  impType = Program,
  logFile = "log.txt",
  logging = LogAll,
  onSfwrConstraint = Exception,
  onPhysConstraint = Warning,
  inputStructure = Loose
}  
  
glassBR_code :: CodeSpec
glassBR_code = codeSpec' glassSystInfo glassChoices

mgBod :: [Section]
(mgBod, _) = makeDD likelyChanges unlikelyChanges reqs modules

glassBR_mg :: Document
glassBR_mg = mgDoc'' glassBRProg (for'' titleize phrase) mg_authors mgBod

--------------------------------------------------------------------------------
--Used in "Terms And Definitions" Section--
termsWithDefsOnly, termsWithAccDefn, glassTypes, loadTypes :: [ConceptChunk]

termsWithDefsOnly = [glBreakage, lateral, lite, specA, blastResisGla,
  eqTNTChar]
termsWithAccDefn  = [sD, loadShareFac, glTyFac, aspectRatio]
glassTypes = [annealedGl, fTemperedGl, hStrengthGl]
loadTypes = [loadResis, nonFactoredL,glassWL, shortDurLoad,
  specDeLoad, longDurLoad] 

s6_1_1_bullets :: Contents
s6_1_1_bullets = Enumeration $ (Number $
  map tAndDOnly termsWithDefsOnly
  ++
  s6_1_1_bullets_glTySubSec
  ++
  s6_1_1_bullets_loadSubSec
  ++
  map tAndDWAcc termsWithAccDefn
  ++
  [tAndDWSym (probBreak) (prob_br)])

s6_1_1_bullets_glTySubSec, s6_1_1_bullets_loadSubSec :: [ItemType]

s6_1_1_bullets_glTySubSec = [Nested (((titleize glassTy) :+: S ":"))
  (Bullet $ map tAndDWAcc glassTypes)]

s6_1_1_bullets_loadSubSec = [Nested (((at_start load) :+: S ":"))
  (Bullet $ map tAndDWAcc (take 2 loadTypes)
  ++ 
  map tAndDOnly (drop 2 loadTypes))]

--Used in "Likely Changes" Section--
s8_list :: Contents
s8_list = enumSimple 1 (short likelyChg) s8_likelychg_list

--Used in "Goal Statements" Section--
s6_1_3_list :: Contents
s6_1_3_list = enumSimple 1 (short goalStmt) s6_1_3_list_goalStmt1

--Used in "Traceability Matrices and Graphs" Section--
traceyMatrices, traceyGraphs :: [Contents]

traceyMatrices = [s9_table1, s9_table2, s9_table3]
traceyGraphs = [fig_2, fig_3, fig_4]

solChSpecSubsections :: [CI]
solChSpecSubsections = [thModel, inModel, dataDefn, dataConst]

--Used in "Values of Auxiliary Constants" Section--
assumption4_constants :: [QDefinition]
assumption4_constants = [constant_M, constant_K, constant_ModElas,
  constant_LoadDur]

auxiliaryConstants :: [QDefinition]
auxiliaryConstants = assumption4_constants ++ gBRSpecParamVals

--Used in "Functional Requirements" Section--
requiredInputs :: [QSWrapper]
requiredInputs = (map qs [plate_len, plate_width, char_weight])
  ++ (map qs [pb_tol, tNT]) ++ (map qs [sdx, sdy, sdz])
  ++ (map qs [glass_type, nom_thick])

s7_1_req6_pulledList :: [QDefinition]
s7_1_req6_pulledList = [loadDF, nonFL, glaTyFac, dimLL, tolPre,
  tolStrDisFac, strDisFac, hFromt]

--Used in "Non-Functional Requirements" Section--
gBRpriorityNFReqs :: [ConceptChunk]
gBRpriorityNFReqs = [correctness, verifiability, understandability, 
  reusability, maintainability, portability]

--------------------------------------------------------------------------------

--ISSUE #342--

underConsidertn :: ConceptChunk -> Sentence
underConsidertn chunk = S "The" +:+ (phrase chunk) +:+ 
  S "under consideration is" +:+. (chunk ^. defn) 

blstRskInvWGlassSlab :: Sentence
blstRskInvWGlassSlab = phrase blastRisk +:+ S "involved with the" +:+ 
  phrase glaSlab

isExpctdToHv :: Sentence -> Sentence -> Sentence
a `isExpctdToHv` b = S "The" +:+ a +:+ S "is expected to have" +:+ b

s4_1_bullets :: (NamedIdea n, NamedIdea n1, NamedIdea n2, NamedIdea n3,
  NamedIdea n4, NamedIdea n5, NamedIdea c , NamedIdea n6) => n6 -> c ->
  n5 -> n4 -> n3 -> n2 -> n1 -> n -> Contents
--FIXME: better way to do this? ; is this really necessary (i.e. too many things being passed in)

ptOfExplsn:: NamedChunk
ptOfExplsn       = npnc "ptOfExplsn"       (cn' "point of explosion")

--------------

{--INTRODUCTION--}

startIntro :: Sentence -> CI -> Sentence
startIntro sfwrPredicts progName = foldlSent [
  at_start software, S "is helpful to efficiently and correctly predict the"
  +:+. sfwrPredicts, underConsidertn blast, S "The", phrase software `sC`
  S "herein called", short progName, S "aims to predict the", sfwrPredicts, 
  S "using an intuitive interface"]

knowIR, undIR, appStanddIR, incScoR, endScoR :: Sentence
knowIR = (phrase theory +:+ S "behind" +:+ phrase glBreakage `sAnd`
  phrase blastRisk)
undIR = (foldlList [phrase scndYrCalculus, phrase structuralMechanics, 
  S "computer applications in" +:+ phrase civilEng])
appStanddIR = (S "In addition, reviewers should be familiar with the" +:+
  S "applicable standards for constructions using glass from" +:+
  sSqBr (S "4-6") +:+ S "in" +:+. (makeRef s11))
incScoR = foldl (+:+) EmptyS [S "getting all", plural inParam, 
  S "related to the", phrase glaSlab `sAnd` S "also the", plural parameter, 
  S "related to", phrase blastTy]
endScoR = foldl (+:+) EmptyS [S "use the", plural datum `sAnd` 
  S "predict whether the", phrase glaSlab, S "is safe to use or not"]

{--Purpose of Document--}

s2_1_intro_p1 :: NamedChunk -> CI -> NamedChunk -> Sentence
s2_1_intro_p1 typeOf progName gvnVar = foldlSent [S "The main", phrase purpose,
  S "of this", phrase typeOf, S "is to predict whether a given", phrase gvnVar,
  S "is likely to" +:+. predxnGoal, S "The", plural goal `sAnd` plural thModel,
  S "used in the", short progName, S "code are provided" `sC` 
  S "with an emphasis on explicitly identifying", (plural assumption) `sAnd` 
  S "unambiguous" +:+. plural definition, S "This", phrase typeOf, 
  S "is intended to be used as a", phrase reference, S "to provide all", 
  phrase information, S "necessary to understand and verify the" +:+. 
  phrase analysis, S "The", short srs, S "is abstract because the",
  plural content, S "say what", phrase problem, 
  S "is being solved, but not how to solve it"] --General?
  where predxnGoal = S "resist a specified" +:+ phrase blast

{--Scope of Requirements--}

{--Organization of Document--}

s2_3_intro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the", phrase template, S "for an", short srs,
  S "for", phrase sciCompS, S "proposed by" +:+ sSqBr (S "1") 
  `sAnd` sSqBr (S "2"), sParen (S "in" +:+ (makeRef s11)) `sC`
  S "with some aspects taken from Volere", phrase template, S "16", 
  sSqBr (S "3")]
  
s2_3_intro_end = foldl (+:+) EmptyS [(at_start' $ the dataDefn), 
  S "are used to support", (plural definition `ofThe` S "different"), 
  plural model]

{--STAKEHOLDERS--}

s3 = stakehldrGeneral (gLassBR) 
  (S "Entuitive. It is developed by Dr. Manuel Campidelli")
--FIXME: Turn "People -> Sentence"? (so knowledge can be easily pulled out...)

{--The Client--}
{--The Customer--}

{--GENERAL SYSTEM DESCRIPTION--}

s4 = genSysF [] (s4_1_bullets (endUser) (gLassBR) (secondYear) (undergradDegree)
  (civilEng) (structuralEng) (glBreakage) (blastRisk)) [] []

{--User Characteristics--}
s4_1_bullets intendedIndvdl progName yr degreeType prog1 prog2 undrstd1 undrstd2
  = enumBullet [((phrase intendedIndvdl +:+ S "of" +:+ short progName) 
  `isExpctdToHv` S "completed at least" +:+ (S "equivalent" `ofThe` (phrase yr))
  +:+ S "of an" +:+ phrase degreeType +:+ S "in" +:+ phrase prog1 +:+ S "or"
  +:+. phrase prog2), 
  (phrase intendedIndvdl `isExpctdToHv` S "an understanding of" +:+ phrase theory
  +:+ S "behind" +:+. (phrase undrstd1 `sAnd` phrase undrstd2)),
  (phrase intendedIndvdl `isExpctdToHv` S "basic" +:+ phrase computerLiteracy
  +:+ S "to handle the" +:+. phrase software)]

--

{--System Constraints--}

{--SCOPE OF THE PROJECT-}

--Awaiting Closure of Issue #257
s5 = scopeOfTheProjF (short gLassBR) (s5_1_table) (s5_2_bullets)

{--Product Use Case Table--}

s5_1_table = prodUCTbl [s5_1_table_UC1, s5_1_table_UC2]

s5_1_table_UC1, s5_1_table_UC2 :: [Sentence]

s5_1_table_UC1 = [S "1", titleize' input_, titleize user,
  titleize' characteristic +:+ S "of the" +:+ phrase glaSlab `sAnd` S "of the"
  +:+. phrase blast +:+ S "Details in" +:+
  (makeRef (SRS.indPRCase SRS.missingP []))]

s5_1_table_UC2 = [S "2", titleize output_, short gLassBR, S "Whether or not the" 
  +:+ phrase glaSlab +:+ S "is safe for the calculated" +:+ phrase load
  `sAnd` S "supporting calculated" +:+ plural value]

{--Individual Product Use Cases--}

s5_2_bullets = enumBullet [s5_2_bt_sent1, s5_2_bt_sent2]

s5_2_bt_sent1 :: Sentence
s5_2_bt_sent1 = foldlSent [titleize useCase, S "1 refers to the", phrase user,
  S "providing", phrase input_, S "to", short gLassBR,
  S "for use within the" +:+. phrase analysis, S "There are two", plural class_, 
  S "of" +: plural input_ +:+. (phrase glassGeo `sAnd` phrase blastTy),
  (glassGeo ^. defn), (blastTy ^. defn), S "These",
  plural parameter, S "describe", phrase char_weight `sAnd`
  S "stand off" +:+. phrase blast, S "Another", phrase input_,
  S "the", phrase user, S "gives" `isThe` S "tolerable", phrase value, S "of",
  phrase prob_br]

s5_2_bt_sent2 :: Sentence
s5_2_bt_sent2 = foldlSent [titleize useCase, S "2", short gLassBR,
  plural output_, S "if the", phrase glaSlab, S "will be safe by",
  S "comparing whether", phrase capacity, S "is greater than" +:+. 
  (phrase demandq), (at_start capacity `isThe` (capacity ^. defn))
  `sAnd` (phrase demandq `isThe` phrase requirement) +:+. 
  (S "which" `isThe` (demandq ^. defn)), S "The second", 
  phrase condition, S "is to check whether the calculated", 
  phrase probability, sParen (getS prob_br), 
  S "is less than the tolerable", phrase probability, sParen (getS pb_tol),
  S "which is obtained from the", phrase user, S "as an" +:+. phrase input_,
  S "If both", plural condition, S "return true then it's shown that the",
  phrase glaSlab, S "is safe to use" `sC` S "else if both return false then the", 
  phrase glaSlab +:+. S "is considered unsafe", 
  S "All the supporting calculated", plural value, S "are also displayed as", 
  phrase output_]

{--SPECIFIC SYSTEM DESCRIPTION--}

s6 = specSysDesF (S "and" +:+ plural definition) [s6_1, s6_2]

{--PROBLEM DESCRIPTION--}

s6_1 = probDescF start gLassBR ending [s6_1_1, s6_1_2, s6_1_3]
  where start = foldlSent [S "A", phrase system, 
                S "is needed to efficiently and correctly predict the", 
                phrase blastRisk +:+ S "involved with the glass"]
        ending = foldl (+:+) EmptyS [S "interpret the", plural input_, 
                S "to give out the", plural output_, 
                S "which predicts whether the", phrase glaSlab,
                S "can withstand the", phrase blast, S "under the", 
                plural condition]

{--Terminology and Definitions--}

s6_1_1 = termDefnF (S "All of the terms are extracted from" +:+ 
  sSqBr (S "4") +:+ S "in" +:+ (makeRef s11)) [s6_1_1_bullets]

{--Physical System Description--}

s6_1_2 = physSystDesc (short gLassBR) (fig_glassbr) [s6_1_2_list, fig_glassbr]

fig_glassbr = Figure (at_start $ the physicalSystem) "physicalsystimage.png"
  
s6_1_2_list = enumSimple 1 (short physSyst) s6_1_2_list_physys

--"Dead" knowledge?
s6_1_2_list_physys :: [Sentence]
s6_1_2_list_physys1 :: Sentence
s6_1_2_list_physys2 :: NamedIdea n => n -> Sentence

s6_1_2_list_physys = [s6_1_2_list_physys1, s6_1_2_list_physys2 (ptOfExplsn)]

s6_1_2_list_physys1 = at_start glaSlab

s6_1_2_list_physys2 imprtntElem = foldlSent [S "The"
  +:+. phrase imprtntElem, S "Where the", phrase bomb `sC` 
  S "or", (blast ^. defn) `sC` S "is located. The", phrase sD
  `isThe` S "distance between the", phrase imprtntElem `sAnd`
  S "the glass"]

{--Goal Statements--}

s6_1_3 = goalStmtF [foldlList [S "dimensions" `ofThe`S "glass plane", 
  phrase glassTy, plural characteristic `ofThe` phrase explosion, 
  S "the" +:+ phrase pb_tol]] [s6_1_3_list]

s6_1_3_list_goalStmt1 :: [Sentence]
s6_1_3_list_goalStmt1 = [foldlSent [S "Analyze and predict whether the",
  phrase glaSlab, S "under consideration will be able to withstand the",
  phrase explosion, S "of a certain", phrase degree_',
  S "which is calculated based on", phrase userInput]]

{--SOLUTION CHARACTERISTICS SPECIFICATION--}
s6_2_5_intro2 :: Sentence

s6_2 = solChSpecF gLassBR (s6_1, s8) (EmptyS) 
 (EmptyS, dataConstraintUncertainty, end)
 (s6_2_1_list, s6_2_2_TMods, [], s6_2_4_DDefns, s6_2_3_IMods,
  [s6_2_5_table1, s6_2_5_table2]) []
  where end = foldlSent [(makeRef s10), S "gives",
             (plural value `ofThe` S "specification"), plural parameter,
              S "used in" +:+. (makeRef s6_2_5_table1)] +:+ s6_2_5_intro2

s6_2_intro = foldlSP [S "This", phrase section_, 
  S "explains all the", plural assumption, S "considered and the", 
  plural thModel, S "which are supported by the", plural dataDefn]

{--Assumptions--}

s6_2_1_list = [(enumSimple 1 (short assumption) s6_2_1_listOfAssumptions)]

s6_2_1_listOfAssumptions :: [Sentence]
s6_2_1_listOfAssumptions = assumption1 ++ assumption2 ++ assumption3 ++ 
  (assumption4 (load_dur)) ++ assumption5 ++ assumption6 ++ assumption7 ++ (assumption8 (loadDF))

assumption1 :: [Sentence]
assumption1 = [foldlSent [S "The standard E1300-09a for", 
  phrase calculation, S "applies only to monolithic, laminated, or insulating", 
  S "glass constructions of rectangular shape with continuous", 
  phrase lateral +:+. S "support along one, two, three, or four edges", 
  S "This", phrase practice, S "assumes that (1) the supported glass edges", 
  S "for two, three" `sAnd` S "four-sided support", plural condition,
  S "are simply supported and free to slip in plane; (2) glass supported on",
  S "two sides acts as a simply supported beam and (3) glass supported on", 
  S "one side acts as a cantilever"]]

assumption2 :: [Sentence]
assumption2 = [foldlSent [S "Following", (sSqBr (S "4 (pg. 1)")) `sC`
  S "this", phrase practice, S "does not apply to any form of wired, patterned" `sC`
  S "etched, sandblasted, drilled, notched, or grooved glass with", 
  phrase surface `sAnd` S "edge treatments that alter the glass strength"]]

assumption3 :: [Sentence]
assumption3 = [foldlSent [S "This", phrase system,
  S "only considers the external", phrase explosion, S "scenario for its",
  plural calculation]]

assumption4 :: UnitaryChunk -> [Sentence]
assumption4 mainIdea = [foldlSent [S "The", plural value, S "provided in", makeRef s10,
  S "are assumed for the", phrase mainIdea, sParen (getS mainIdea) `sC` 
  S "and the material properties of", 
  foldlList (map getS (take 3 assumption4_constants))]]

assumption5 :: [Sentence]
assumption5 = [foldlSent [S "Glass under consideration", 
  S "is assumed to be a single" +:+. phrase lite, S "Hence the", 
  phrase value, S "of", short lShareFac, S "is equal to 1 for all", 
  plural calculation, S "in", short gLassBR]]

assumption6 :: [Sentence]
assumption6 = [foldlSent [S "Boundary", plural condition, S "for the", 
  phrase glaSlab, S "is assumed to be 4-sided support for",
  plural calculation]]

assumption7 :: [Sentence]
assumption7 = [foldlSent [S "The response type considered in", short gLassBR,
  S "is flexural"]]

assumption8 :: QDefinition -> [Sentence]
assumption8 mainConcept = [foldlSent [S "With", phrase reference, S "to",
  acroA 4, S "the", phrase value, S "of", phrase mainConcept, 
  sParen (getS mainConcept), S "is a constant in" +:+. short gLassBR,
  S "It is calculated by the" +: phrase equation +:+. 
  E (C mainConcept := equat mainConcept), S "Using this" `sC`
  E ((C mainConcept) := (Dbl 0.27))]]

{--Theoretical Models--}

s6_2_2_TMods :: [Contents]
s6_2_2_TMods = map gbSymbMapT tModels

{--Instance Models--}

s6_2_3_IMods :: [Contents]
s6_2_3_IMods = map gbSymbMapT iModels

{--Data Definitions--}

s6_2_4_DDefns ::[Contents] 
s6_2_4_DDefns = map gbSymbMapD dataDefns

{--Data Constraints--}

{-input and output tables-}

s6_2_5_table1 = inDataConstTbl (gbInputDataConstraints)
s6_2_5_table2 = outDataConstTbl [prob_br]

s6_2_5_intro2 = foldlSent [(makeRef s6_2_5_table2), S "shows the", 
  plural constraint, S "that must be satisfied by the", phrase output_]

{--REQUIREMENTS--}

s7 = reqF [s7_1, s7_2]

{--Functional Requirements--}

s7_1 = SRS.funcReq (s7_1_list) []

s7_1_req6 :: [Contents] --FIXME: Issue #327
s7_1_req1, s7_1_req2, s7_1_req3, s7_1_req4 :: Sentence
s7_1_req5 :: NamedChunk -> Sentence

s7_1_list = [enumSimple 1 (getAcc requirement) (s7_1_listOfReqs)] ++ 
  s7_1_req6 ++ [s7_1_req1Table]

s7_1_listOfReqs :: [Sentence]
s7_1_listOfReqs = [s7_1_req1, s7_1_req2, s7_1_req3, s7_1_req4, s7_1_req5 (output_)]

s7_1_req1 = foldlSent [at_start input_, S "the", plural quantity, S "from",
  makeRef s7_1_req1Table `sC` S "which define the glass dimensions" `sC` 
  (glassTy ^. defn) `sC` S "tolerable", phrase probability,
  S "of failure and", (plural characteristic `ofThe` phrase blast)]

s7_1_req1Table :: Contents
s7_1_req1Table = Table 
  [at_start symbol_, at_start description, S "Units"]
  (mkTable
  [(\ch -> (\(Just t) -> (getS t)) (getSymb ch)),
   (at_start), unit'2Contents] requiredInputs)
  (S "Required Inputs following R1") True

s7_1_req2 = foldlSent [S "The", phrase system,
   S "shall set the known", plural value +: S "as follows",
   foldlList [(foldlsC (map getS assumption4_constants) `followA` 4),
     ((getS loadDF) `followA` 8), 
     (short lShareFac `followA` 5)]]

--ItemType
{-s7_1_req2 = (Nested (S "The" +:+ phrase system +:+
   S "shall set the known" +:+ plural value +: S "as follows")
    (Bullet $ map Flat
     [foldlsC (map getS assumption4_constants) `followA` 4, 
     (getS loadDF) `followA` 8, 
     short lShareFac `followA` 5]))
-}
--FIXME:should constants, LDF, and LSF have some sort of field that holds
-- the assumption(s) that're being followed?

s7_1_req3 = foldlSent [S "The", phrase system, S "shall check the entered",
  plural inValue, S "to ensure that they do not exceed the",
  plural datumConstraint, S "mentioned in" +:+. 
  makeRef (SRS.datCon SRS.missingP []), S "If any of the", plural inParam,
  S "is out of bounds, an error message is displayed and the",
  plural calculation, S "stop"]

s7_1_req4 = foldlSent [titleize output_, S "the", plural inQty,
  S "from", acroR 1 `sAnd` S "the known", plural quantity,
  S "from", acroR 2]

s7_1_req5 cmd = S "If" +:+ (getS is_safe1) `sAnd` (getS is_safe2) +:+
  sParen (S "from" +:+ (makeRef (gbSymbMapT t1SafetyReq))
  `sAnd` (makeRef (gbSymbMapT t2SafetyReq))) +:+ S "are true" `sC`
  phrase cmd +:+ S "the message" +:+ Quote (safeMessage ^. defn) +:+
  S "If the" +:+ phrase condition +:+ S "is false, then" +:+ phrase cmd
  +:+ S "the message" +:+ Quote (notSafe ^. defn)

{-
s7_1_req6 = [(Enumeration $ Simple $ [(acroR 6, Nested (titleize output_ +:+
  S "the following" +: plural quantity)
  (Bullet $ 
    [Flat $ (at_start prob_br) +:+ sParen (getS prob_br) +:+ sParen (makeRef (gbSymbMapT probOfBr))] ++
    [Flat $ (titleize lResistance) +:+ sParen (short lResistance) +:+ sParen (makeRef (gbSymbMapT calOfCap))] ++
    [Flat $ (at_start demand) +:+ sParen (getS demand) +:+ sParen (makeRef (gbSymbMapT calOfDe))] ++
    [Flat $ (at_start act_thick) +:+ sParen (getS act_thick) +:+ sParen (makeRef (gbSymbMapD hFromt))] ++
    [Flat $ (titleize loadDF) +:+ sParen (getS loadDF) +:+ sParen (makeRef (gbSymbMapD loadDF))] ++
    [Flat $ (at_start strDisFac) +:+ sParen (getS strDisFac) +:+ sParen (makeRef (gbSymbMapD strDisFac))] ++
    [Flat $ (titleize nonFL) +:+ sParen (getS nonFL) +:+ sParen (makeRef (gbSymbMapD nonFL))] ++
    [Flat $ (titleize gTF) +:+ sParen (getS gTF) +:+ sParen (makeRef (gbSymbMapD glaTyFac))] ++
    map (\c -> Flat $ (at_start c) +:+ sParen (getS c) +:+ sParen (makeRef (gbSymbMapD c))) [dimLL, tolPre, tolStrDisFac] ++
    [Flat $ (titleize aspectR) +:+ sParen (short aspectR {-getS aspectR -}) {-+:+ E ((C aspectR) := (C plate_len):/(C plate_width))-}]
    ))])]
-}

s7_1_req6 = [(Enumeration $ Simple $ [(acroR 6, Nested (titleize output_ +:+
  S "the following" +: plural quantity)
  (Bullet $ 
    --FIXME:The implementation below is quite repetitive in nature.
    --map (\(c, d) -> Flat $ (at_start c) +:+ sParen (getS c) +:+ sParen (makeRef (gbSymbMapT d)))
    --[(prob_br, probOfBr), (demand, calOfDe), (lResistance, calOfCap)]
    --FIXME:The above doesn't work becuase prob_br = ConstrainedChunk, demand = UnitaryChunk, lResistance = CI
    [Flat $ (at_start prob_br) +:+ sParen (getS prob_br) +:+ sParen (makeRef (gbSymbMapT probOfBr))] ++
    [Flat $ (titleize lResistance) +:+ sParen (short lResistance) +:+ sParen (makeRef (gbSymbMapT calOfCap))] ++
    [Flat $ (at_start demand) +:+ sParen (getS demand) +:+ sParen (makeRef (gbSymbMapT calOfDe))]
    ++
    map (\d -> Flat $ (at_start d) +:+ sParen (getS d) +:+ sParen (makeRef (gbSymbMapD d)))
    s7_1_req6_pulledList
    ++
    [Flat $ (titleize aspectR) +:+ sParen (short aspectR) {-+:+ E ((C aspectR) := (C plate_len):/(C plate_width))-}]
    --short is technically a symbol here (see Concepts.hs)
    ))])]

{--Nonfunctional Requirements--}

s7_2 = SRS.nonfuncReq [s7_2_intro] []

s7_2_intro = foldlSP [
  S "Given the small size, and relative simplicity, of this", 
  phrase problem `sC` phrase performance, S "is not a" +:+. phrase priority, 
  S "Any reasonable", phrase implementation +:+. 
  S "will be very quick and use minimal storage", 
  S "Rather than", phrase performance `sC` S "the", phrase priority, 
  phrase nonfunctional, short requirement :+:
  S "s are", foldlList (map phrase gBRpriorityNFReqs)]

{--LIKELY CHANGES--}

s8 = SRS.likeChg [s8_list] []

--Considered "dead" knowldege?
s8_likelychg_list :: [Sentence]
s8_likelychg_list = [s8_likelychg1 (blastRisk), s8_likelychg2, s8_likelychg3, s8_likelychg4, 
  s8_likelychg5]

s8_likelychg1 :: NamedChunk -> Sentence
s8_likelychg2, s8_likelychg3, s8_likelychg4, 
  s8_likelychg5 :: Sentence

s8_likelychg1 mainConcept = foldlSent [acroA 3 `sDash` S "The", 
  phrase system, S "currently only calculates for external" +:+.
  phrase mainConcept, S "In the future", plural calculation,
  S "can be added for the internal", phrase mainConcept]

s8_likelychg2 = foldlSent [acroA 4 `sC` (acroA 8 `sDash`
  S "Currently the"), plural value, S "for",
  foldlList (map getS (take 3 assumption4_constants)),
  S "are assumed to be the same for all glass. In the", 
  S "future these", plural value, S "can be changed to",
  phrase variable, plural input_]

s8_likelychg3 = foldlSent [acroA 5 `sDash` S "The", phrase software, 
  S "may be changed to accommodate more than a single", phrase lite]

s8_likelychg4 = foldlSent [acroA 6 `sDash` S "The", phrase software, 
  S "may be changed to accommodate more boundary", plural condition, 
  S "than 4-sided support"]

s8_likelychg5 = foldlSent [acroA 7 `sDash` S "The", phrase software, 
  S "may be changed to consider more than just flexure of the glass"]

{--TRACEABLITY MATRICES AND GRAPHS--}

s9 = traceMGF traceyMatrices
  [(foldlList (map plural (take 3 solChSpecSubsections)) +:+. S "with each other"),
   (plural requirement +:+ S "on" +:+. foldlList (map plural solChSpecSubsections)),
   (foldlsC (map plural (take 3 solChSpecSubsections)) `sC` plural likelyChg `sAnd`
   plural requirement +:+ S "on the" +:+ plural assumption)]
  (traceyMatrices ++ (s9_intro2) ++ traceyGraphs)
  []

s9_theorys, s9_instaModel, s9_dataDef, s9_data, s9_funcReq, s9_assump, 
  s9_likelyChg :: [String]

s9_theorysRef, s9_instaModelRef, s9_dataDefRef, s9_dataRef, s9_funcReqRef, 
  s9_assumpRef, s9_likelyChgRef :: [Sentence]

s9_theorys = ["T1", "T2"]
s9_theorysRef = map (refFromType Theory gbSymbMap) tModels

s9_instaModel = ["IM1", "IM2", "IM3"]
s9_instaModelRef = map (refFromType Theory gbSymbMap) iModels

s9_dataDef =  ["DD1", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8", "DD9"]
s9_dataDefRef = map (refFromType Data gbSymbMap) dataDefns

s9_data  = ["Data Constraints"]
s9_dataRef = [makeRef (SRS.datCon SRS.missingP [])]

s9_funcReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
s9_funcReqRef = makeListRef s9_funcReq s7_1

s9_assump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"]
s9_assumpRef = makeListRef s9_assump (SRS.assump SRS.missingP [])

s9_likelyChg = ["LC1", "LC2", "LC3", "LC4", "LC5"]
s9_likelyChgRef = makeListRef s9_likelyChg s8

s9_row_t1 :: [String]
s9_row_t1 = s9_theorys ++ s9_instaModel ++ s9_dataDef

-- The headers for the first row, and column
s9_row_header_t1 :: [Sentence]
s9_row_header_t1 = zipWith itemRefToSent s9_row_t1 (s9_theorysRef ++ 
  s9_instaModelRef ++ s9_dataDefRef)

-- list of columns and their rows for traceability matrix
s9_columns_t1 :: [[String]]
s9_columns_t1 = [s9_t1_T1, s9_t1_T2, s9_t1_IM1, s9_t1_IM2, s9_t1_IM3, s9_t1_DD1, 
  s9_t1_DD2, s9_t1_DD3, s9_t1_DD4, s9_t1_DD5, s9_t1_DD6, s9_t1_DD7, s9_t1_DD8, 
  s9_t1_DD9]

s9_t1_T1, s9_t1_T2, s9_t1_IM1, s9_t1_IM2, s9_t1_IM3, s9_t1_DD1, s9_t1_DD2, 
  s9_t1_DD3, s9_t1_DD4, s9_t1_DD5, s9_t1_DD6, s9_t1_DD7, s9_t1_DD8, 
  s9_t1_DD9 :: [String]

-- list of each item that "this" item requires for traceability matrix
s9_t1_T1  = ["T2", "IM1"]
s9_t1_T2  = ["T1", "IM2", "IM3"]
s9_t1_IM1 = ["DD1", "DD2", "DD3", "DD4"]
s9_t1_IM2 = ["DD5", "DD6"]
s9_t1_IM3 = []
s9_t1_DD1 = []
s9_t1_DD2 = []
s9_t1_DD3 = []
s9_t1_DD4 = ["DD7"]
s9_t1_DD5 = ["DD2", "DD8"]
s9_t1_DD6 = []
s9_t1_DD7 = ["IM3", "DD2", "DD6"]
s9_t1_DD8 = ["DD9"]
s9_t1_DD9 = ["DD2", "DD3"]

s9_table1 = Table (EmptyS:s9_row_header_t1) 
  (makeTMatrix s9_row_header_t1 s9_columns_t1 s9_row_t1)
  (showingCxnBw (traceyMatrix) 
  (titleize' item +:+ S "of Different" +:+ titleize' section_)) True

--

s9_row_t2 :: [String]
s9_row_t2 = s9_row_t1 ++ s9_data ++ s9_funcReq

s9_row_header_t2, s9_col_header_t2 :: [Sentence]
s9_row_header_t2 = s9_row_header_t1 ++ 
  (zipWith itemRefToSent (s9_data ++ s9_funcReq) (s9_dataRef ++ s9_funcReqRef))

s9_col_header_t2 = map (\(x, y) -> S x +:+ sParen (S "in" +:+ y)) 
  (zip s9_funcReq s9_funcReqRef)

s9_t2_r1, s9_t2_r2, s9_t2_r3, s9_t2_r4, s9_t2_r5, 
  s9_t2_r6 :: [String]

s9_columns_t2 :: [[String]]
s9_columns_t2 = [s9_t2_r1, s9_t2_r2, s9_t2_r3, s9_t2_r4, s9_t2_r5, s9_t2_r6]
s9_t2_r1 = []
s9_t2_r2 = []
s9_t2_r3 = ["Data Constraints"]
s9_t2_r4 = ["R1", "R2"]
s9_t2_r5 = ["T1", "T2"]
s9_t2_r6 = ["IM1", "IM2", "IM3", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8", 
  "DD9"]

s9_table2 = Table (EmptyS:s9_row_header_t2)
  (makeTMatrix s9_col_header_t2 s9_columns_t2 s9_row_t2)
  (showingCxnBw (traceyMatrix) (titleize' requirement `sAnd` S "Other" +:+
  titleize' item)) True

--

s9_row_t3 :: [String]
s9_row_t3 = s9_assump

s9_row_header_t3, s9_col_header_t3 :: [Sentence]
s9_row_header_t3 = zipWith itemRefToSent s9_assump s9_assumpRef

s9_col_header_t3 = s9_row_header_t1 ++ (zipWith itemRefToSent
  (s9_likelyChg ++ s9_funcReq) (s9_likelyChgRef ++ s9_funcReqRef))

s9_columns_t3 :: [[String]]
s9_columns_t3 = [s9_t3_T1, s9_t3_T2, s9_t3_IM1, s9_t3_IM2, s9_t3_IM3, s9_t3_DD1, 
  s9_t3_DD2, s9_t3_DD3, s9_t3_DD4, s9_t3_DD5, s9_t3_DD6, s9_t3_DD7, s9_t3_DD8, 
  s9_t3_DD9, s9_t3_lc1, s9_t3_lc2, s9_t3_lc3, s9_t3_lc4, s9_t3_lc5, s9_t3_r1, 
  s9_t3_r2, s9_t3_r3, s9_t3_r4, s9_t3_r5, s9_t3_r6]

s9_t3_T1, s9_t3_T2, s9_t3_IM1, s9_t3_IM2, s9_t3_IM3, s9_t3_DD1, s9_t3_DD2, 
  s9_t3_DD3, s9_t3_DD4, s9_t3_DD5, s9_t3_DD6, s9_t3_DD7, s9_t3_DD8, 
  s9_t3_DD9, s9_t3_lc1, s9_t3_lc2, s9_t3_lc3, s9_t3_lc4, s9_t3_lc5, s9_t3_r1, 
  s9_t3_r2, s9_t3_r3, s9_t3_r4, s9_t3_r5, s9_t3_r6 :: [String]

-- list of each item that "this" item requires for traceability matrix
s9_t3_T1  = []
s9_t3_T2  = []
s9_t3_IM1 = ["A4", "A6", "A7"]
s9_t3_IM2 = ["A1", "A2", "A5"]
s9_t3_IM3 = []
s9_t3_DD1 = []
s9_t3_DD2 = []
s9_t3_DD3 = ["A4", "A8"]
s9_t3_DD4 = []
s9_t3_DD5 = ["A4"]
s9_t3_DD6 = []
s9_t3_DD7 = ["A5"]
s9_t3_DD8 = []
s9_t3_DD9 = ["A4"]
s9_t3_lc1 = ["A3"]
s9_t3_lc2 = ["A4", "A8"]
s9_t3_lc3 = ["A5"]
s9_t3_lc4 = ["A6"]
s9_t3_lc5 = ["A7"]
s9_t3_r1  = []
s9_t3_r2  = ["A4", "A5", "A8"]
s9_t3_r3  = []
s9_t3_r4  = []
s9_t3_r5  = []
s9_t3_r6  = []

s9_table3 = Table (EmptyS:s9_row_header_t3)
  (makeTMatrix s9_col_header_t3 s9_columns_t3 s9_row_t3)
  (showingCxnBw (traceyMatrix) (titleize' assumption `sAnd` S "Other"
  +:+ titleize' item)) True

--

s9_intro2 = traceGIntro traceyGraphs
  [(foldlList (map plural (take 3 solChSpecSubsections)) +:+. S "on each other"),
  (plural requirement +:+ S "on" +:+. foldlList (map plural solChSpecSubsections)),
  (foldlList ((map plural (take 3 solChSpecSubsections))++
  [plural requirement, plural likelyChg +:+ S "on" +:+ plural assumption]))]

fig_2 = figureLabel "2" (traceyMatrix)
  (titleize' item +:+ S "of Different" +:+ titleize' section_)
  ("Trace.png")

fig_3 = figureLabel "3" (traceyMatrix)
  (titleize' requirement `sAnd` S "Other" +:+ titleize' item)
  ("RTrace.png")

fig_4 = figureLabel "4" (traceyMatrix)
  (titleize' assumption `sAnd` S "Other" +:+ titleize' item)
  ("ATrace.png")

{--VALUES OF AUXILIARY CONSTANTS--}

s10 = valsOfAuxConstantsF gLassBR auxiliaryConstants

{--REFERENCES--}

s11 = SRS.reference [s11_list] []

s11_list = mkRefsList 1 (map (foldlsC) references)

s11_ref1, s11_ref2, s11_ref3, s11_ref4, s11_ref5, s11_ref6 :: [Sentence]

references :: [[Sentence]]
references = [s11_ref1, s11_ref2, s11_ref3, s11_ref4, s11_ref5, s11_ref6]

s11_ref1 = [S "N. Koothoor",
  Quote (S "A" +:+ phrase document +:+ S "drive approach to certifying" 
  +:+ phrase sciCompS :+: S ",") +:+ S "Master's thesis", 
  S "McMaster University, Hamilton, Ontario, Canada", S "2013."]

s11_ref2 = [S "W. S. Smith and L. Lai", 
  Quote (S "A new requirements template for scientific computing,")
  +:+ S "in Proceedings of the First International Workshop on Situational Requirements" +:+ 
  S "Engineering Processes - Methods, Techniques and Tools to Support Situation-Specific Requirements" +:+
  S "Engineering Processes, SREP'05"
  +:+ sParen (S "J.Ralyt" :+: (F Acute 'e') `sC` S "P.Agerfalk, and N.Kraiem, eds."),
  sParen (S "Paris, France"), S "pp. 107-121", 
  S "In conjunction with 13th IEEE International Requirements Engineering Conference",
  S "2005."]
  --FIXME:Make a compoundNC "requirement template"?

s11_ref3 = [S "J. Robertson and S. Robertson", 
  Quote (S "Volere requirements specification template edition 16.") +:+
  Quote (S "www.cs.uic.edu/ i442/VolereMaterials/templateArchive16/c Volere template16.pdf"),
  S "2012."]
  --FIXME:Make a compoundNC "requirement specification template"?

s11_ref4 = [S "ASTM Standards Committee",
  Quote (S "Standard practice for determining load resistance of glass in buildings,")
  +:+ S "Standard E1300-09a", S "American Society for Testing and Material (ASTM)",
  S "2009."]

s11_ref5 = [S "ASTM", S "developed by subcommittee C1408", S "Book of standards 15.02",
  Quote (S "Standard" +:+ phrase specification +:+. S "for flat glass, C1036")]

s11_ref6 = [S "ASTM", S "developed by subcommittee C14.08", S "Book of standards 15.02",
  Quote (at_start specification +:+ S "for" +:+ plural heat +:+.
  S "treated flat glass-Kind HS, kind FT coated and uncoated glass, C1048")]

{--APPENDIX--}

s12 = SRS.appendix [s12_intro, fig_5, fig_6] []

s12_intro = foldlSP [
  S "This", phrase appendix, S "holds the", plural graph, 
  sParen ((makeRef fig_5) `sAnd` (makeRef fig_6)), 
  S "used for interpolating", plural value, S "needed in the", plural model]

fig_5 = Figure (titleize figure +: S "5" +:+ (demandq ^. defn) +:+ 
  sParen (getS demand) `sVersus` at_start sD `sVersus` 
  at_start char_weight +:+ sParen (getS sflawParamM)) "ASTM_F2248-09.png"

fig_6 = Figure (titleize figure +:+ S "6: Non dimensional" +:+ 
  phrase lateral +:+ phrase load +:+ sParen (getS dimlessLoad)
  `sVersus` titleize aspectR +:+ sParen (short aspectR)
  `sVersus` at_start stressDistFac +:+ sParen (getS stressDistFac))
  "ASTM_F2248-09_BeasonEtAl.png"