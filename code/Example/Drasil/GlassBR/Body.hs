module Drasil.GlassBR.Body where
import Control.Lens ((^.))

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Authors
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Software.Products
import Data.Drasil.Concepts.Computation
import Data.Drasil.Concepts.Software (performance)
import Data.Drasil.Concepts.Math (graph, calculation, equation, 
                                  surface, probability, parameter)
import Data.Drasil.Concepts.Thermodynamics (heat)
import Prelude hiding (id)
import Data.Drasil.Utils (itemRefToSent, getS,
  makeTMatrix, makeListRef, mkRefsList, refFromType, enumSimple, enumBullet)
import Data.Drasil.SentenceStructures (foldlSent, foldlList, ofThe, isThe, 
  showingCxnBw, figureLabel, foldlSP, sAnd)

import Drasil.Template.MG
import Drasil.Template.DD

import           Drasil.TableOfSymbols
import qualified Drasil.SRS as SRS
import           Drasil.ReferenceMaterial

import Drasil.GlassBR.Unitals
import Drasil.GlassBR.Concepts
import Drasil.GlassBR.Changes
import Drasil.GlassBR.Modules
import Drasil.GlassBR.Reqs
import Drasil.GlassBR.TMods
import Drasil.GlassBR.IMods
import Drasil.GlassBR.DataDefs

import Drasil.DocumentLanguage
import Drasil.TraceabilityMandGs
import Drasil.Stakeholders
import Drasil.Requirements
import Drasil.GeneralSystDesc
import Drasil.SpecificSystemDescription

this_si :: [UnitDefn]
this_si = map UU [metre, second] ++ map UU [pascal, newton]

s3, s4, s4_2, 
  s5, s5_1, s5_2, s6, s6_1, s6_1_1, s6_1_2, s6_1_3, s6_2, s6_2_1, 
  s6_2_2, s6_2_3, s6_2_4, s6_2_5, s7, s7_1, s7_2, s8, s9, s10, s11 :: Section 

s4_1_bullets, s5_intro, s5_1_table, s5_2_bullets, 
  s6_1_1_bullets, s6_1_2_list, s6_1_3_list, 
  s6_2_intro, s6_2_5_table1, s6_2_5_table2, 
  s6_2_5_intro2, s6_2_5_table3, s7_2_intro, s8_list, s9_table1, 
  s9_table2, s9_table3, s10_list, s11_intro, fig_glassbr, fig_2, 
  fig_3, fig_4, fig_5, fig_6 :: Contents

s6_2_1_list, s7_1_list, s9_intro2 :: [Contents]

srs_authors, mg_authors, s2_3_intro_end, s2_3_intro :: Sentence
srs_authors = manyNames [nikitha, spencerSmith]
mg_authors = manyNames [spencerSmith, thulasi]

authors :: People
authors = [nikitha, spencerSmith]

glassBR_srs' :: Document
glassBR_srs' = mkDoc' mkSRS (for'' titleize phrase) glassSystInfo

mkSRS :: DocDesc 
mkSRS = RefSec (RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA]):
  IntroSec (IntroProg startIntro (short gLassBR) 
  [IPurpose (s2_1_intro_p1), 
  IScope incScoR endScoR, 
  IChar knowIR undIR appStanddIR, 
  IOrgSec s2_3_intro dataDefn s6_2_4 s2_3_intro_end]) :
  map Verbatim [s3, s4, s5, s6, s7, s8, s9, s10, s11]
  
glassSystInfo :: SystemInformation
glassSystInfo = SI glassBRProg srs authors this_si this_symbols 
  ([] :: [CQSWrapper]) acronyms --FIXME: All named ideas, not just acronyms.

mgBod :: [Section]
(mgBod, _) = makeDD likelyChanges unlikelyChanges reqs modules

glassBR_mg :: Document
glassBR_mg = mgDoc'' glassBRProg (for'' titleize phrase) mg_authors mgBod

-----------------------
-- S2: Intro helpers --
-----------------------

startIntro, knowIR, undIR, appStanddIR, incScoR, endScoR :: Sentence
startIntro = foldlSent [(at_start software), 
  S "is helpful to efficiently and correctly predict the", 
  (phrase blastRisk), S "involved with the" +:+. (phrase glaSlab), 
  S "The", (phrase blast), S "under consideration is" +:+. (blast ^. defn), 
  S "The", phrase software `sC` S "herein called", (short gLassBR), 
  S "aims to predict the", (phrase blastRisk), S "involved with the", 
  (phrase glaSlab), S "using an intuitive interface"]
knowIR = (phrase theory +:+ S "behind" +:+ (phrase glBreakage) `sAnd`
  (phrase blastRisk))
undIR = (foldlList [S "second year calculus", S "structural mechanics", 
  S "computer applications in civil engineering"])
appStanddIR = (S "In addition, reviewers should be familiar with the" +:+
  S "applicable standards for constructions using glass from" +:+
  sSqBr (S "4-6") +:+ S "in" +:+. (makeRef s10))
incScoR = foldl (+:+) EmptyS [S "getting all", phrase input_, (plural parameter), 
  S "related to the", (phrase glaSlab), S "and also the", (plural parameter), 
  S "related to", (phrase blastTy)]
endScoR = foldl (+:+) EmptyS [S "use the", plural datum, S "and predict whether the", 
  (phrase glaSlab), S "is safe to use or not"]
--for Purpose of Document Section
s2_1_intro_p1 :: Sentence
s2_1_intro_p1 = foldlSent [S "The main", phrase purpose, S "of this", 
  phrase document, S "is to predict whether a given", (phrase glaSlab), 
  S "is likely to resist a specified" +:+. (phrase blast), 
  S "The", plural goal `sAnd` plural thModel, S "used in the", 
  (short gLassBR), S "code are provided" `sC`
  S "with an emphasis on explicitly identifying",   (plural assumption), 
  S "and unambiguous" +:+. plural definition, S "This", phrase document, 
  S "is intended to be used as a", phrase reference, S "to provide all", 
  phrase information, S "necessary to understand and verify the" +:+.
  phrase analysis, S "The", (short srs), S "is abstract because the", 
  plural content, S "say what", phrase problem, 
  S "is being solved, but not how to solve it"]

--for Organization of Document Section
s2_3_intro = foldlSent [S "The", phrase organization, S "of this", 
  phrase document, S "follows the", phrase template, S "for an", (short srs), 
  S "for", phrase sciCompS, S "proposed by" +:+ sSqBr (S "1") `sAnd` sSqBr (S "2"),
  sParen (S "in" +:+ (makeRef s10)) `sC`
  S "with some aspects taken from Volere", phrase template, S "16", 
  sSqBr (S "3")]
  
s2_3_intro_end = foldl (+:+) EmptyS [(at_start' $ the dataDefn), 
  S "are used to support", (plural definition `ofThe` S "different"), 
  plural model]
  
s3 = stakehldrGeneral (gLassBR) 
  (S "Entuitive. It is developed by Dr. Manuel Campidelli")

s4 = genSysF [] s4_1_bullets [] []

s4_1_bullets = enumBullet [(S "The" +:+ phrase endUser +:+ S "of" +:+ 
  (short gLassBR) +:+ S "is expected to have completed at least" +:+.
  (S "equivalent" `ofThe` 
  S "second year of an undergraduate degree in civil or structural engineering")), 
  (S "The" +:+ phrase endUser +:+ S "is expected to have an understanding of" +:+
  phrase theory +:+ S "behind" +:+. ((phrase glBreakage) `sAnd`
  (phrase blastRisk))), (S "The" +:+ phrase endUser +:+
  S "is expected to have basic" +:+ phrase computer +:+
  S "literacy to handle the" +:+. phrase software)]

s4_2 = systCon [] []

s5 = SRS.scpOfTheProj [s5_intro] [s5_1, s5_2]

s5_intro = foldlSP [S "This", phrase section_, 
  S "presents the" +:+. phrase (scpOfTheProj phrase), 
  S "It describes the expected use of", (short gLassBR), 
  S "as well as the", plural input_ `sAnd` plural output_, 
  S "of each action. The", plural useCase, S "are", 
  phrase input_ `sAnd` phrase output_ `sC` 
  S "which defines the action of getting the", 
  phrase input_, S "and displaying the", phrase output_]

s5_1 = SRS.prodUCTable [s5_1_table] []

s5_1_table = Table [titleize useCase +:+. S "NO", titleize useCase +:+
  titleize name_, S "Actor", titleize input_ `sAnd` titleize output_]
  (mkTable
  [(\x -> (x!!0)), (\x -> (x!!1)), (\x -> (x!!2)), (\x -> (x!!3))]
  [[S "1", titleize' input_, titleize user, titleize' characteristic +:+
  S "of the" +:+ (phrase glaSlab) +:+ S "and of the" +:+.
  (phrase blast) +:+ S "Details in" +:+ (makeRef s5_2)], 
  [S "2", titleize output_, (short gLassBR), S "Whether or not the" +:+
  (phrase glaSlab) +:+ S "is safe for the calculated" +:+
  (phrase load) +:+ S "and supporting" +:+
  S "calculated" +:+ plural value]]) 
  (titleize table_ +: S "1" +:+ titleize useCaseTable) True

s5_2 = SRS.indPRCase [s5_2_bullets] []

s5_2_bullets = enumBullet [s5_2_bt_sent1, s5_2_bt_sent2]

s5_2_bt_sent1 :: Sentence
s5_2_bt_sent1 = foldlSent [titleize useCase, S "1 refers to the", phrase user,
  S "providing", phrase input_, S "to", (short gLassBR),
  S "for use within the" +:+. phrase analysis, S "There are two classes of"
  +: plural input_ +:+. ((phrase glassGeo) `sAnd` (phrase blastTy)),
  (glassGeo ^. defn), (blastTy ^. defn), S "These",
  (plural parameter), S "describe", (phrase char_weight),
  S "and stand off" +:+. (phrase blast), S "Another", phrase input_,
  S "the", phrase user, S "gives" `isThe` S "tolerable", phrase value, S "of",
  (phrase prob_br)]

s5_2_bt_sent2 :: Sentence
s5_2_bt_sent2 = foldlSent [S " Use Case 2", (short gLassBR), 
  plural output_, S "if the", (phrase glaSlab), S "will be safe by", 
  S "comparing whether", (phrase capacity), S "is greater than" +:+. 
  (phrase demandq), ((at_start capacity) `isThe`
  (capacity ^. defn)) `sAnd` ((phrase demandq) `isThe` 
  phrase requirement) +:+. (S "which" `isThe` (demandq ^. defn)), 
  S "The second", phrase condition, S "is to check whether the calculated", 
  (phrase probability), sParen (getS prob_br), 
  S "is less than the tolerable", (phrase probability), 
  sParen (getS pb_tol), S "which is obtained from the", phrase user, 
  S "as an" +:+. phrase input_, S "If both", plural condition, 
  S "return true then it's shown that the", (phrase glaSlab), 
  S "is safe to use" `sC` S "else if both return false then the", 
  (phrase glaSlab) +:+. S "is considered unsafe", 
  S "All the supporting calculated", plural value, S "are also displayed as", 
  phrase output_]

s6 = specSysDesF (S "and" +:+ plural definition) [s6_1, s6_2]

s6_1 = probDescF start gLassBR ending [s6_1_1, s6_1_2, s6_1_3]
  where start = foldlSent [S "A", phrase system, 
                S "is needed to efficiently and correctly predict the", 
                (phrase blastRisk) +:+ S "involved with the glass"]
        ending = foldl (+:+) EmptyS [S "interpret the", plural input_, 
                S "to give out the", plural output_, 
                S "which predicts whether the", (phrase glaSlab), 
                S "can withstand the", (phrase blast), S "under the", 
                plural condition]

s6_1_1 = termDefnF (S "All of the terms are extracted from" +:+ 
  sSqBr (S "4") +:+ S "in" +:+. (makeRef s10)) [s6_1_1_bullets]

s6_1_1_bullets = Enumeration $ (Number $
  map (\b -> Flat $ ((at_start b) +:+ S "- ") :+: (b ^. defn)) 
  [glBreakage, lateral, lite, specA, blastResisGla, eqTNTChar]
  ++
  s6_1_1_bullets_glTySubSec
  ++
  s6_1_1_bullets_loadSubSec
  ++
  map (\a -> Flat $ ((at_start a) :+: sParenDash (getS a)) :+: (a ^. defn))
  [standOffDist]
  ++
  map (\(d, a) -> Flat $ ((at_start d) :+: sParenDash (short a)) :+: (d ^. defn))
  [(loadShareFac, lShareFac), (glTyFac, glassTypeFac), (aspectRatio, aspectR)]
  ++ 
  map (\c -> Flat c)
  [(((at_start probBreak) :+: sParenDash (getS prob_br)) :+: (probBreak ^. defn))])

-- Terminology and Definition Subsection Helpers --

s6_1_1_bullets_glTySubSec, s6_1_1_bullets_loadSubSec :: [ItemType]

s6_1_1_bullets_glTySubSec = [Nested (((titleize glassTy) :+: S ":"))
  (Bullet $ 
  map (\(d, a) -> Flat $ ((at_start d) :+: sParenDash (short a)) :+: (d ^. defn)) 
  [(annealedGl, annealedGlass), (fTemperedGl, fullyTGlass), (hStrengthGl, heatSGlass)])]

s6_1_1_bullets_loadSubSec = [Nested (((at_start load) :+: S ":"))
  (Bullet $ map (\c -> Flat c)
  [(((at_start loadResis) :+: sParenDash (short lResistance)) :+: (loadResis ^. defn)),
  (((at_start nonFL) +:+ sParenDash (getS nonFL)) :+: (nonFactoredL ^. defn))]
  ++ 
  map (\c -> Flat $ ((at_start c) +:+ S "- ") :+: (c ^. defn))
    [glassWL, shortDurLoad, specDeLoad, longDurLoad])]

s6_1_2 = physSystDesc (short gLassBR) (fig_glassbr) [s6_1_2_list, fig_glassbr]

fig_glassbr = Figure (at_start $ the physicalSystem) "physicalsystimage.png"
  
s6_1_2_list = enumSimple 1 (short physSyst) s6_1_2_list_physys1

s6_1_2_list_physys1 :: [Sentence]
s6_1_2_list_physys1 = [(at_start glaSlab), (foldlSent [S "The point of"
  +:+. (phrase explosion), S "Where the", (phrase bomb) `sC` 
  S "or", (blast ^. defn) `sC` S "is located. The", (phrase sD) 
  `isThe` S "distance between the point of", (phrase explosion), 
  S "and the glass"])]

s6_1_3 = goalStmtF [foldlList [S "dimensions" `ofThe`S "glass plane", 
  phrase glassTy, plural characteristic `ofThe` phrase explosion, 
  S "the" +:+ phrase pb_tol]] [s6_1_3_list]

s6_1_3_list = enumSimple 1 (short goalStmt) s6_1_3_list_goalStmt1

s6_1_3_list_goalStmt1 :: [Sentence]
s6_1_3_list_goalStmt1 = [foldlSent [S "Analyze and predict whether the", 
  (phrase glaSlab), S "under consideration", 
  S "will be able to withstand the", (phrase explosion), 
  S "of a certain degree which is calculated based on", phrase user, 
  phrase input_]]

s6_2 = solChSpecF gLassBR (s6_1, s8) False (EmptyS) (tbRef, EmptyS, True, end)
 (s6_2_1_list, s6_2_2_TMods, [], s6_2_4_DDefns, s6_2_3_IMods, 
  [s6_2_5_table1, s6_2_5_table2, s6_2_5_intro2, s6_2_5_table3]) []
  where tbRef = (makeRef s6_2_5_table1) +:+ S "shows"
        end = foldlSent [(makeRef s6_2_5_table2), S "gives", 
             (plural value `ofThe` S "specification"), (plural parameter), 
              S "used in" +:+. (makeRef s6_2_5_table1), (getS ar_max), --FIXME: Issue #167
              S "refers to the", (phrase ar_max), S "for the plate of glass"]

s6_2_intro = foldlSP [S "This", phrase section_, 
  S "explains all the", (plural assumption), S "considered and the", 
  plural thModel, S "which are supported by the", (plural dataDefn)]
  
s6_2_1 = assumpF' (s6_2_2) (s6_2_4) (s6_2_3) (s8) (s6_2_1_list)

s6_2_1_list = 
  [(enumSimple 1 (short assumption) s6_2_1_list_assum1), 
  assumptionM, assumptionK, assumptionMod_Elas, assumptionLoad_Dur,
  (enumSimple 5 (short assumption) s6_2_1_list_assum2)]

assumptionM, assumptionK, assumptionMod_Elas, assumptionLoad_Dur :: Contents
assumptionM = EqnBlock $ (C sflawParamM) := (Int 7)
assumptionK = EqnBlock $ (C sflawParamK):=(Grouping (Dbl 2.86)):*(Int 10):^(Neg (Int 53))
assumptionMod_Elas = EqnBlock $ (C mod_elas):=(Grouping (Dbl 7.17)):*(Int 10):^(Int 7)
assumptionLoad_Dur = EqnBlock $ (C load_dur):=(Int 3)

s6_2_1_list_assum1 :: [Sentence]
s6_2_1_list_assum1 = [foldlSent [S "The standard E1300-09a for", 
  (phrase calculation), 
  S "applies only to monolithic, laminated, or insulating", 
  S "glass constructions of rectangular shape with continuous", 
  (phrase lateral), 
  S "support along one, two, three, or four edges. This practice assumes", 
  S "that (1) the supported glass edges for two, three and four-sided", 
  S "support", plural condition, S "are simply supported and free to slip in", 
  S "plane; (2) glass supported on two sides acts as a simply supported", 
  S "beam and (3) glass supported on one side acts as a cantilever"], 
  foldlSent [S "Following", (sSqBr (S "4 (pg. 1)")) `sC`
  S "this practice does not apply to any form of wired, patterned" `sC`
  S "etched, sandblasted, drilled, notched, or grooved glass with", 
  (phrase surface), S "and edge treatments that alter the glass strength"], 
  foldlSent [S "This", phrase system, S "only considers the external", 
  (phrase explosion), S "scenario for its", 
  (plural calculation)], 
  (S "Standard" +:+ plural value +:+ S "used for" +:+
  (phrase calculation) +:+ S "in" +:+ (short gLassBR) +: S "are")]

s6_2_1_list_assum2 :: [Sentence]
s6_2_1_list_assum2 = [(foldlSent [S "Glass under consideration", 
  S "is assumed to be a single" +:+. (phrase lite), S "Hence the", 
  phrase value, S "of", (short lShareFac), S "is equal to 1 for all", 
  (plural calculation), S "in", (short gLassBR)]), 
  (foldlSent [S "Boundary", plural condition, S "for the", 
  (phrase glaSlab), S "is assumed to be 4-sided", 
  S "support for", (plural calculation)]), 
  (foldlSent [S "The response type considered in", (short gLassBR), 
  S "is flexural"]), 
  (foldlSent [S "With", phrase reference, S "to A4 the", phrase value, 
  S "of", (phrase loadDF), sParen (getS loadDF), 
  S "is a constant in" +:+. (short gLassBR), S "It is calculated by the" +: 
  (phrase equation), --(getS loadDF) +:+ S "=" +:+ (getS load_dur) :+: 
  S ". Using this" `sC` (getS loadDF), S "= 0.27"])]

s6_2_2 = thModF (gLassBR) (s6_2_2_TMods) 
  
s6_2_2_TMods :: [Contents]
s6_2_2_TMods = map gbSymbMapT tModels

s6_2_3 = inModelF' s6_1 s6_2_4 s6_2_2 (s6_2_3_IMods)

s6_2_3_IMods :: [Contents]
s6_2_3_IMods = map gbSymbMapT iModels

s6_2_4 = dataDefnF EmptyS (s6_2_4_DDefns)

s6_2_4_DDefns ::[Contents] 
s6_2_4_DDefns = map gbSymbMapD dataDefns

s6_2_5 = datConF ((makeRef s6_2_5_table1) +:+ S "shows") EmptyS True end 
                 [s6_2_5_table1, s6_2_5_table2, s6_2_5_intro2] --issue #213: discrepancy?
  where end = foldlSent [(makeRef s6_2_5_table3), S "gives the", 
              (plural value `ofThe` S "specification"), (plural parameter), 
              S "used in" +:+. (makeRef s6_2_5_table1), (getS ar_max), --FIXME: Issue #167
              S "refers to the", (phrase ar_max), S "for the plate of glass"]

s6_2_5_table1 = Table [S "Var", S "Physical Cons", S "Software Constraints", 
  S "Typical Value", S "Uncertainty"] (mkTable [(\x -> x!!0), (\x -> x!!1), 
  (\x -> x!!2), (\x -> x!!3), (\x -> x!!4)] 
  [inputVarA, inputVarB, inputVarPbTol, inputVarW, inputVarTNT, inputVarSD])
  (titleize table_ +: S "2" +:+ titleize input_ +:+ titleize' variable) 
  True

inputVarA, inputVarB, inputVarPbTol, inputVarW, inputVarTNT, inputVarSD :: [Sentence]

inputVarA = [(getS plate_len), 
  E ((C plate_len) :> (Int 0)) `sAnd` E ((C plate_len) :/ (C plate_width) :> (Int 1)),
  (getS dim_min) +:+ S "<=" +:+ (getS plate_len) +:+ S "<=" +:+ (getS dim_max)
  `sAnd` E ((C plate_len) :/ (C plate_width) :< (C ar_max)),
  E (Int 1500) +:+ Sy (unit_symb plate_len),
  S "10%"]

inputVarB = [(getS plate_width),
  E ((C plate_width) :> Int 0) `sAnd` E ((C plate_width) :< (C plate_len)),
  (getS dim_min) +:+ S "<=" +:+ (getS plate_width) +:+ S "<=" +:+ (getS dim_max)
  `sAnd` E ((C plate_len) :/ (C plate_width) :< (C ar_max)),
  E (Int 1200) +:+ Sy (unit_symb plate_width),
  S "10%"]

inputVarPbTol = [(getS pb_tol),
  E (Int 0 :< (C pb_tol) :< Int 1),
  S "-",
  E (Dbl 0.008),
  S "0.1%"]

inputVarW = [(getS char_weight),
  (getS char_weight) +:+ S ">=" +:+ E (Int 0),
  E ((C cWeightMin) :< (C char_weight) :< (C cWeightMax)),
  E (Int 42) +:+ Sy (unit_symb char_weight), 
  S "10%"]

inputVarTNT = [(getS tNT),
  E ((C tNT) :> (Int 0)),
  S "-",
  E (Int 1), 
  S "10%"]

inputVarSD = [(getS standOffDist),
  E ((C standOffDist) :> (Int 0)),
  E ((C sd_min) :< (C standOffDist) :< (C sd_max)),
  E (Int 45) +:+ Sy (unit_symb standOffDist), 
  S "10%"]

s6_2_5_table2 = Table [S "Var", titleize value] (mkTable 
  [(\x -> fst x), (\x -> snd x)]
  (zipWith s6_2_5_table2_formatF
  [dim_min, dim_max, cWeightMin, cWeightMax, sd_min, sd_max]
  [(Int 100), (Int 100), (Dbl 4.5), (Int 910), (Int 6), (Int 130)]))
  --FIXME: how to incorporate (getS ar_max, E (Int 5))? (Issue #272)
  (titleize table_ +: E (Int 3) +:+ titleize specification +:+
  (titleize parameter) +:+ titleize' value) True

s6_2_5_table2_formatF :: UnitaryChunk -> Expr -> (Sentence, Sentence)
s6_2_5_table2_formatF varName val = (getS varName, E (val) +:+ Sy (unit_symb varName))

s6_2_5_intro2 = foldlSP [(makeRef s6_2_5_table3), S "shows the", 
  plural constraint, S "that must be satisfied by the", phrase output_]

s6_2_5_table3 = Table [S "Var", S "Physical Constraints"] (mkTable 
  [(\x -> P $ fst(x)), (\x -> snd(x))] 
  [(prob_br ^. symbol, E (Int 0 :< C prob_br :< Int 1))])
  (titleize table_ +: S "4" +:+ titleize output_ +:+ titleize' variable) True

s7 = reqF [s7_1, s7_2]

s7_1 = SRS.funcReq (s7_1_list) []

s7_1_req1, s7_1_req2, s7_1_req6 :: [Contents]
s7_1_req3, s7_1_req4, s7_1_req5 :: Sentence

s7_1_list = s7_1_req1++s7_1_req2++[enumSimple 3 (getAcc requirement) (s7_1_listOfReqs)]++s7_1_req6

s7_1_listOfReqs :: [Sentence]
s7_1_listOfReqs = [s7_1_req3, s7_1_req4, s7_1_req5]

s7_1_req1 = [(Enumeration $ Simple $ map (\(a, b) -> (a, Flat b))
  [(acroR "1", at_start input_ +:+ S "the following" +:+
  plural quantity `sC` S "which define the glass dimensions" `sC` 
  (glassTy ^. defn) `sC` S "tolerable" +:+ (phrase probability) +:+
  S "of failure and" +: (plural characteristic `ofThe` phrase blast))]),
  s7_1_req1Table]

s7_1_req1Table :: Contents
s7_1_req1Table = (table ((map qs [plate_len, plate_width, sdx, sdy, sdz, nom_thick, char_weight]) 
  ++ (map qs [glass_type, pb_tol, tNT])) (\x -> at_start x))

s7_1_req2 = [(Enumeration $ Simple $
   [(acroR "2", Nested (S "The" +:+ phrase system +:+
   S "shall set the known" +:+ plural value +: S "as follows")
    (Bullet $ map (\c -> Flat c) 
     [(getS sflawParamM) `sC` (getS sflawParamK) `sC` 
     (getS mod_elas) `sC` (getS load_dur) +:+ 
     S "following" +:+ acroA "4", 
     (getS loadDF) +:+ S "following" +:+ acroA "8", 
     (short lShareFac) +:+ S "following" +:+ acroA "5"]))])]

s7_1_req3 = foldlSent [S "The", phrase system, S "shall check the entered",
  phrase input_, plural value, S "to ensure that they do not exceed the",
  plural datumConstraint, S "mentioned in" +:+. (makeRef s6_2_5),
  S "If any of the", phrase input_, plural parameter,
  S "is out of bounds, an error message is displayed and the",
  plural calculation, S "stop"]

s7_1_req4 = foldlSent [titleize output_, S "the", phrase input_,
  plural quantity, S "from", acroR "1", S "and the known", plural quantity,
  S "from", acroR "2"]

s7_1_req5 = S "If" +:+ (getS is_safe1) `sAnd`
  (getS is_safe2) +:+ sParen (S "from" +:+ (makeRef (gbSymbMapT t1SafetyReq))
  `sAnd` (makeRef (gbSymbMapT t2SafetyReq))) +:+ S "are true" `sC`
  phrase output_ +:+ S "the message" +:+ Quote (safeMessage ^. defn) +:+
  S "If the" +:+ phrase condition +:+ S "is false, then" +:+ phrase output_ +:+
  S "the message" +:+ Quote (notSafe ^. defn)

s7_1_req6 = [(Enumeration $ Simple $ [(acroR "6", Nested (titleize output_ +:+
  S "the following" +: plural quantity)
  (Bullet $ 
    [Flat $ (at_start prob_br) +:+ sParen (getS prob_br) +:+ 
    sParen (makeRef (gbSymbMapT probOfBr))] ++
    [Flat $ (titleize lResistance) +:+ sParen (short lResistance) +:+ 
    sParen (makeRef (gbSymbMapT calOfCap))] ++
    [Flat $ (at_start demand) +:+ sParen (getS demand) +:+
    sParen (makeRef (gbSymbMapT calOfDe))] ++
    [Flat $ (at_start act_thick) +:+ sParen (getS act_thick) +:+
    sParen (makeRef (gbSymbMapD hFromt))] ++
    [Flat $ (titleize loadDF) +:+ sParen (getS loadDF) +:+ 
    sParen (makeRef (gbSymbMapD loadDF))]++
    [Flat $ (at_start strDisFac) +:+ sParen (getS strDisFac) +:+ 
    sParen (makeRef (gbSymbMapD strDisFac))]++
    [Flat $ (titleize nonFL) +:+ sParen (getS nonFL) +:+ 
    sParen (makeRef (gbSymbMapD nonFL))]++
    [Flat $ (titleize glassTypeFac) +:+ sParen (short glassTypeFac) +:+ 
    sParen (makeRef (gbSymbMapD glaTyFac))] ++
    map (\c -> Flat $ (at_start c) +:+ sParen (getS c) +:+ sParen (makeRef (gbSymbMapD c)))
    [dimLL, tolPre, tolStrDisFac] ++
    [Flat $ (titleize aspectR) +:+ sParen (short aspectR {-getS aspectR -})]
    ))])]

s7_2 = SRS.nonfuncReq [s7_2_intro] []

s7_2_intro = foldlSP [
  S "Given the small size, and relative simplicity, of this", 
  phrase problem `sC` (phrase performance), S "is not a" +:+. phrase priority, 
  S "Any reasonable", phrase implementation +:+. 
  S "will be very quick and use minimal storage", 
  S "Rather than", (phrase performance) `sC` S "the", phrase priority, 
  phrase nonfunctional, (short requirement) :+:
  S "s are correctness, verifiability, understandability, reusability,", 
  S "maintainability and portability"]

s8 = SRS.likeChg [s8_list] []

s8_likelychg1, s8_likelychg2, s8_likelychg3, s8_likelychg4, 
  s8_likelychg5 :: Sentence

s8_likelychg1 = foldlSent [acroA "3" +:+ S "- The", phrase system, 
  S "currently only calculates for external" +:+. (phrase blastRisk), 
  S "In the future", (plural calculation), 
  S "can be added for the internal", (phrase blastRisk)]

s8_likelychg2 = foldlSent [acroA "4" `sC` acroA "8",
  S "- Currently the", plural value, S "for",
  (getS sflawParamM) `sC` (getS sflawParamK) `sC`
  S "and", (getS mod_elas), S "are assumed to be the", 
  S "same for all glass. In the future these", plural value, 
  S "can be changed to", phrase variable, plural input_]

s8_likelychg3 = foldlSent [acroA "5" +:+ S "- The", phrase software, 
  S "may be changed to accommodate more than a single", (phrase lite)]

s8_likelychg4 = foldlSent [acroA "6" +:+ S "- The", phrase software, 
  S "may be changed to accommodate more boundary", plural condition, 
  S "than 4-sided support"]

s8_likelychg5 = foldlSent [acroA "7" +:+ S "- The", phrase software, 
  S "may be changed to consider more than just flexure of the glass"]

s8_likelychg_list :: [Sentence]
s8_likelychg_list = [s8_likelychg1, s8_likelychg2, s8_likelychg3, s8_likelychg4, 
  s8_likelychg5]

s8_list = enumSimple 1 (short likelyChg) s8_likelychg_list

s9 = traceMGF [s9_table1, s9_table2, s9_table3]
  [(plural thModel `sC` (plural dataDefn) `sAnd` plural inModel +:+.
  S "with each other"), (plural requirement +:+ S "on" +:+ plural thModel `sC`
  (plural inModel) `sC` (plural dataDefn) +:+ S "and" +:+. plural datumConstraint), 
  (plural thModel `sC` (plural dataDefn) `sC` plural inModel
  `sC` plural likelyChg `sAnd` (plural requirement) +:+ S "on the" +:+ 
  plural assumption)]
  ([s9_table1, s9_table2, s9_table3] ++ (s9_intro2) ++ [fig_2, fig_3, fig_4])
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
s9_dataRef = [makeRef s6_2_5]

s9_funcReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
s9_funcReqRef = makeListRef s9_funcReq s7_1

s9_assump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"]
s9_assumpRef = makeListRef s9_assump s6_2_1

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
  (showingCxnBw (traceyMatrix) (titleize' requirement +:+ S "and Other" +:+
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
  (showingCxnBw (traceyMatrix) (titleize' assumption +:+ S "and Other"
  +:+ titleize' item)) True

--

s9_intro2 = traceGIntro [fig_2, fig_3, fig_4]
  [(plural requirement +:+ S "on" +:+ plural thModel `sC` plural inModel
  `sC` plural dataDefn +:+ S "and" +:+. plural datumConstraint), 
  (plural requirement +:+ S "on" +:+ plural thModel `sC` plural inModel
  `sC` (plural dataDefn) +:+ S "and" +:+. plural datumConstraint), 
  (plural thModel `sC` plural inModel `sC` plural dataDefn `sC`
  plural requirement `sAnd` plural likelyChg +:+ S "on" +:+
  plural assumption)]

fig_2 = figureLabel "2" (traceyMatrix)
  (titleize' item +:+ S "of Different" +:+ titleize' section_)
  ("Trace.png")

fig_3 = figureLabel "3" (traceyMatrix)
  (titleize' requirement +:+ S "and Other" +:+ titleize' item)
  ("RTrace.png")

fig_4 = figureLabel "4" (traceyMatrix)
  (titleize' assumption +:+ S "and Other" +:+ titleize' item)
  ("ATrace.png")

s10 = SRS.reference [s10_list] []

s10_list = mkRefsList 1 
  [(S "N. Koothoor" `sC` Quote (S "A" +:+ phrase document +:+ 
  S "drive approach to certifying" +:+ phrase sciCompS :+: S ",") +:+
  S "Master's thesis" `sC` S "McMaster University, Hamilton, Ontario, Canada, 2013."), 
  (S "W. S. Smith and L. Lai" `sC` Quote (S "A new" +:+ plural requirement +:+
  phrase template +:+ S "for scientific computing,") +:+ S "in Proceedings of the" +:+
  S "First International Workshop on Situational" +:+ titleize' requirement +:+ 
  S "Engineering Processes - Methods, Techniques and Tools to Support Situation-Specific" +:+
  titleize' requirement +:+ S "Engineering Processes, SREP'05 (J.Ralyt" :+: 
  (F Acute 'e') `sC` S "P.Agerfalk, and N.Kraiem, eds.), (Paris, France),"
  +:+ S "pp. 107-121, In conjunction with 13th IEEE International" +:+
  titleize' requirement +:+. S "Engineering Conference, 2005"), 
  (S "J. Robertson and S. Robertson" `sC` Quote (S "Volere" +:+
  plural requirement +:+ phrase specification +:+ phrase template +:+. S "edition 16") +:+ 
  Quote (S "www.cs.uic.edu/ i442/VolereMaterials/templateArchive16/c" +:+ 
  S "Volere template16.pdf") `sC` S "2012."), 
  (S "ASTM Standards Committee" `sC` Quote (S "Standard practice"
  +:+ S "for determining" +:+ (phrase load) +:+ S "resistance of" +:+
  S "glass in buildings,") +:+ 
  S "Standard E1300-09a, American Society for Testing and Material (ASTM),"
  +:+. S "2009"), 
  (S "ASTM, developed by subcommittee C1408, Book of standards 15.02,"
  +:+ Quote (S "Standard" +:+ phrase specification +:+. S "for flat glass, C1036")), 
  (S "ASTM, developed by subcommittee C14.08, Book of standards" +:+
  S "15.02" `sC` Quote (at_start specification +:+ S "for" +:+ (plural heat) +:+.
  S "treated flat glass-Kind HS, kind FT coated and uncoated glass, C1048"))]

s11 = SRS.appendix [s11_intro, fig_5, fig_6] []

s11_intro = foldlSP [
  S "This", phrase appendix, S "holds the", (plural graph), 
  sParen ((makeRef fig_5) `sAnd` (makeRef fig_6)), 
  S "used for interpolating", plural value, S "needed in the", plural model]

fig_5 = Figure (titleize figure +: S "5" +:+ (demandq ^. defn) +:+ sParen
  (getS demand) +:+ S "versus" +:+ (at_start sD) +:+
  S "versus" +:+ (at_start char_weight) +:+ sParen
  (getS sflawParamM)) "ASTM_F2248-09.png"

fig_6 = Figure (titleize figure +:+ S "6: Non dimensional" +:+ 
  (phrase lateral) +:+
  (phrase load) +:+ sParen
  (getS dimlessLoad) +:+ S "versus" +:+ (titleize aspectR) +:+ 
  sParen (short aspectR) {-(P (aspectR))-} +:+ S "versus" +:+
  (at_start stressDistFac) +:+ sParen (getS stressDistFac))
  "ASTM_F2248-09_BeasonEtAl.png"