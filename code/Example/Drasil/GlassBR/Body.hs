module Drasil.GlassBR.Body where
import Control.Lens ((^.))

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Authors
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Software.Products
import Data.Drasil.Concepts.Computation
import Data.Drasil.Concepts.Math (matrix, graph, calculation,
                                  equation, surface, probability,
                                  parameter)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Concepts.Thermodynamics (heat)
import Prelude hiding (id)
import Data.Drasil.Utils (foldlSent, mkEnumAbbrevList, itemRefToSent,
  makeTMatrix, makeListRef, refFromType)

import Drasil.Template.MG
import Drasil.Template.DD

import           Drasil.TableOfSymbols
import           Drasil.OrganizationOfSRS
import qualified Drasil.SRS as SRS
import           Drasil.ReferenceMaterial

import Drasil.GlassBR.Example
import Drasil.GlassBR.Concepts
import Drasil.GlassBR.Changes
import Drasil.GlassBR.Modules
import Drasil.GlassBR.Reqs
import Drasil.GlassBR.TMods
import Drasil.GlassBR.IMods
import Drasil.GlassBR.DataDefs

import Drasil.DocumentLanguage
import Drasil.OrganizationOfSRS (showingCxnBw, figureLabel)

this_si :: [UnitDefn]
this_si = map UU [metre, second] ++ map UU [pascal, newton]

s2, s2_1, s2_2, s2_3, s3, s3_1, s3_2, s4, s4_1, s4_2,
  s5, s5_1, s5_2, s6, s6_1, s6_1_1, s6_1_2, s6_1_3, s6_2, s6_2_1, s6_2_2, 
  s6_2_3, s6_2_4, s6_2_5, s7, s7_1, s7_2, s8, s9, s10, s11 :: Section 

s2_intro, s2_2_intro, 
  s3_1_intro, s3_2_intro, s4_1_bullets, s5_intro, 
  s5_1_table, s5_2_bullets, s6_1_intro, s6_1_1_intro, s6_1_1_bullets,
  s6_1_2_intro, s6_1_2_list, s6_1_3_list, s6_2_intro, s6_2_1_intro, 
  s6_2_5_table1, s6_2_5_table2, s6_2_5_intro2, s6_2_5_table3, 
  s7_2_intro, s8_list, s9_intro1, s9_table1, s9_table2, s9_table3,
  s10_list, s11_intro, fig_glassbr, fig_2, fig_3, fig_4, 
  fig_5, fig_6 :: Contents

s2_1_intro, s6_2_1_list, s7_1_list, s9_intro2 :: [Contents]

srs_authors, mg_authors, s2_3_intro_end, s2_3_intro :: Sentence
srs_authors = manyNames [nikitha, spencerSmith]
mg_authors = manyNames [spencerSmith, thulasi]

authors :: People
authors = [nikitha, spencerSmith]

glassBR_srs' :: Document
glassBR_srs' = mkDoc' mkSRS (for'' titleize phrase) glassSystInfo

--FIXME: Missing ToS intro because this example was using the default 
-- (nuclear literature related) intro.

mkSRS :: DocDesc 
mkSRS = RefSec (RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA]) :
  map Verbatim [s2, s3, s4, s5, s6, s7, s8, s9, s10, s11]
  
glassSystInfo :: SystemInformation
glassSystInfo = SI glassBRProg srs authors this_si this_symbols ([] :: [CQSWrapper])
  acronyms --FIXME: All named ideas, not just acronyms.

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

glassBR_mg :: Document
glassBR_mg = mgDoc'' glassBRProg (for'' titleize phrase) mg_authors mgBod

this_symbols :: [QSWrapper]
this_symbols = ((map qs glassBRSymbolsWithDefns) ++ (map qs glassBRSymbols)
  ++ (map qs glassBRUnitless))

s2 = SRS.intro [s2_intro] [s2_1, s2_2, s2_3]

s2_intro = Paragraph $ foldlSent [(at_start software), 
  S "is helpful to efficiently and correctly predict the", 
  (phrase $ blastRisk ^. term), S "involved with the" +:+. 
  (phrase $ glaSlab ^. term), S "The", (phrase $ blast ^. term), 
  S "under consideration is" +:+. (blast ^. defn), S "The", phrase software `sC` 
  S "herein called", (gLassBR ^. defn), S "aims to predict the", 
  (phrase $ blastRisk ^. term), S "involved with the", 
  (phrase $ glaSlab ^. term), S "using an intuitive", 
  S "interface. The following", phrase section_, S "provides an overview", 
  S "of the", titleize srs, sParen (short srs), S "for" +:+.
  (gLassBR ^. defn), S "This", phrase section_, S "explains the", 
  phrase purpose, S "of the", phrase document, S "is designed to fulfil, the", 
  phrase scope, S "of the", plural requirement, S "and the", 
  phrase organization, S "of the" +: phrase document, S "what the", 
  phrase document, S "is based on and intended to portray"]

s2_1 = SRS.prpsOfDoc (s2_1_intro) []

s2_1_intro = [s2_1_intro_p1, s2_1_intro_p2] 

s2_1_intro_p1, s2_1_intro_p2 :: Contents

s2_1_intro_p1 = Paragraph $ foldlSent [S "The main", phrase purpose, S "of this", 
  phrase document, S "is to predict whether a given", (phrase $ glaSlab ^. term),
  S "is likely to resist a specified" +:+. (phrase $ blast ^. term),
  S "The", plural goal, S "and", plural thModel, S "used in the", (gLassBR ^. defn),
  S "code are provided" `sC` S "with an emphasis on explicitly identifying", 
  (plural assumption), S "and unambiguous" +:+. plural definition,
  S "This", phrase document, S "is intended to be used as a", phrase reference,
  S "to provide all", phrase information, S "necessary to understand and",
  S "verify the" +:+. phrase analysis, S "The", (short srs), S "is abstract",
  S "because the", plural content, S "say what", phrase problem, 
  S "is being solved, but not how to solve it"]

s2_1_intro_p2 = Paragraph $ foldlSent [S "This", phrase document, 
  S "will be used as a starting point for subsequent development", 
  S "phases, including writing the", phrase desSpec, S "and the", 
  phrase softwareVAV, S "plan. The", phrase designDoc,
  S "will show how the", plural requirement, S "are to be realized, including",
  plural decision, S "on the numerical", (plural $ algorithm ^. term), 
  S "and programming" +:+. phrase environment, S "The", phrase vavPlan, 
  S "will show the steps that will be used to increase confidence in the",
  phrase softwareDoc, S "and the", phrase implementation]

s2_2 = SRS.scpOfReq [s2_2_intro] []

s2_2_intro = Paragraph $ foldlSent [S "The", phrase scope, S "of the",
  plural requirement, S "includes getting all", phrase input_, 
  (plural $ parameter ^. term), S "related to the", (phrase $ glaSlab ^. term),
  S "and also the", (plural $ parameter ^. term), S "related to" +:+. 
  (phrase $ blastTy ^. term), S "Given the", phrase input_ `sC` (gLassBR ^. defn),
  S "is intended to use the", plural datum, S "and predict whether the",
  (phrase $ glaSlab ^. term), S "is safe to use or not"]

s2_3 = orgSecWTS s2_3_intro dataDefn s6_2_4 s2_3_intro_end

s2_3_intro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the", phrase template, S "for an", (short srs),
  S "for", phrase sciCompS, S "proposed by [1] and [2] (in", (makeRef s10) :+:
  S "), with some aspects taken from Volere", phrase template, S "16 [3]"]
  
s2_3_intro_end = foldl (+:+) EmptyS [(at_start' $ the dataDefn),
  S "are used to support the", plural definition, S "of the different",
  plural model]
  
s3 = stakehldr [s3_1, s3_2]

s3_1 = SRS.theClient [s3_1_intro] []

s3_1_intro = Paragraph $ foldlSent [(at_start $ the client), S "for",
  (gLassBR ^. defn), S "is a", phrase company, S "named" +:+.
  S "Entuitive. It is developed by Dr. Manuel Campidelli", 
  (at_start $ the client), S "has the final say on acceptance of the", 
  phrase product_]

s3_2 = SRS.theCustomer [s3_2_intro] []

s3_2_intro = Paragraph $ foldlSent [(at_start' $ the customer), 
  S "are the end", phrase user, S "of", (gLassBR ^. defn)]

s4 = genSysF [s4_1, s4_2]

s4_1 = SRS.userChar [s4_1_bullets] []

s4_1_bullets = Enumeration $ Bullet $ map Flat
  [(S "The end" +:+ phrase user +:+ S "of" +:+ (gLassBR ^. defn) +:+ 
  S "is expected to have completed at least the equivalent of the second year of an" +:+
  S "undergraduate degree in civil or structural engineering"),
  (S "The end" +:+ phrase user +:+ S "is expected to have an understanding of" +:+
  phrase theory +:+ S "behind" +:+ (phrase $ gbr ^. term) +:+ S "and" +:+ (phrase $ blastRisk ^. term)),
  (S "The end" +:+ phrase user +:+ S "is expected to have basic" +:+ phrase computer +:+
  S "literacy to handle the" +:+. phrase software)]

s4_2 = systCon Nothing []

s5 = SRS.scpOfTheProj [s5_intro] [s5_1, s5_2]

s5_intro = Paragraph $ foldlSent [S "This", phrase section_,
  S "presents the" +:+. phrase (scpOfTheProj phrase),
  S "It describes the expected use of", (gLassBR ^. defn), 
  S "as well as the", plural input_, S "and", plural output_, 
  S "of each action. The", plural useCase, S "are", phrase input_, S "and",
  phrase output_ `sC` S "which defines the action of getting the", phrase input_,
  S "and displaying the", phrase output_]

s5_1 = SRS.prodUCTable [s5_1_table] []

s5_1_table = Table [titleize useCase +:+. S "NO", titleize useCase +:+
  titleize name_, S "Actor", titleize input_ +:+ S "and" +:+ titleize output_]
  (mkTable
  [(\x -> (x!!0)),(\x -> (x!!1)), (\x -> (x!!2)), (\x -> (x!!3))]
  [[S "1", titleize' input_, titleize user, titleize' characteristic +:+
  S "of the" +:+ (phrase $ glaSlab ^. term) +:+ S "and of the" +:+.
  (phrase $ blast ^. term) +:+ S "Details in" +:+ (makeRef s5_2)],
  [S "2", titleize output_, (gLassBR ^. defn), S "Whether or not the" +:+
  (phrase $ glaSlab ^. term) +:+ S "is safe for the calculated" +:+
  (phrase $ load ^. term) +:+ S "and supporting" +:+
  S "calculated" +:+ plural value]]) 
  (titleize table_ +: S "1" +:+ titleize useCaseTable) True

s5_2 = SRS.indPRCase [s5_2_bullets] []

s5_2_bullets = Enumeration $ Bullet $ map Flat [s5_2_bt_sent1, s5_2_bt_sent2]

s5_2_bt_sent1 :: Sentence
s5_2_bt_sent1 = foldlSent [titleize useCase, S "1 refers to the", phrase user, 
  S "providing", phrase input_, S "to", (gLassBR ^. defn),
  S "for use within the" +:+. phrase analysis, S "There are two classes of"
  +: plural input_, (phrase $ glassGeo ^. term), S "and" +:+.
  (phrase $ blastTy ^. term), (glassGeo ^. defn), (blastTy ^. defn), S "These",
  (plural $ parameter ^. term), S "describe", (phrase $ char_weight ^. term),
  S "and stand off" +:+. (phrase $ blast ^. term), S "Another", phrase input_,
  S "the", phrase user, S "gives is the tolerable", phrase value, S "of",
  (phrase $ prob_br ^. term)]

s5_2_bt_sent2 :: Sentence
s5_2_bt_sent2 = foldlSent [S " Use Case 2", (gLassBR ^. defn), 
  plural output_, S "if the", (phrase $ glaSlab ^. term), S "will be safe by", 
  S "comparing whether", (phrase $ capacity ^. term), S "is greater than" +:+. 
  (phrase $ demandq ^. term), (at_start $ capacity ^. term), S "is the",
  (capacity ^. defn), S "and", (phrase $ demandq ^. term), S "is the", 
  phrase requirement, S "which is the" +:+. (demandq ^. defn), S "The second", 
  phrase condition, S "is to check whether the calculated", 
  (phrase $ probability ^. term), sParen (P $ prob_br ^. symbol), 
  S "is less than the tolerable", (phrase $ probability ^. term), 
  sParen (P $ pb_tol ^. symbol), S "which is obtained from the", phrase user,
  S "as an" +:+. phrase input_, S "If both", plural condition, 
  S "return true then it's shown that the", (phrase $ glaSlab ^. term), 
  S "is safe to use" `sC` S "else if both return false then the", 
  (phrase $ glaSlab ^. term) +:+. S "is considered unsafe",
  S "All the supporting calculated", plural value, S "are also displayed as",
  phrase output_]

s6 = specSysDesF (S "and" +:+ plural definition) [s6_1, s6_2]

s6_1 = SRS.probDesc [s6_1_intro] [s6_1_1, s6_1_2, s6_1_3]

s6_1_intro = Paragraph $ foldlSent [S "A", phrase system,
  S "is needed to efficiently and correctly predict the", 
  (phrase $ blastRisk ^. term) +:+. S "involved with the glass", (gLassBR ^. defn),
  S "is a", phrase computer, (phrase $ program ^. term), 
  S "developed to interpret the", plural input_, S "to give out the",
  plural output_, S "which predicts whether the", (phrase $ glaSlab ^. term), 
  S "can withstand the", (phrase $ blast ^. term), S "under the",
  plural condition]

s6_1_1 = SRS.termAndDefn [s6_1_1_intro, s6_1_1_bullets] []
  
s6_1_1_intro = Paragraph $ foldlSent [
  S "This subsection provides a list of terms that are used in subsequent",
  plural section_, S "and their meaning, with the", phrase purpose,
  S "of reducing ambiguity and making it easier to correctly understand the" +:+. 
  (plural requirement), S "All of the terms are extracted from [4] in",
  (makeRef s10)]

s6_1_1_bullets = Enumeration $ (Number $ 
  [Flat $ ((at_start $ aspectR ^. term) :+: sParenDash (short aspectR)) {-P $ aspectR ^. symbol))-} :+: -- conceptually correct to call abbreviation as a symbol?
  (aR ^. defn)] ++
  map (\c -> Flat $ ((at_start $ c ^. term) +:+ S "- ") :+: (c ^. defn))
  [gbr, lite] ++ [Nested (((titleize $ glassTy ^. term) :+: S ":")) 
  (Bullet $ map (\c -> Flat c) [(((at_start $ an ^. term) :+: 
    sParenDash (short annealedGlass)) :+: (an ^. defn)),
  (((at_start $ ft ^. term) :+: sParenDash (short fullyTGlass)) :+:
    (ft ^. defn)),
  (((at_start $ hs ^. term) :+: sParenDash (short heatSGlass)) :+:
    (hs ^. defn))])] ++
  map (\c -> Flat c)
  [(((at_start $ gtf ^. term) :+: sParenDash (short glassTypeFac)) :+: 
  (gtf ^. defn)),
  (((at_start $ lateral ^. term) +:+ S "- ") :+: (lateral ^. defn))] ++ 
  [Nested (((at_start $ load ^. term) :+: S ":")) (Bullet $ map (\c -> Flat c)  
  [(((at_start $ specDeLoad ^. term) +:+ S "- ") :+: (specDeLoad ^. defn)),
  (((at_start $ lr ^. term) :+: sParenDash (short lResistance)) :+:
    (lr ^. defn)),
  (((at_start $ ldl ^. term) +:+ S "- ") :+: (ldl ^. defn)),
  (((at_start $ nonFL ^. term) +:+ sParen (P $ nonFL ^. symbol)) +:+ S "-" 
    +:+ (nfl ^. defn))] ++ 
  map (\c -> Flat $ ((at_start $ c ^. term) +:+ S "- ") :+: (c ^. defn))
    [glassWL, sdl])] ++ 
  map (\c -> Flat c)
  [(((at_start $ lsf ^. term) :+: sParenDash (short lShareFac)) :+: 
  (lsf ^. defn)),
  (((at_start $ pb ^. term) :+: sParenDash (P $ prob_br ^. symbol)) :+:
  (pb ^. defn))] ++
  map (\c -> Flat $ ((at_start $ c ^. term) +:+ S "- ") :+: (c ^. defn)) 
  [specA, blaReGLa, eqTNTChar] ++
  [Flat $ ((at_start $ sD ^. term) :+: sParenDash (P $ sd ^. symbol)) :+:
  (sD ^. defn)])
  where sParenDash = \x -> S " (" :+: x :+: S ") - "
  
s6_1_2 = SRS.physSyst [s6_1_2_intro, s6_1_2_list, 
  fig_glassbr] []

s6_1_2_intro = Paragraph $ S "The" +:+ phrase physicalSystem +:+ S "of" +:+ 
  (gLassBR ^. defn) +:+ S "as shown in" +:+ (makeRef fig_glassbr) `sC` 
  S "includes the following" +: plural element

fig_glassbr = Figure (at_start $ the physicalSystem) "physicalsystimage.png"
  
s6_1_2_list = Enumeration $ Simple $ mkEnumAbbrevList 1 (short physSyst) 
  s6_1_2_list_physys1

s6_1_2_list_physys1 :: [Sentence]
s6_1_2_list_physys1 = [(at_start $ glaSlab ^. term), (foldlSent [S "The point of"
  +:+. (phrase $ explosion ^. term), S "Where the", (phrase $ bomb ^. term) `sC` 
  S "or", (blast ^. defn) `sC` S "is located. The", (phrase $ sD ^. term), 
  S "is the distance between the point of", (phrase $ explosion ^. term), 
  S "and the glass"])]

s6_1_3 = SRS.goalStmt [s6_1_3_list] []

s6_1_3_list = Enumeration $ Simple $ mkEnumAbbrevList 1 (short goalStmt) 
  s6_1_3_list_goalStmt1

s6_1_3_list_goalStmt1 :: [Sentence]
s6_1_3_list_goalStmt1 = [foldlSent [S "Analyze and predict whether the",
  (phrase $ glaSlab ^. term), S "under consideration",
  S "will be able to withstand the", (phrase $ explosion ^. term),
  S "of a certain degree which is calculated based on", phrase user, 
  phrase input_]]

s6_2 = SRS.solCharSpec
  [s6_2_intro] [s6_2_1, s6_2_2, s6_2_3, s6_2_4, s6_2_5]

s6_2_intro = Paragraph $ foldlSent [S "This", phrase section_, 
  S "explains all the", (plural assumption), S "considered and the",
  plural thModel, S "which are supported by the", (plural dataDefn)]
  
s6_2_1 = SRS.assump ([s6_2_1_intro] ++ (s6_2_1_list)) []

s6_2_1_intro = Paragraph $ foldlSent [
  S "This", phrase section_, S "simplifies the original", phrase problem, 
  S "and helps in developing the", (phrase thModel), S "[" :+: 
  (short thModel) :+: S "] by filling in the missing", phrase information,
  S "for the" +:+. phrase physicalSystem, S "The numbers given in the",
  S "square brackets refer to the", (phrase dataDefn), S "[" :+:
  (short dataDefn) :+: S "], or", phrase inModel, S "[" :+: 
  (short inModel) :+: S "], in which the respective", (phrase assumption),
  S "is used"]

s6_2_1_list = 
  [(Enumeration $ Simple $ mkEnumAbbrevList 1 (short assumption) 
    s6_2_1_list_assum1),
  (EqnBlock $ (C sflawParamM):=(Int 7)),
  (EqnBlock $ (C sflawParamK):=(Grouping (Dbl 2.86)):*(Int 10):^
    (Neg (Int 53))),
  (EqnBlock $ (C mod_elas):=(Grouping (Dbl 7.17)):*(Int 10):^(Int 7)),
  (EqnBlock $ (C load_dur):=(Int 3)),
  --  (Number $ map (\c -> Flat c) [
  --  (P $ sflawParamM ^. symbol) +:+ S "= 7" +:+ Sy (sflawParamM ^. unit), 
  --  (P $ sflawParamK ^. symbol) +:+ S "= 2.86 * 10^(-53)" +:+ Sy (sflawParamK ^. unit), 
  --  (P $ mod_elas ^. symbol) +:+ S "= 7.17 * 10^7" +:+ Sy (mod_elas ^. unit),
  --  (P $ load_dur ^. symbol) +:+ S "= 3" +:+ Sy (load_dur ^. unit)]))] ++
  (Enumeration $ Simple $ mkEnumAbbrevList 5 (short assumption) s6_2_1_list_assum2)]
  --equation in sentence

s6_2_1_list_assum1 :: [Sentence]
s6_2_1_list_assum1 = [foldlSent [S "The standard E1300-09a for", 
  (phrase $ calculation ^. term),
  S "applies only to monolithic, laminated, or insulating",
  S "glass constructions of rectangular shape with continuous", 
  (phrase $ lateral ^. term),
  S "support along one, two, three, or four edges. This practice assumes", 
  S "that (1) the supported glass edges for two, three and four-sided", 
  S "support", plural condition, S "are simply supported and free to slip in",
  S "plane; (2) glass supported on two sides acts as a simply supported", 
  S "beam and (3) glass supported on one side acts as a cantilever"], 
  foldlSent [S "Following [4 (pg. 1)], this practice does not apply", 
  S "to any form of wired, patterned, etched, sandblasted, drilled" `sC`
  S "notched, or grooved glass with", (phrase $ surface ^. term), S "and edge",
  S "treatments that alter the glass strength"],
  foldlSent [S "This", phrase system, S "only considers the external", 
  (phrase $ explosion ^. term), S "scenario for its", 
  (plural $ calculation ^. term)],
  (S "Standard" +:+ plural value +:+ S "used for" +:+
  (phrase $ calculation ^. term) +:+ S "in" +:+ (gLassBR ^. defn) +: S "are")]

s6_2_1_list_assum2 :: [Sentence]
s6_2_1_list_assum2 = [(foldlSent [S "Glass under consideration", 
  S "is assumed to be a single" +:+. (phrase $ lite ^. term), S "Hence the",
  phrase value, S "of", (short lShareFac), S "is equal to 1 for all", 
  (plural $ calculation ^. term), S "in", (gLassBR ^. defn)]),
  (foldlSent [S "Boundary", plural condition, S "for the", 
  (phrase $ glaSlab ^. term), S "is assumed to be 4-sided",
  S "support for", (plural $ calculation ^. term)]),
  (foldlSent [S "The response type considered in", (gLassBR ^. defn), 
  S "is flexural"]),
  (foldlSent [S "With", phrase reference, S "to A4 the", phrase value, 
  S "of", (phrase $ loadDF ^. term), sParen (P $ loadDF ^. symbol), 
  S "is a constant in" +:+. (gLassBR ^. defn), S "It is calculated by the" +: 
  (phrase $ equation ^. term), --(P $ loadDF ^. symbol) +:+ S "=" +:+ (P $ load_dur ^. symbol) :+: 
  S ". Using this" `sC` (P $ loadDF ^. symbol), S "= 0.27"])]

s6_2_2 = thModF (S "GlassBR") (s6_2_2_TMods) 
-- update thModF to receive NP argument (gLassBR ^. term) i/of Sentence?
-- is intro generic enough to be used by GlassBR?
  
s6_2_2_TMods :: [Contents]
s6_2_2_TMods = map Definition (map Theory tModels)

s6_2_3 = inModelF' s6_1 s6_2_4 s6_2_2 (s6_2_3_IMods)

s6_2_3_IMods :: [Contents]
s6_2_3_IMods = map Definition (map Theory iModels)

s6_2_4 = dataDefnF EmptyS (s6_2_4_DDefns)

s6_2_4_DDefns ::[Contents] 
s6_2_4_DDefns = map Definition (map Data dataDefns)

s6_2_5 = datConF ((makeRef s6_2_5_table1) +:+ S "shows") EmptyS True end [s6_2_5_table1, s6_2_5_table2, s6_2_5_intro2] --issue #213: discrepancy?
  where end = foldlSent [(makeRef s6_2_5_table1), S "gives the", plural value, 
              S "of the specification", (plural $ parameter ^. term),
              S "used in" +:+. (makeRef s6_2_5_table1), (P $ ar_max ^. symbol), --FIXME: Issue #167
              S "refers to the", (phrase $ ar_max ^. term), S "for the plate of glass"]

s6_2_5_table1 = Table [S "Var", S "Physical Cons", S "Software Constraints", S "Typical Value",
  S "Uncertainty"] (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2), (\x -> x!!3),
  (\x -> x!!4)] [[(P $ plate_len ^. symbol), (P $ plate_len ^. symbol) +:+ S "> 0 and" +:+ 
  (P $ plate_len ^. symbol) :+: S "/" :+: (P $ plate_width ^. symbol) +:+ S "> 1",
  (P $ dim_min ^. symbol) +:+ S "<=" +:+ (P $ plate_len ^. symbol) +:+ S "<=" +:+ 
  (P $ dim_max ^. symbol) +:+ S "and" +:+ (P $ plate_len ^. symbol) :+: S "/" :+: 
  (P $ plate_width ^. symbol) +:+ S "<" +:+ (P $ ar_max ^. symbol), S "1500" +:+
  Sy (unit_symb plate_len), S "10%"], [(P $ plate_width ^. symbol), (P $ (plate_width ^. symbol)) 
  +:+ S "> 0 and" +:+ (P $ plate_width ^. symbol) +:+ S "<" +:+ (P $ plate_len ^. symbol),
  (P $ dim_min ^. symbol) +:+ S "<=" +:+ (P $ plate_width ^. symbol) +:+ S "<=" +:+ 
  (P $ dim_max ^.symbol) +:+ S "and" +:+ (P $ plate_len ^. symbol) :+: S "/" :+: 
  (P $ plate_width ^. symbol) +:+ S "<" +:+ (P $ ar_max ^. symbol), S "1200" +:+ 
  Sy (unit_symb plate_width), S "10%"], [(P $ pb_tol ^. symbol), S "0 <" +:+ 
  (P $ pb_tol ^. symbol) +:+ S "< 1", S "-", S "0.008", S "0.1%"], [(P $ char_weight ^. symbol), 
  (P $ char_weight ^. symbol) +:+ S ">= 0", (P $ cWeightMin ^. symbol) +:+ S "<" +:+ 
  (P $ char_weight ^. symbol) +:+ S "<" +:+ (P $ cWeightMax ^. symbol), S "42" +:+ 
  Sy (unit_symb char_weight), S "10%"],[(P $ tNT ^. symbol), (P $ tNT ^. symbol) :+: 
  S " > 0", S "-", S "1", S "10%"], [(P $ sd ^. symbol), (P $ sd ^. symbol) +:+ S "> 0", 
  (P $ sd_min ^. symbol) +:+ S "<" +:+ (P $ sd ^. symbol) +:+ S "<" +:+ 
  (P $ sd_max ^. symbol), S "45" :+: Sy (unit_symb sd), S "10%"]])
  (S "Table 2: Input Variables") True

s6_2_5_table2 = Table [S "Var", titleize value] (mkTable 
  [(\x -> fst x), (\x -> snd x)] 
  [(P $ dim_min ^. symbol, S "0.1" +:+ Sy (unit_symb sd)), 
  (P $ dim_max ^.symbol, S "0.1" +:+ Sy (unit_symb sd)),
  ((P $ ar_max ^. symbol), S "5"), (P $ cWeightMin ^. symbol, S "4.5" +:+
  Sy (unit_symb cWeightMin)), (P $ cWeightMax ^. symbol, S "910" +:+ 
  Sy (unit_symb cWeightMax)), (P $ sd_min ^. symbol, S "6" +:+
  Sy (unit_symb sd_min)), (P $ sd_max ^. symbol, S "130" +:+
  Sy (unit_symb sd_max))])
  (titleize table_ +: S "3" +:+ titleize specification +:+
  (titleize $ parameter ^. term) +:+ titleize' value) True

s6_2_5_intro2 = Paragraph $ foldlSent [(makeRef s6_2_5_table3), S "shows the",
  plural constraint, S "that must be satisfied by the", phrase output_]

s6_2_5_table3 = Table [S "Var", S "Physical Constraints"] (mkTable 
  [(\x -> P $ fst(x)), (\x -> snd(x))] 
  [(prob_br ^. symbol, S "0 <" +:+ (P $ prob_br ^. symbol) +:+ S "< 1")])
  (S "Table 4: Output Variables") True

s7 = reqF [s7_1, s7_2]

s7_1 = SRS.funcReq (s7_1_list) []

s7_1_list = 
  [(Enumeration $ Simple $ map (\(a, b) -> (a, Flat b))
  [(((short requirement) :+: S "1"), at_start input_ +:+ S "the following" +:+
    plural quantity `sC` S "which define the glass dimensions" `sC` 
    (glassTy ^. defn) `sC` S "tolerable" +:+ (phrase $ probability ^. term) +:+
    S "of failure and the" +:+ plural characteristic +:+ S "of the" +:
    (phrase $ blast ^. term))]),
  (table ((map qs [plate_len, plate_width, sdx, sdy, sdz, nom_thick, char_weight]) 
  ++ (map qs [glass_type, pb_tol, tNT])) (\x -> at_start $ x ^.term)),
--s7_1_table = Table [S "Symbol", S "Units", S "Description"] (mkTable
--  [(\ch -> P (ch ^. symbol)),  
--   (\ch -> maybeUnits $ ch ^. unit'),
--   (\ch -> ch ^. term)
--   ]
--  [plate_len,plate_width,glass_type,pb_tol,sdx,sdy,sdz,nom_thick,tNT,
--  char_weight])
--  (S "Input Parameters") False
  (Enumeration $ Simple $
  [(((short requirement) :+: S "2"), Nested (S "The" +:+ phrase system +:+
  S "shall set the known" +:+ plural value +:+ S "as follows: ")
   (Bullet $ map (\c -> Flat c) 
    [(P $ sflawParamM ^. symbol) `sC` (P $ sflawParamK ^. symbol) `sC` 
    (P $ mod_elas ^. symbol) `sC` (P $ load_dur ^. symbol) +:+ 
    S "following" +:+ (short assumption) :+: S "4",
    (P $ loadDF ^. symbol) +:+ S "following" +:+ (short assumption) 
    :+: S "8",
    (short lShareFac) +:+ S "following" +:+ (short assumption) 
    :+: S "5"]))] ++
  map (\(a, b) -> (a, Flat b))
  [(((short requirement) :+: S "3"), S "The" +:+ phrase system +:+
  S "shall check the entered" +:+ phrase input_ +:+ plural value +:+ 
  S "to ensure that they do not exceed the"  +:+ plural datumConstraint +:+
  S "mentioned in" +:+. (makeRef s6_2_5) +:+ S "If any of" +:+
  S "the" +:+ phrase input_ +:+ (plural $ parameter ^. term) +:+
  S "is out of bounds, an error message is displayed and the" +:+
  (plural $ calculation ^. term) +:+. S "stop"), (((short requirement)
  :+: S "4"), titleize output_ +:+ S "the" +:+ phrase input_ +:+
  plural quantity +:+ S "from" +:+ (short requirement) :+: S "1 and the known"
  +:+ plural quantity +:+ S "from" +:+ (short requirement) :+: S "2."),
  (((short requirement) :+: S "5"), S "If" +:+ (P $ is_safe1 ^. symbol)
  +:+ S "and" +:+ (P $ is_safe2 ^. symbol) +:+ S "(from" +:+ 
  (makeRef (Definition (Theory t1SafetyReq))) +:+ S "and" +:+ 
  (makeRef (Definition (Theory t2SafetyReq))) :+: S ") are true" `sC`
  phrase output_ +:+ S "the message" +:+ Quote (safeMessage ^. defn) +:+
  S "If the" +:+ phrase condition +:+ S "is false, then" +:+ phrase output_ +:+
  S "the message" +:+ Quote (notSafe ^. defn))] ++ 
  [(((short requirement) :+: S "6"), Nested (titleize output_ +:+ S "the following" +:
  plural quantity)
  (Bullet $ 
    [Flat $ (at_start $ prob_br ^. term) +:+ sParen (P $ prob_br ^. symbol) +:+ 
    sParen (makeRef (Definition (Theory probOfBr)))] ++
    [Flat $ (titleize $ lResistance ^. term) +:+ sParen(short lResistance) +:+ 
    sParen (makeRef (Definition (Theory calOfCap)))] ++
    [Flat $ (at_start $ demand ^. term) +:+ sParen (P $ demand ^. symbol) +:+
    sParen (makeRef (Definition (Theory calOfDe)))] ++
    [Flat $ (at_start $ act_thick ^. term) +:+ sParen(P $ act_thick ^. symbol) +:+
    sParen (makeRef (Definition (Data hFromt)))] ++
    [Flat $ (titleize $ loadDF ^. term) +:+ sParen (P $ loadDF ^. symbol) +:+ 
    sParen (makeRef (Definition (Data loadDF)))]++
    [Flat $ (at_start $ strDisFac ^. term) +:+ sParen (P $ strDisFac ^. symbol) +:+ 
    sParen (makeRef (Definition (Data strDisFac)))]++
    [Flat $ (titleize $ nonFL ^. term) +:+ sParen (P $ nonFL ^. symbol) +:+ 
    sParen (makeRef (Definition (Data nonFL)))]++
    [Flat $ (titleize $ glassTypeFac ^. term) +:+ sParen(short glassTypeFac) +:+ 
    sParen (makeRef (Definition (Data glaTyFac)))] ++
    map (\c -> Flat $ (at_start $ c ^. term) +:+ sParen (P $ c ^. symbol) +:+ 
    sParen (makeRef (Definition (Data c))))
    [dL, tolPre, tolStrDisFac] ++
    [Flat $ (titleize $ aspectR ^. term) +:+ sParen(short aspectR {-P $ aspectR ^. symbol-})  
    --S " = a/b)"
    ]))])]

s7_2 = SRS.nonfuncReq [s7_2_intro] []

s7_2_intro = Paragraph $ foldlSent [
  S "Given the small size, and relative simplicity, of this", 
  phrase problem `sC` phrase performance, S "is not a" +:+. phrase priority +:+.
  S "Any reasonable", phrase implementation, S "will be very quick and use minimal storage", 
  S "Rather than", phrase performance `sC` S "the", phrase priority,
  phrase nonfunctional, (short requirement) :+:
  S "s are correctness, verifiability, understandability, reusability,", 
  S "maintainability and portability"]

s8 = SRS.likeChg [s8_list] []

s8_list = Enumeration $ Simple $ map (\(a, b) -> (a, Flat b))
  [(((short likelyChg) :+: S "1"), ((short assumption) :+: 
  S "3 - The" +:+ phrase system +:+
  S "currently only calculates for external" +:+. (phrase $ blastRisk ^. term)
  +:+. (S "In the future" +:+ (plural $ calculation ^. term) +:+
  S "can be added for the internal" +:+ (phrase $ blastRisk ^. term)))),
  (((short likelyChg) :+: S "2"), ((short assumption) :+:
  S "4" `sC` (short assumption) :+: S "8 - Currently the" +:+ plural value +:+
  S "for" +:+ (P $ sflawParamM ^. symbol) `sC` (P $ sflawParamK ^. symbol) `sC`
  S "and" +:+ (P $ mod_elas ^. symbol) +:+ S "are assumed to be the"
  +:+ S "same for all glass. In the future these" +:+ plural value +:+ 
  S "can be changed to" +:+ phrase variable +:+. plural input_)),
  (((short likelyChg) :+: S "3"), ((short assumption ) :+: 
  S "5 - The" +:+ phrase software +:+
  S "may be changed to accommodate more than a single" +:+. 
  (phrase $ lite ^. term))), (((short likelyChg) :+: S "4"),
  ((short assumption) :+: S "6 - The" +:+ phrase software +:+
  S "may be changed to accommodate more boundary" +:+ plural condition +:+.
  S "than 4-sided support")), (((short likelyChg) :+: S "5"), 
  ((short assumption) :+: S "7 - The" +:+ phrase software +:+.
  S "may be changed to consider more than just flexure of the glass"))]

s9 = SRS.traceyMandG ([s9_intro1, s9_table1, s9_table2, s9_table3] 
  ++ (s9_intro2) ++ [fig_2, fig_3, fig_4]) []
--s9 = traceMGF s9_table1 s9_table2 s9_table3 ([s9_table1, s9_table2, s9_table3] 
--  ++ (s9_intro2) ++ [fig_2, fig_3, fig_4]) []

s9_intro1 = Paragraph $ foldlSent [
  S "The", phrase purpose, S "of the", (plural traceyMatrix),
  S "is to provide easy", plural reference, S "on what has to be additionally",
  S "modified if a certain", phrase component, S "is changed. Every time a", 
  phrase component, S "is changed, the", plural item, S "in the", phrase column,
  S "of that", phrase component, S "that are marked with an", Quote (S "X") +:+. 
  S "should be modified as well", at_start table_, S "5", 
  sParen (makeRef s9_table1), S "shows the", plural dependency, S "of",
  plural thModel `sC` (plural dataDefn), S "and", plural inModel +:+.
  S "with each other", titleize table_, S "6", sParen (makeRef s9_table2),
  S "shows the", plural dependency, S "of", plural requirement, S "on", 
  plural thModel `sC` (plural inModel) `sC` (plural dataDefn),
  S "and" +:+. plural datumConstraint, titleize table_, S "7",
  sParen (makeRef s9_table3), S "shows the", plural dependency, S "of",
  plural thModel `sC` (plural dataDefn) `sC` plural inModel `sC`
  plural likelyChg, S "and", (plural requirement), S "on the",
  (plural assumption)]

--FIXME: There has to be a better way to do this.

s9_theorys, s9_instaModel, s9_dataDef, s9_data, s9_funcReq, s9_assump, 
  s9_likelyChg :: [String]

s9_theorysRef, s9_instaModelRef, s9_dataDefRef, s9_dataRef, s9_funcReqRef, 
  s9_assumpRef, s9_likelyChgRef :: [Sentence]

s9_theorys = ["T1", "T2"]
s9_theorysRef = map (refFromType Theory) tModels

s9_instaModel = ["IM1", "IM2", "IM3"]
s9_instaModelRef = map (refFromType Theory) iModels

s9_dataDef =  ["DD1", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8", "DD9"]
s9_dataDefRef = map (refFromType Data) dataDefns


s9_data  = ["Data Constraint"]
s9_dataRef = [makeRef s6_2_5]

s9_funcReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
s9_funcReqRef = makeListRef s9_funcReq s7_1

s9_assump = ["AS1", "AS2", "AS3", "AS4", "AS5", "AS6", "AS7", "AS8"]
s9_assumpRef = makeListRef s9_assump s6_2_1

s9_likelyChg = ["LC1", "LC2", "LC3", "LC4", "LC5"]
s9_likelyChgRef = makeListRef s9_likelyChg s8

s9_row_t1 :: [String]
s9_row_t1 = s9_theorys ++ s9_instaModel ++ s9_dataDef

-- The headers for the first row, and column
s9_row_header_t1 :: [Sentence]
s9_row_header_t1 = zipWith itemRefToSent s9_row_t1 (s9_theorysRef ++ 
  s9_instaModelRef ++ s9_dataDefRef)

-- list of columns and there rows for traceability matrix
s9_columns_t1 :: [[String]]
s9_columns_t1 = [s9_t1_T1, s9_t1_T2, s9_t1_IM1, s9_t1_IM2, s9_t1_IM3, s9_t1_DD1, 
  s9_t1_DD2, s9_t1_DD3, s9_t1_DD4, s9_t1_DD5, s9_t1_DD6, s9_t1_DD7, s9_t1_DD8, 
  s9_t1_DD9]

s9_t1_T1, s9_t1_T2, s9_t1_IM1, s9_t1_IM2, s9_t1_IM3, s9_t1_DD1, s9_t1_DD2, 
  s9_t1_DD3, s9_t1_DD4, s9_t1_DD5, s9_t1_DD6, s9_t1_DD7, s9_t1_DD8, 
  s9_t1_DD9 :: [String]
-- list of each item that "this" item requires for traceability matrix
s9_t1_T1  = ["T2","IM1"]
s9_t1_T2  = ["T1","IM2","IM3"]
s9_t1_IM1 = ["DD1","DD2","DD3","DD4"]
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
s9_t1_DD9 = ["DD2","DD3"]

s9_table1 = Table (EmptyS:s9_row_header_t1) 
  (makeTMatrix s9_row_header_t1 s9_columns_t1 s9_row_t1)
  (showingCxnBw (traceyMatrix) (titleize' item +:+ S "of Different" +:+ titleize' section_)) True

s9_row_t2 :: [String]
s9_row_t2 = s9_row_t1 ++ s9_data ++ s9_funcReq

s9_row_header_t2, s9_col_header_t2 :: [Sentence]
s9_row_header_t2 = s9_row_header_t1 ++ (zipWith itemRefToSent (s9_data ++ s9_funcReq) (s9_dataRef ++ s9_funcReqRef))

s9_col_header_t2 = map (\(x,y) -> S x +:+ sParen (S "in" +:+ y)) (zip s9_funcReq s9_funcReqRef)


s9_t2_r1, s9_t2_r2, s9_t2_r3, s9_t2_r4, s9_t2_r5, 
  s9_t2_r6 :: [String]

s9_columns_t2 :: [[String]]
s9_columns_t2 = [s9_t2_r1, s9_t2_r2, s9_t2_r3, s9_t2_r4, s9_t2_r5, s9_t2_r6]
s9_t2_r1 = []
s9_t2_r2 = []
s9_t2_r3 = ["Data Constraint"]
s9_t2_r4 = ["R1", "R2"]
s9_t2_r5 = ["T1", "T2"]
s9_t2_r6 = ["IM1", "IM2", "IM3", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8",
  "DD9"]

-- FIXME: Same goes for this one (see above)
s9_table2 = Table (EmptyS:s9_row_header_t2)
  (makeTMatrix s9_col_header_t2 s9_columns_t2 s9_row_t2)
  (showingCxnBw (traceyMatrix) (titleize' requirement +:+ S "and Other" +:+ titleize' item)) True

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
s9_t3_IM1 = ["A4","A6","A7"]
s9_t3_IM2 = ["A5"]
s9_t3_IM3 = []
s9_t3_DD1 = []
s9_t3_DD2 = []
s9_t3_DD3 = ["A4","A8"]
s9_t3_DD4 = []
s9_t3_DD5 = ["A4"]
s9_t3_DD6 = []
s9_t3_DD7 = ["A5"]
s9_t3_DD8 = []
s9_t3_DD9 = ["A9"]
s9_t3_lc1 = ["A3"]
s9_t3_lc2 = ["A4", "A8"]
s9_t3_lc3 = ["A5"]
s9_t3_lc4 = ["A6"]
s9_t3_lc5 = ["A7"]
s9_t3_r1  = []
s9_t3_r2  = ["A4", "A5","A8"]
s9_t3_r3  = []
s9_t3_r4  = []
s9_t3_r5  = []
s9_t3_r6  = []

-- FIXME: Same goes for this one (see above)
s9_table3 = Table (EmptyS:s9_row_header_t3)
  (makeTMatrix s9_col_header_t3 s9_columns_t3 s9_row_t3)
  (showingCxnBw (traceyMatrix) (titleize' assumption +:+ S "and Other" +:+ titleize' item)) True

s9_intro2 = 
  [Paragraph $
  S "The" +:+ phrase purpose +:+ S "of the" +:+ plural traceyGraph +:+ 
  S "is also to provide easy" +:+ plural reference +:+ S "on what has to be" +:+
  S "additionally modified if a certain" +:+ phrase component +:+. S "is changed" +:+ 
  S "The arrows in the" +:+ (plural $ graph ^. term) +:+ S "represent" +:+.
  plural dependency +:+ S "The" +:+ phrase component +:+ S "at the tail of an arrow" +:+
  S "is depended on by the" +:+ phrase component +:+ S "at the head of that arrow. Therefore, if a" +:+
  phrase component +:+ S "is changed, the" +:+ plural component +:+ S "that it points to should also" +:+.
  S "be changed" +:+ titleize figure +:+ S "2" +:+ sParen (makeRef fig_2) +:+ S "shows the" +:+
  plural dependency +:+ S "of" +:+ plural thModel `sC` (plural dataDefn) +:+ S "and" +:+
  plural inModel +:+. S "on each other" +:+ titleize figure +:+ S "3" +:+ sParen (makeRef fig_3) +:+
  S "shows the" +:+ plural dependency +:+ S "of" +:+ plural requirement +:+ S "on" +:+ 
  plural thModel `sC` plural inModel `sC` (plural dataDefn) +:+ S "and" +:+.
  plural datumConstraint +:+ titleize figure +:+ S "4" +:+ sParen (makeRef fig_4) +:+
  S "shows the" +:+ plural dependency +:+ S "of" +:+ plural thModel `sC` plural inModel `sC`
  (plural dataDefn) `sC` plural requirement +:+ S "and" +:+ (plural likelyChg) +:+ S "on" +:+.
  (plural assumption),
  Paragraph $ S "NOTE: Building a tool to automatically generate the graphical" +:+
  S "representation of the" +:+ (phrase $ matrix ^. term) +:+ S "by scanning the" +:+
  plural label +:+ S "and" +:+ phrase reference +:+. S "can be future work"]

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

s10_list = Enumeration $ Simple $ map (\(a, b) -> (a, Flat b))
  [(S "[1]", S "N. Koothoor" `sC` Quote (S "A" +:+ phrase document +:+ 
  S "drive approach to certifying" +:+ phrase sciCompS :+: S ",") +:+
  S "Master's thesis" `sC` S "McMaster University, Hamilton, Ontario, Canada, 2013."),
  (S "[2]", S "W. S. Smith and L. Lai" `sC` Quote (S "A new" +:+ plural requirement +:+
  phrase template +:+ S "for scientific computing,") +:+ S "in Proceedings of the" +:+
  S "First International Workshop on Situational" +:+ titleize' requirement +:+ 
  S "Engineering Processes - Methods, Techniques and Tools to Support Situation-Specific" +:+
  titleize' requirement +:+ S "Engineering Processes, SREP'05 (J.Ralyt" :+: 
  (F Acute 'e') :+: S ", P.Agerfalk, and N.Kraiem, eds.), (Paris, France),"
  +:+ S "pp. 107-121, In conjunction with 13th IEEE International" +:+
  titleize' requirement +:+. S "Engineering Conference, 2005"),
  (S "[3]", S "J. Robertson and S. Robertson" `sC` Quote (S "Volere" +:+
  plural requirement +:+ phrase specification +:+ phrase template +:+. S "edition 16") +:+ 
  Quote (S "www.cs.uic.edu/ i442/VolereMaterials/templateArchive16/c" +:+ 
  S "Volere template16.pdf") :+: S ", 2012."),
  (S "[4]", S "ASTM Standards Committee" `sC` Quote (S "Standard practice"
  +:+ S "for determining" +:+ (phrase $ load ^. term) +:+ S "resistance of" +:+
  S "glass in buildings,") :+: 
  S " Standard E1300-09a, American Society for Testing and Material (ASTM),"
  +:+. S "2009"),
  (S "[5]", S "ASTM, developed by subcommittee C1408,Book of standards 15.02,"
  +:+ Quote (S "Standard" +:+ phrase specification +:+. S "for flat glass,C1036")),
  (S "[6]", S "ASTM, developed by subcommittee C14.08,Book of standards" +:+
  S "15.02" `sC` Quote (at_start specification +:+ S "for" +:+ (plural $ heat ^. term) +:+.
  S "treated flat glass-Kind HS, kind FT coated and uncoated glass,C1048"))]

s11 = SRS.appendix [s11_intro, fig_5, fig_6] []

s11_intro = Paragraph $ foldlSent [
  S "This", phrase appendix, S "holds the", (plural $ graph ^. term),
  sParen ((makeRef fig_5) +:+ S "and" +:+ (makeRef fig_6)),
  S "used for interpolating", plural value, S "needed in the", plural model]

fig_5 = Figure (titleize figure +: S "5" +:+ (demandq ^. defn) +:+ sParen
  (P (demand ^. symbol)) +:+ S "versus" +:+ (at_start $ sD ^. term) +:+
  S "versus" +:+ (at_start $ char_weight ^. term) +:+ sParen
  (P (sflawParamM ^. symbol))) "ASTM_F2248-09.png"

fig_6 = Figure (titleize figure +:+ S "6: Non dimensional" +:+ 
  (phrase $ lateral ^. term) +:+
  (phrase $ load ^. term) +:+ sParen
  (P (dimlessLoad ^. symbol)) +:+ S "versus" +:+ (titleize $ aspectR ^. term) +:+ 
  sParen (short aspectR) {-(P (aspectR ^. symbol))-} +:+ S "versus" +:+
  (at_start $ sdf ^. term) +:+ sParen (P (sdf ^. symbol))) "ASTM_F2248-09_BeasonEtAl.png"