module Drasil.GlassBR.Body where
import Data.Char (toLower)
import Control.Lens ((^.))

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Authors
import Data.Drasil.Concepts.Documentation
import Prelude hiding (id)

import Drasil.TableOfUnits
import Drasil.TableOfSymbols
import Drasil.TableOfAbbAndAcronyms
import Drasil.OrganizationOfSRS

import Drasil.GlassBR.Example

import Drasil.GlassBR.Changes
import Drasil.GlassBR.Modules
import Drasil.GlassBR.Reqs

this_si :: [UnitDefn]
this_si = map UU [metre, second] ++ map UU [pascal, newton]

s1, s1_1,  s1_2, s1_3, s2, s2_1, s2_2, s2_3, s3, s3_1, s3_2, s4, s4_1, s4_2,
  s5, s5_1, s5_2, s6, s6_1, s6_1_1, s6_1_2, s6_1_3, s6_2, s6_2_1, s6_2_2, 
  s6_2_3, s6_2_4, s6_2_5, s7, s7_1, s7_2, s8, s9, s10, s11 :: Section

s1_intro,  --s1_1_intro, s1_1_table, s1_2_intro, s1_2_table, 
  s2_intro, s2_2_intro, s3_intro, 
  s3_1_intro, s3_2_intro, s4_intro, s4_1_bullets, s4_2_intro, s5_intro, 
  s5_1_table, s5_2_bullets, s6_intro, s6_1_intro, s6_1_1_intro, s6_1_1_bullets,
  s6_1_2_intro, s6_1_2_list, s6_1_3_list, s6_2_intro, s6_2_1_intro, 
  s6_2_4_intro, s6_2_5_intro, --s6_2_5_table1, 
  s6_2_5_table2, s6_2_5_intro2, --s6_2_5_table3, 
  s7_1_intro, s7_2_intro, s8_list, s9_intro1, s9_table1, s9_table2, s9_table3,
  s10_list, s11_intro, fig_glassbr, fig_2, fig_3, fig_4, 
  fig_5, fig_6 :: Contents

s2_1_intro, s2_3_intro, s6_2_1_list, s7_1_list, s9_intro2 :: [Contents]

srs_authors, mg_authors :: Sentence
srs_authors = twoNames nikitha spencerSmith
mg_authors = twoNames spencerSmith thulasi

glassBR_srs :: Document  
glassBR_srs = Document ((srs ^. defn) +:+ S "for" +:+ (gLassBR ^. defn)) srs_authors
  [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11]

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

glassBR_mg :: Document
glassBR_mg = Document (S "Module Guide for" +:+ (gLassBR ^. term)) mg_authors mgBod


s1 = Section(S "Reference Material") [Con s1_intro, Sub s1_1, Sub s1_2, 
  Sub s1_3]

s1_intro = Paragraph (S "This section records information for easy reference.")

s1_1 = table_of_units this_si

s1_2 = table_of_symbols ((map qs glassBRSymbols) ++ 
  (map qs glassBRUnitless)) (^.term)

s1_3 = table_of_abb_and_acronyms acronyms

s2 = Section(S "Introduction") [Con s2_intro, Sub s2_1, Sub s2_2, Sub s2_3]

s2_intro = Paragraph $ 
  S "Software is helpful to efficiently and correctly predict the blast" +:+
  S "risk involved with the" +:+. (sMap (map toLower) (glaSlab ^. term)) +:+ 
  S "The" +:+ (sMap (map toLower) (blast ^. term)) +:+ S "under" +:+
  S "consideration is" +:+. (sMap (map toLower) (blast ^. defn)) +:+ 
  S "The software, herein called" +:+ (gLassBR ^. defn) +:+ S "aims to" +:+
  S "predict the blast risk involved with the" +:+ 
  (sMap (map toLower) (glaSlab ^. term)) +:+ S "using an intuitive" +:+
  S "interface. The following section provides an overview of the" +:+ 
  (srs ^. defn) +:+ sParen (srs ^. term) +:+ S "for" +:+. (gLassBR ^. defn) +:+
  S "This section explains the purpose of the" +:+
  S "document is designed to fulfil, the scope of the requirements and" +:+
  S "the organization of the document: what the document is based on and" +:+.
  S "intended to portray"

s2_1 = Section (S "Purpose of Document") (map Con s2_1_intro)

s2_1_intro = 
  [Paragraph $
  S "The main purpose of this document is to predict whether a given" +:+
  (sMap (map toLower) (glaSlab ^. term)) +:+ S "is likely to resist a" +:+
  S "specified" +:+. (sMap (map toLower) (blast ^. term)) +:+
  S "The goals and" +:+ (sMap (map toLower) (thModel ^. defn)) :+:
  S "s used in the" +:+ (gLassBR ^. defn) +:+ S "code are provided" `sC`
  S "with an emphasis on explicitly identifying" +:+ 
  (sMap (map toLower) (assumption ^. defn)) :+: S "s and unambiguous" +:+
  S "definitions. This document is intended to be used as a reference" +:+
  S "to provide all information necessary to understand and verify the" +:+
  S "analysis. The" +:+ (srs ^. term) +:+ S "is abstract" +:+
  S "because the contents say what problem is being solved, but not how" +:+.
  S "to solve it",
  Paragraph $
  S "This document will be used as a starting point for subsequent" +:+
  S "development phases, including writing the design specification and" +:+
  S "the software verification and validation plan. The design document" +:+
  S "will show how the requirements are to be realized, including" +:+.
  S "decisions on the numerical algorithms and programming environment" +:+
  S "The verification and validation plan will show the steps that will" +:+
  S "be used to increase confidence in the software documentation and" +:+.
  S "the implementation"]

s2_2 = Section (S "Scope of" +:+ (requirement ^. defn) :+: S "s") 
  [Con s2_2_intro]

s2_2_intro = Paragraph $
  S "The scope of the" +:+ (sMap (map toLower) (requirement ^. defn)) :+: 
  S "s includes getting all input parameters related to the" +:+ 
  (sMap (map toLower) (glaSlab ^. term)) +:+ S "and also the parameters" +:+
  S "related to" +:+. (sMap (map toLower) (blastTy ^. term)) +:+ 
  S "Given the input" `sC` (gLassBR ^. defn) +:+ S "is intended to" +:+
  S "use the data and predict whether the" +:+ 
  (sMap (map toLower) (glaSlab ^. term)) +:+. S "is safe to use or not"

s2_3 = Section (S "Organization of Document") (map Con s2_3_intro)

s2_3_intro = 
  [Paragraph $
  S "The organization of this document follows the template for an" +:+ 
  (srs ^. term) +:+ S "for scientific computing software" +:+
  S "proposed by [1] and [2] (in" +:+ (makeRef s10) :+: S "), with" +:+ 
  S "some aspects taken from Volere template 16 [3]. The presentation" +:+
  S "follows the standard pattern of presenting goals, theories" `sC`
  S "definitions, and" +:+ (sMap (map toLower) (assumption ^. defn)) :+:
  S "s. For readers that would like a more bottom up approach, they can" +:+
  S "start reading the" +:+ (sMap (map toLower) (dataDefn ^. defn)) :+:
  S "s in" +:+ (makeRef s6_2_4) +:+ S "and trace back to find any" +:+.
  S "additional information they require",
  Paragraph $ refineChain [goalStmt, thModel, inModel] +:+
  {-
  S "The" +:+ (sMap (map toLower) (goalStmt ^. defn)) :+: S "s are" +:+
  S "refined to the" +:+ (sMap (map toLower) (thModel ^. defn)) :+:
  S "s, and" +:+ (sMap (map toLower) (thModel ^. defn)) :+: 
  S "s to the" +:+ (sMap (map toLower) (inModel ^. defn)) :+: 
  -}
  S "The" +:+ addS (sLower (dataDefn ^. defn)) +:+.
  S "are used to support the definitions of the different models"] 
  
s3 = Section(S "Stakeholders") [Con s3_intro, Sub s3_1, Sub s3_2]

s3_intro = Paragraph $
  S "This section describes the Stakeholders: the people who have an" +:+.
  S "interest in the product"

s3_1 = Section (S "The Client") [Con s3_1_intro]

s3_1_intro = Paragraph $
  S "The client for" +:+ (gLassBR ^. defn) +:+ S "is a company named" +:+
  S "Entuitive. It is developed by Dr. Manuel Campidelli. The client has" +:+.
  S "the final say on acceptance of the product"

s3_2 = Section (S "The Customer") [Con s3_2_intro]

s3_2_intro = Paragraph $
  S "The customers are the end user of" +:+. (gLassBR ^. defn)

s4 = Section(S "General System Description") [Con s4_intro, Sub s4_1, 
  Sub s4_2]

s4_intro = Paragraph $
  S "This section provides general information about the system" `sC`
  S "identifies the interface between the system and its environment" `sC`
  S "and describes the user characteristics and the system constraints."

s4_1 = Section (S "User Characteristics") [Con s4_1_bullets]

s4_1_bullets = Enumeration $ Bullet $ map Flat
  [(S "The end user of" +:+ (gLassBR ^. defn) +:+ S "is expected to" +:+
  S "have completed at least the equivalent of the second year of an" +:+.
  S "undergraduate degree in civil or structural engineering"),
  (S "The end user is expected to have an understanding of theory behind" +:+
  (sMap (map toLower) ((gbr ^. term))) +:+. S "and blast risk"),
  (S "The end user is expected to have basic computer literacy to handle" +:+.
  S "the software")]

s4_2 = Section (S "System Constraints") [Con s4_2_intro]

s4_2_intro = Paragraph $
  (notApp ^. term)

s5 = Section(S "Scope of the Project") [Con s5_intro, Sub s5_1, Sub s5_2]

s5_intro = Paragraph $
  S "This section presents the scope of the project. It describes the" +:+
  S "expected use of" +:+ (gLassBR ^. defn) +:+ S "as well as the" +:+
  S "inputs and outputs of each action. The use cases are input and" +:+
  S "output, which defines the action of getting the input and displaying" +:+.
  S "the output"

s5_1 = Section (S "Product Use Case Table") [Con s5_1_table]

s5_1_table = Table [S "Use Case NO.", S "Use Case Name", S "Actor", 
  S "Input and Output"] (mkTable
  [(\x -> (x!!0)),(\x -> (x!!1)), (\x -> (x!!2)), (\x -> (x!!3))]
  [[S "1", S "Inputs", S "User", S "Characteristics of the" +:+
  (sMap (map toLower) (glaSlab ^. term)) +:+ S "and of the" +:+.
  (sMap (map toLower) (blast ^. term)) +:+ S "Details in" +:+ 
  (makeRef s5_2)],
  [S "2", S "Output", (gLassBR ^. defn), S "Whether or not the" +:+
  (sMap (map toLower) (glaSlab ^. term)) +:+ S "is safe for the calculated" +:+
  (sMap (map toLower) (load ^. term)) +:+ S "and supporting" +:+
  S "calculated values"]])
  (S "Table 1: Use Case Table") True

s5_2 = Section (S "Individual Product Use Cases") [Con s5_2_bullets]

s5_2_bullets = Enumeration $ Bullet $ map Flat
  [(S "Use Case 1 refers to the user providing input to" +:+ 
  (gLassBR ^. defn) +:+ S "for use within the analysis. There are two" +:+
  S "classes of inputs:" +:+ (sMap (map toLower) (glassGeo ^. term)) +:+
  S "and" +:+. (sMap (map toLower) (blastTy ^. term)) +:+
  (glassGeo ^. defn) +:+ (blastTy ^. defn) +:+ S "These" +:+
  S "parameters describe" +:+ (sMap (map toLower) (char_weight ^. term)) +:+
  S "and stand off" +:+. (sMap (map toLower) (blast ^. term)) +:+
  S "Another input the user gives is the tolerable value of" +:+.
  (sMap (map toLower) (prob_br ^. term))),
  (S " Use Case 2" +:+ (gLassBR ^. defn) +:+ S "outputs if the" +:+
  (sMap (map toLower) (glaSlab ^. term)) +:+ S "will be safe by" +:+
  S "comparing whether" +:+ (sMap (map toLower) (capacity ^. term)) +:+
  S "is greater than" +:+. (sMap (map toLower) (demandq ^. term)) +:+
  (capacity ^. term) +:+ S "is the" +:+
  (sMap (map toLower) (capacity ^. defn)) +:+ S "and" +:+
  (sMap (map toLower) (demandq ^. term)) +:+ S "is the requirement" +:+
  S "which is the" +:+. (demandq ^. defn) +:+ S "The second condition" +:+
  S "is to check whether the calculated probability" +:+ 
  sParen (P $ prob_br ^. symbol) +:+ 
  S "is less than the tolerable probability" +:+ 
  sParen (P $ pb_tol ^. symbol) +:+ S "which is obtained from the user" +:+
  S "as an input. If both conditions return true then it's shown that the" 
  +:+ (sMap (map toLower) (glaSlab ^. term)) +:+ S "is safe to use" `sC` 
  S "else if both return false then the" +:+ 
  (sMap (map toLower) (glaSlab ^. term)) +:+. S "is considered unsafe" +:+.
  S "All the supporting calculated values are also displayed as output")]

s6 = Section(S "Specific System Description") [Con s6_intro, Sub s6_1,
  Sub s6_2]

s6_intro = Paragraph $ 
  S "This section first presents the problem description, which gives a" +:+
  S "high-level view of the problem to be solved. This is followed by" +:+
  S "the solution characteristics specification, which presents the" +:+
  (sMap (map toLower) (assumption ^. defn)) :+: S "s, theories, definitions."

s6_1 = Section (S "Problem Description") [Con s6_1_intro, Sub s6_1_1, 
  Sub s6_1_2, Sub s6_1_3]

s6_1_intro = Paragraph $ 
  S "A system is needed to efficiently and correctly predict the blast" +:+.
  S "risk involved with the glass" +:+ (gLassBR ^. defn) +:+ S "is a" +:+
  S "computer program developed to interpret the inputs to give out the" +:+
  S "outputs which predicts whether the" +:+ 
  (sMap (map toLower) (glaSlab ^. term)) +:+ S "can withstand the" +:+
  (sMap (map toLower) (blast ^. term)) +:+. S "under the conditions"

s6_1_1 = Section (S "Terminology and Definitions") [Con s6_1_1_intro, 
  Con s6_1_1_bullets]
  
s6_1_1_intro = Paragraph $ 
  S "This subsection provides a list of terms that are used in subsequent" +:+
  S "sections and their meaning, with the purpose of reducing ambiguity" +:+
  S "and making it easier to correctly understand the" +:+ 
  (sMap (map toLower) (requirement ^. defn)) :+: S "s. All of the terms" +:+
  S "are extracted from [4] in" +:+. (makeRef s10)

s6_1_1_bullets = Enumeration $ (Number $ 
  [Flat $ ((aR ^. term) :+: sParenDash (aspectR ^. term)) :+: 
  (aR ^. defn)] ++
  map (\c -> Flat $ ((c ^. term) +:+ S "- ") :+: (c ^. defn)) [gbr, lite] ++ 
  [Nested (((glassTy ^. term) :+: S ":")) (Bullet $ map (\c -> Flat c)
  [(((an ^. term) :+: sParenDash (annealedGlass ^. term)) :+: 
    (an ^. defn)),
  (((ft ^. term) :+: sParenDash (fullyTGlass ^. term)) :+:
    (ft ^. defn)),
  (((hs ^. term) :+: sParenDash (heatSGlass ^. term)) :+:
    (hs ^. defn))])] ++
  map (\c -> Flat c)
  [(((gtf ^. term) :+: sParenDash (glassTypeFac ^. term)) :+: 
  (gtf ^. defn)),
  (((lateral ^. term) +:+ S "- ") :+: (lateral ^. defn))] ++ 
  [Nested (((load ^. term) :+: S ":")) (Bullet $ map (\c -> Flat c)  
  [(((specDeLoad ^. term) +:+ S "- ") :+: (specDeLoad ^. defn)),
  (((lr ^. term) :+: sParenDash (lResistance ^. term)) :+: 
    (lr ^. defn)),
  (((ldl ^. term) +:+ S "- ") :+: (ldl ^. defn)),
  (((nfl ^. term) :+: sParenDash (nonFactorL ^. term)) :+:
    (nfl ^. defn))] ++ 
  map (\c -> Flat $ ((c ^. term) +:+ S "- ") :+: (c ^. defn))
    [glassWL, sdl])] ++ 
  map (\c -> Flat c)
  [(((lsf ^. term) :+: sParenDash (lShareFac ^. term)) :+: 
  (lsf ^. defn)),
  (((pb ^. term) :+: sParenDash (P $ prob_br ^. symbol)) :+:
  (pb ^. defn))] ++
  map (\c -> Flat $ ((c ^. term) +:+ S "- ") :+: (c ^. defn)) 
  [specA, blaReGLa, eqTNTChar] ++
  [Flat $ ((sD ^. term) :+: sParenDash (P $ sd ^. symbol)) :+:
  (sD ^. defn)])
  where sParenDash = \x -> S " (" :+: x :+: S ") - "
  
s6_1_2 = Section (physSyst ^. defn) [Con s6_1_2_intro, Con s6_1_2_list, 
  Con fig_glassbr]

s6_1_2_intro = Paragraph $ S "The physical system of" +:+ (gLassBR ^. defn) 
  +:+ S "as shown in" +:+ (makeRef fig_glassbr) `sC` S "includes the" +:+
  S "following elements:"

fig_glassbr = Figure (S "The physical system") "physicalsystimage.png"
  
s6_1_2_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (((physSyst ^. term) :+: S "1"), (glaSlab ^. term)), 
  (((physSyst ^. term) :+: S "2"), S "The point of explosion." +:+
  S "Where the bomb, or" +:+ (sMap (map toLower) (blast ^. defn)) `sC` 
  S "is located. The" +:+ (sMap (map toLower) ((sD ^. term))) 
  +:+ S "is the distance between the point of explosion and the glass.")]
--NOTE: The only difference here from the original is the removal of an 
--    extraneous space

s6_1_3 = Section ((goalStmt ^. defn) :+: S "s") [Con s6_1_3_list]

s6_1_3_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (((goalStmt ^. term) :+: S "1"), S "Analyze and predict whether the" +:+
  (sMap (map toLower) (glaSlab ^. term)) +:+ S "under consideration" +:+
  S "will be able to withstand the explosion of a certain degree which" +:+.
  S "is calculated based on user input")]

s6_2 = Section (S "Solution Characteristics Specification") 
  [Con s6_2_intro, Sub s6_2_1, Sub s6_2_2, Sub s6_2_3, Sub s6_2_4, Sub s6_2_5]

s6_2_intro = Paragraph $ S "This section explains all the" +:+
  (sMap (map toLower) (assumption ^. defn)) :+: S "s considered and the" +:+
  (sMap (map toLower) (thModel ^. defn)) :+: S "s which are" +:+
  S "supported by the" +:+ (sMap (map toLower) (dataDefn ^. defn)) :+: S "s."
  
s6_2_1 = Section (assumption ^. defn :+: S "s") ([Con s6_2_1_intro] ++
  (map Con s6_2_1_list))

s6_2_1_intro = Paragraph $ 
  S "This section simplifies the original problem and helps in developing the" 
  +:+ (sMap (map toLower) (thModel ^. defn)) +:+ 
  S "[" :+: (thModel ^. term) :+: S "] by filling in the missing" 
  +:+ S "information for the physical system. The numbers given in the" +:+
  S "square brackets refer to the" +:+ 
  (sMap (map toLower) (dataDefn ^. defn)) +:+
  S "[" :+: (dataDefn ^. term) :+: S "], or" +:+
  (sMap (map toLower) (inModel ^. defn)) +:+ 
  S "[" :+: (inModel ^. term) :+: S "], in which the respective" +:+
  (sMap (map toLower) $ assumption ^. defn) +:+. S "is used"

s6_2_1_list = 
  [(Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (((assumption ^. term) :+: S "1"), S "The standard E1300-09a for" +:+
    S "calculation applies only to monolithic, laminated, or insulating" +:+
    S "glass constructions of rectangular shape with continuous" +:+ 
    (sMap (map toLower) (lateral ^. term)) +:+
    S "support along one, two, three, or four edges. This practice assumes" 
    +:+ S "that (1) the supported glass edges for two, three and four-sided" 
    +:+ S "support conditions are simply supported and free to slip in" +:+
    S "plane; (2) glass supported on two sides acts as a simply supported" 
    +:+. S "beam and (3) glass supported on one side acts as a cantilever"), 
  (((assumption ^. term) :+: S "2"), S "This practice does not apply" 
    +:+ S "to any form of wired, patterned, etched, sandblasted, drilled" `sC`
    S "notched, or grooved glass with surface and edge treatments" +:+.
    S "that alter the glass strength"),
  (((assumption ^. term) :+: S "3"), S "This system only considers" +:+.
    S "the external explosion scenario for its calculations"),
  (((assumption ^. term) :+: S "4"), S "Standard values used for" +:+
    S "calculation in" +:+ (gLassBR ^. defn) +:+ S "are:")]),
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
  (Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (((assumption ^. term) :+: S "5"), S "Glass under consideration" +:+
    S "is assumed to be a single" +:+.
    (sMap (map toLower) (lite ^. term)) +:+ S "Hence the value of" +:+ 
    (P $ loadSF ^. symbol) +:+ S "is equal to 1 for all calculations in" 
    +:+. (gLassBR ^. defn)),
  (((assumption ^. term) :+: S "6"), S "Boundary conditions for the" +:+ 
    (sMap (map toLower) (glaSlab ^. term)) +:+ S "is assumed to be 4-sided"
    +:+ S "support for calculations."),
  (((assumption ^. term) :+: S "7"), S "The response type considered in" 
    +:+ (gLassBR ^. defn) +:+. S "is flexural"),
  (((assumption ^. term) :+: S "8"), S "With reference to A4 the value" 
    +:+ S "of" +:+ (sMap (map toLower) (loadDF ^. term)) +:+ 
    sParen (P $ loadDF ^. symbol) +:+ S "is a constant in" +:+. 
    (gLassBR ^. defn) +:+ S "It is calculated by the equation:" +:+
    --(P $ loadDF ^. symbol) +:+ S "=" +:+ (P $ load_dur ^. symbol) :+: 
    S ". Using this" `sC` (P $ loadDF ^. symbol) +:+. S "= 0.27")])]
  --equation in sentence

s6_2_2 = Section ((thModel ^. defn) :+: S "s") (map Con s6_2_2_TMods)
  
s6_2_2_TMods :: [Contents]
s6_2_2_TMods = map Definition (map Theory tModels)

s6_2_3 = Section ((inModel ^. defn) :+: S "s") (map Con s6_2_3_IMods)

s6_2_3_IMods :: [Contents]
s6_2_3_IMods = map Definition (map Theory iModels)

s6_2_4 = Section ((dataDefn ^. defn) :+: S "s") 
  ((Con s6_2_4_intro):(map Con s6_2_4_DDefns))

s6_2_4_intro = Paragraph $ 
  S "This section collects and defines all the data needed to build the" +:+
  (sMap (map toLower) (inModel ^. defn)) :+: S "s."

s6_2_4_DDefns ::[Contents] 
s6_2_4_DDefns = map Definition (map Data dataDefns)

s6_2_5 = Section (S "Data Constraints") [Con s6_2_5_intro, --Con s6_2_5_table1, 
  Con s6_2_5_table2, Con s6_2_5_intro2] --, Con s6_2_5_table3]

s6_2_5_intro = Paragraph $
  S "Table 2 (" :+: --(makeRef s6_2_5_table1) :+: 
  S ") shows the data" +:+
  S "constraints on the input variables. The column of physical constraints"
  +:+ S "gives the physical limitations on the range of values that can" +:+
  S " be taken by the variable. The constraints are conservative, to give"
  +:+ S "the user of the model the flexibility to experiment with unusual"
  +:+ S "situations. The column of typical values is intended to provide" +:+
  S "a feel for a common scenario. The uncertainty column provides an" +:+
  S "estimate of the confidence with which the physical quantities can be"
  +:+ S "measured. This information would be part of the input if one were"
  +:+ S "performing an uncertainty quantification exercise. Table 3 (" :+:
  (makeRef s6_2_5_table2) :+: S ") gives the values of the specification" +:+
  S "parameters used in Table 2 (" :+: --(makeRef s6_2_5_table1) :+: 
  S ")." +:+ 
  (P $ ar_max ^. symbol) +:+ S "refers to the" +:+
  (sMap (map toLower) (ar_max ^. term)) +:+. S "for the plate of glass"

-- s6_2_5_table1 = Table [S "Var", S "Physical Cons", S "Software Constraints", S "Typical Value",
--  S "Uncertainty"] (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2), (\x -> x!!3),
--  (\x -> x!!4)] [[(P $ plate_len ^. symbol), (P $ plate_len ^. symbol) +:+ S "> 0 and" +:+ 
--  (P $ plate_len ^. symbol) :+: S "/" :+: (P $ plate_width ^. symbol) +:+ S "> 1",
--  (P $ dim_min ^. symbol) +:+ S "<=" +:+ (P $ plate_len ^. symbol) +:+ S "<=" +:+ 
--  (P $ dim_max ^. symbol) +:+ S "and" +:+ (P $ plate_len ^. symbol) :+: S "/" :+: 
--  (P $ plate_width ^. symbol) +:+ S "<" +:+ (P $ ar_max ^. symbol), S "1500" +:+
--  Sy (plate_len ^. unit), S "10%"], [(P $ plate_width ^. symbol), (P $ (plate_width ^. symbol)) 
--  +:+ S "> 0 and" +:+ (P $ plate_width ^. symbol) +:+ S "<" +:+ (P $ plate_len ^. symbol),
--  (P $ dim_min ^. symbol) +:+ S "<=" +:+ (P $ plate_width ^. symbol) +:+ S "<=" +:+ 
--  (P $ dim_max ^.symbol) +:+ S "and" +:+ (P $ plate_len ^. symbol) :+: S "/" :+: 
--  (P $ plate_width ^. symbol) +:+ S "<" +:+ (P $ ar_max ^. symbol), S "1200" +:+ 
--  Sy (plate_width ^. unit), S "10%"], [(P $ pb_tol ^. symbol), S "0 <" +:+ 
--  (P $ pb_tol ^. symbol) +:+ S "< 1", S "-", S "0.008", S "0.1%"], [(P $ char_weight ^. symbol), 
--  (P $ char_weight ^. symbol) +:+ S ">= 0", (P $ cWeightMin ^. symbol) +:+ S "<" +:+ 
--  (P $ char_weight ^. symbol) +:+ S "<" +:+ (P $ cWeightMax ^. symbol), S "42" +:+ 
--  Sy (char_weight ^. unit), S "10%"],[(P $ tNT ^. symbol), (P $ tNT ^. symbol) :+: 
--  S " > 0", S "-", S "1", S "10%"], [(P $ sd ^. symbol), (P $ sd ^. symbol) +:+ S "> 0", 
--  (P $ sd_min ^. symbol) +:+ S "<" +:+ (P $ sd ^. symbol) +:+ S "<" +:+ 
--  (P $ sd_max ^. symbol), S "45" :+: Sy (sd ^. unit), S "10%"]])
--  (S "Table 2: Input Variables") True

s6_2_5_table2 = Table [S "Var", S "Value"] (mkTable 
  [(\x -> P $ fst x), (\x -> snd x)] 
  [(dim_min ^. symbol, S "0.1" +:+ Sy (sd ^. unit)), 
  (dim_max ^.symbol, S "0.1" +:+ Sy (sd ^. unit)),(ar_max ^. symbol, S "5"), 
  (cWeightMin ^. symbol, S "4.5" +:+ Sy (cWeightMin ^. unit)),
  (cWeightMax ^. symbol, S "910" +:+ Sy (cWeightMax ^. unit)), 
  (sd_min ^. symbol, S "6" +:+ Sy (sd_min ^. unit)), 
  (sd_max ^. symbol, S "130" +:+ Sy (sd_max ^. unit))])
  (S "Table 3: Specification Parameter Values") True

s6_2_5_intro2 = Paragraph $
  S "Table 4 (" :+: --(makeRef s6_2_5_table3) :+:
  S ") shows the constraints"
  +:+. S "that must be satisfied by the output"

-- s6_2_5_table3 = Table [S "Var", S "Physical Constraints"] (mkTable 
--  [(\x -> P $ fst(x)), (\x -> snd(x))] 
--  [(prob_br ^. symbol, S "0 <" +:+ (P $ prob_br ^. symbol) +:+ S "< 1")])
--  (S "Table4: Output Variables") True

s7 = Section((requirement ^. defn) :+: S "s") [Sub s7_1, Sub s7_2]

s7_1 = Section (S "Functional" +:+ (requirement ^. defn) :+: S "s") 
  ([Con s7_1_intro] ++ (map Con s7_1_list))

s7_1_intro = Paragraph $
  S "The following section provides the functional" +:+
  (requirement ^. defn) :+: S "s, the business tasks that the software" +:+.
  S "is expected to complete"

s7_1_list = 
  [(Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(((requirement ^. term) :+: S "1"), S "Input the following" +:+
    S "quantities, which define the glass dimensions" `sC` 
    (sMap (map toLower) (glassTy ^. defn)) `sC` S "tolerable probability"
    +:+ S "of failure and the characteristics of the" +:+ 
    (sMap (map toLower) (blast ^. term)) :+: S ":")]),
  (table ((map qs [plate_len,plate_width,sdx,sdy,sdz,nom_thick,char_weight]) 
  ++ (map qs [glass_type,pb_tol,tNT])) (^.term) ),
--s7_1_table = Table [S "Symbol", S "Units", S "Description"] (mkTable
--  [(\ch -> P (ch ^. symbol)),  
--   (\ch -> maybeUnits $ ch ^. unit'),
--   (\ch -> ch ^. term)
--   ]
--  [plate_len,plate_width,glass_type,pb_tol,sdx,sdy,sdz,nom_thick,tNT,
--  char_weight])
--  (S "Input Parameters") False
  (Enumeration $ Simple $
  [(((requirement ^. term) :+: S "2"), Nested (S "The system shall set" +:+
  S "the known values as follows: ") (Bullet $ map (\c -> Flat c) 
    [(P $ sflawParamM ^. symbol) `sC` (P $ sflawParamK ^. symbol) `sC` 
    (P $ mod_elas ^. symbol) `sC` (P $ load_dur ^. symbol) +:+ 
    S "following" +:+ (assumption ^. term) :+: S "4",
    (P $ loadDF ^. symbol) +:+ S "following" +:+ (assumption ^. term) 
    :+: S "8",
    (P $ loadSF ^. symbol) +:+ S "following" +:+ (assumption ^. term) 
    :+: S "5"]))] ++
  map (\(a,b) -> (a, Flat b))
  [(((requirement ^. term) :+: S "3"), S "The system shall check the" +:+
  S "entered input values to ensure that they do not exceed the data" +:+
  S "constraints mentioned in" +:+. (makeRef s6_2_5) +:+ S "If any of" +:+
  S "the input parameters is out of bounds, an error message is" +:+.
  S "displayed and the calculations stop"),
  (((requirement ^. term) :+: S "4"), S "Output the input quantities" +:+
  S "from" +:+ (requirement ^. term) :+: S "1 and the known quantities"
  +:+ S "from" +:+ (requirement ^. term) :+: S "2."),
  (((requirement ^. term) :+: S "5"), S "If" +:+ (P $ is_safe1 ^. symbol)
  +:+ S "and" +:+ (P $ is_safe2 ^. symbol) +:+ S "(from" +:+ 
  (makeRef (Definition (Theory t1SafetyReq))) +:+ S "and" +:+ 
  (makeRef (Definition (Theory t2SafetyReq))) :+: S ") are true" `sC`
  S "output the message" +:+ Quote (safeMessage ^. defn) +:+ S "If" +:+
  S "the condition is false, then output the message" +:+ 
  Quote (notSafe ^. defn))] ++
  [(((requirement ^. term) :+: S "6"), Nested (S "Output the following"
  +:+ S "quantities:")
  (Bullet $ 
    [Flat $ (prob_br ^. term) +:+ sParen (P $ prob_br ^. symbol) +:+ 
    sParen (makeRef (Definition (Theory probOfBr)))] ++
    [Flat $ (lRe ^. term) +:+ sParen(P $ lRe ^. symbol) +:+ 
    sParen (makeRef (Definition (Theory calOfCap)))] ++
    [Flat $ (demand ^. term) +:+ sParen (P $ demand ^. symbol) +:+
    sParen (makeRef (Definition (Theory calOfDe)))] ++
    [Flat $ (act_thick ^. term) +:+ sParen(P $ act_thick ^. symbol) +:+
    sParen (makeRef (Definition (Data hFromt)))] ++
    map (\c -> Flat $ (c ^. term) +:+ sParen(P $ c ^. symbol) +:+ 
    sParen (makeRef (Definition (Data c))))
    [loadDF,strDisFac,nonFL]++
    [Flat $ (gTF ^. term) +:+ sParen(P $ gTF ^. symbol) +:+ 
    sParen (makeRef (Definition (Data glaTyFac)))] ++
    map (\c -> Flat $ (c ^. term) +:+ sParen (P $ c ^. symbol) +:+ 
    sParen (makeRef (Definition (Data c))))
    [dL,tolPre,tolStrDisFac] ++
    [Flat $ (ar ^. term) +:+ sParen(P $ ar ^. symbol)  
    --S " = a/b)"
    ]))])]

s7_2 = Section (S "Nonfunctional" +:+ (requirement ^. defn) :+: S "s") 
  [Con s7_2_intro]

s7_2_intro = Paragraph $
  S "Given the small size, and relative simplicity, of this problem" `sC`
  S "performance is not a priority. Any reasonable implementation will" +:+
  S "be very quick and use minimal storage. Rather than performance" `sC`
  S "the priority nonfunctional" +:+ (requirement ^. term) :+: 
  S "s are correctness, verifiability, understandability, reusability," +:+.
  S "maintainability and portability"

s8 = Section((likelyChg ^. defn) :+: S "s") [Con s8_list]

s8_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(((likelyChg ^. term) :+: S "1"), ((assumption ^. term) :+: 
  S "3 - The system currently only calculates for external blast risk." +:+.
  S "In the future calculations can be added for the internal blast risk")),
  (((likelyChg ^. term) :+: S "2"), ((assumption ^. term) :+:
  S "4" `sC` (assumption ^. term) :+: S "8 - Currently the values for"
  +:+ (P $ sflawParamM ^. symbol) `sC` (P $ sflawParamK ^. symbol) `sC`
  S "and" +:+ (P $ mod_elas ^. symbol) +:+ S "are assumed to be the"
  +:+ S "same for all glass. In the future these values can be changed to"
  +:+. S "variable inputs")),
  (((likelyChg ^. term) :+: S "3"), ((assumption ^. term ) :+: 
  S "5 - The software may be changed to accommodate more than a single" +:+.
  (sMap (map toLower) (lite ^. term)))),
  (((likelyChg ^. term) :+: S "4"), ((assumption ^. term) :+: 
  S "6 - The software may be changed to accommodate more boundary" +:+.
  S "conditions than 4-sided support")),
  (((likelyChg ^. term) :+: S "5"), ((assumption ^. term) :+: 
  S "7 - The software may be changed to consider more than just flexure" +:+.
  S "of the glass"))]

s9 = Section(S "Traceability Matrices and Graphs") ([Con s9_intro1, 
  Con s9_table1, Con s9_table2, Con s9_table3] ++ (map Con s9_intro2) ++ 
  [Con fig_2, Con fig_3, Con fig_4])

s9_intro1 = Paragraph $
  S "The purpose of the traceability matrices is to provide easy references"
  +:+ S "on what has to be additionally modified if a certain component is"
  +:+ S "changed. Every time a component is changed, the items in the column"
  +:+ S "of that component that are marked with an" +:+ Quote (S "X") +:+
  S "should be modified as well. Table 5" +:+ sParen (makeRef s9_table1) +:+ 
  S "shows the dependencies of" +:+ 
  (sMap (map toLower) (thModel ^. defn)) :+: S "s" `sC` 
  (sMap (map toLower) (dataDefn ^. defn)) :+: S "s and" +:+
  (sMap (map toLower) (inModel ^. defn)) :+: S "s with each other." +:+
  S "Table 6" +:+ sParen (makeRef s9_table2) +:+ S "shows the dependencies of" +:+
  (sMap (map toLower) (requirement ^. defn)) :+: S "s on" +:+
  (sMap (map toLower) (thModel ^. defn)) :+: S "s" `sC`
  (sMap (map toLower) (inModel ^. defn)) :+: S "s" `sC`
  (sMap (map toLower) (dataDefn ^. defn)) :+: S "s and data constraints." +:+
  S "Table 7" +:+ sParen (makeRef s9_table3) +:+ S "shows the dependencies of"
  +:+ (sMap (map toLower) (thModel ^. defn)) :+: S "s" `sC`
  (sMap (map toLower) (dataDefn ^. defn)) :+: S "s" `sC`
  (sMap (map toLower) (inModel ^. defn)) :+: S "s" `sC`
  (sMap (map toLower) (likelyChg ^. defn)) :+: S "s and" +:+
  (sMap (map toLower) (requirement ^. defn)) :+: S "s on the" +:+
  (sMap (map toLower) (assumption ^. defn)) :+: S "s."

--FIXME: There has to be a better way to do this.
s9_table1 = Table [S "", 
  S "T1"  +:+ sParen (makeRef (Definition (Theory t1SafetyReq))), 
  S "T2"  +:+ sParen (makeRef (Definition (Theory t2SafetyReq))),
  S "IM1" +:+ sParen (makeRef (Definition (Theory probOfBr))), 
  S "IM2" +:+ sParen (makeRef (Definition (Theory calOfCap))),
  S "IM3" +:+ sParen (makeRef (Definition (Theory calOfDe))),
  S "DD1" +:+ sParen (makeRef (Definition (Data risk))),
  S "DD2" +:+ sParen (makeRef (Definition (Data hFromt))),
  S "DD3" +:+ sParen (makeRef (Definition (Data loadDF))), 
  S "DD4" +:+ sParen (makeRef (Definition (Data strDisFac))), 
  S "DD5" +:+ sParen (makeRef (Definition (Data nonFL))),
  S "DD6" +:+ sParen (makeRef (Definition (Data glaTyFac))),
  S "DD7" +:+ sParen (makeRef (Definition (Data dL))), 
  S "DD8" +:+ sParen (makeRef (Definition (Data tolPre))),
  S "DD9" +:+ sParen (makeRef (Definition (Data tolStrDisFac)))]
  --For now, I'm not propagating the sParen changes 
  --through the rest of this.
  [[S "T1 (" :+: (makeRef (Definition (Theory t1SafetyReq))) :+: S ")", S "",
  S "X", S "X", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "",
  S ""], 
  [S "T2 (" :+: (makeRef (Definition (Theory t2SafetyReq))) :+: S ")", S "X",
  S "", S "", S "X", S "X", S "", S "", S "", S "", S "", S "", S "", S "", 
  S ""],
  [S "IM1 (" :+: (makeRef (Definition (Theory probOfBr))) :+: S ")", S "",
  S "", S "", S "", S "", S "X", S "X", S "X", S "X", S "", S "", S "", S "",
  S ""],
  [S "IM2 (" :+: (makeRef (Definition (Theory calOfCap))) :+: S ")", S "", 
  S "", S "", S "", S "", S "", S "", S "", S "", S "X", S "X", S "", S "",
  S ""],
  [S "IM3 (" :+: (makeRef (Definition (Theory calOfDe))) :+: S ")", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "",
  S ""],
  [S "DD1 (" :+: (makeRef (Definition (Data risk))) :+: S ")", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "DD2 (" :+: (makeRef (Definition (Data hFromt))) :+: S ")", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "DD3 (" :+: (makeRef (Definition (Data loadDF))) :+: S ")", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "DD4 (" :+: (makeRef (Definition (Data strDisFac))) :+: S ")", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "X", S "",
  S ""],
  [S "DD5 (" :+: (makeRef (Definition (Data nonFL))) :+: S ")", S "", S "",
  S "", S "", S "", S "", S "X", S "", S "", S "", S "", S "", S "X", S ""],
  [S "DD6 (" :+: (makeRef (Definition (Data glaTyFac))) :+: S ")", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "DD7 (" :+: (makeRef (Definition (Data dL))) :+: S ")", S "", S "", S "",
  S "", S "X", S "", S "X", S "", S "", S "", S "X", S "", S "", S ""],
  [S "DD8 (" :+: (makeRef (Definition (Data tolPre))) :+: S ")", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "X"],
  [S "DD9 (" :+: (makeRef (Definition (Data tolStrDisFac))) :+: S ")", S "",
  S "", S "", S "", S "", S "", S "X", S "X", S "", S "", S "", S "", S "",
  S ""]]
  (S "Traceability Matrix Showing the Connections Between Items of Different"
  +:+ S "Sections") True

-- FIXME: Same goes for this one (see above)
s9_table2 = Table [S "", S "T1 (" :+: 
  (makeRef (Definition (Theory t1SafetyReq))) :+: S ")", S "T2 (" :+: 
  (makeRef (Definition (Theory t2SafetyReq))) :+: S ")", S "IM1 (" :+:
  (makeRef (Definition (Theory probOfBr))) :+: S ")", S "IM2 (" :+:
  (makeRef (Definition (Theory calOfCap))) :+: S ")", S "IM3 (" :+:
  (makeRef (Definition (Theory calOfDe))) :+: S ")", S "DD1 (" :+:
  (makeRef (Definition (Data risk))) :+: S ")", S "DD2 (" :+:
  (makeRef (Definition (Data hFromt))) :+: S ")", S "DD3 (" :+:
  (makeRef (Definition (Data loadDF))) :+: S ")", S "DD4 (" :+:
  (makeRef (Definition (Data strDisFac))) :+: S ")", S "DD5 (" :+:
  (makeRef (Definition (Data nonFL))) :+: S ")", S "DD6 (" :+:
  (makeRef (Definition (Data glaTyFac))) :+: S ")", S "DD7 (" :+:
  (makeRef (Definition (Data dL))) :+: S ")", S "DD8 (" :+:
  (makeRef (Definition (Data tolPre))) :+: S ")", S "DD9 (" :+:
  (makeRef (Definition (Data tolStrDisFac))) :+: S ")", S "Data Constraints (" 
  :+: (makeRef s6_2_5) :+: S ")", S "R1 (in" +:+ (makeRef s7_1) :+: S ")",
  S "R2 (in" +:+ (makeRef s7_1) :+: S ")"]
  [[S "R1 (in" +:+ (makeRef s7_1) :+: S ")", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "R2 (in" +:+ (makeRef s7_1) :+: S ")", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "R3 (in" +:+ (makeRef s7_1) :+: S ")", S "", S "", S "", S "", S"",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "X", S "", S ""],
  [S "R4 (in" +:+ (makeRef s7_1) :+: S ")", S "", S "", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "X", S "X"],
  [S "R5 (in" +:+ (makeRef s7_1) :+: S ")", S "X", S "X", S "", S "", S "",
  S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
  [S "R6 (in" +:+ (makeRef s7_1) :+: S ")", S "", S "", S "X", S "X", S "X",
  S "", S "X", S "X", S "X", S "X", S "X", S "X", S "X", S "X", S "", S "",
  S ""]]
  (S "Traceability Matrix Showing the Connections Between Requirements and"
  +:+ S "Other Items") True

-- FIXME: Same goes for this one (see above)
s9_table3 = Table [S "", S "A1 (in" +:+ (makeRef s6_2_1) :+: S ")",
  S "A2 (in" +:+ (makeRef s6_2_1) :+: S ")", S "A3 (in" +:+ 
  (makeRef s6_2_1) :+: S ")", S "A4 (in" +:+ (makeRef s6_2_1) :+: S ")",
  S "A5 (in" +:+ (makeRef s6_2_1) :+: S ")", S "A6 (in" +:+
  (makeRef s6_2_1) :+: S ")", S "A7 (in" +:+ (makeRef s6_2_1) :+: S ")",
  S "A8 (in" +:+ (makeRef s6_2_1) :+: S ")"]
  [[S "T1 (" :+: (makeRef (Definition (Theory t1SafetyReq))) :+: S ")", S "",
  S "", S "", S "", S "", S "", S "", S ""],
  [S "T2 (" :+: (makeRef (Definition (Theory t2SafetyReq))) :+: S ")", S "",
  S "", S "", S "", S "", S "", S "", S ""],
  [S "IM1 (" :+: (makeRef (Definition (Theory probOfBr))) :+: S ")", S "",
  S "", S "", S "X", S "", S "X", S "X", S ""],
  [S "IM2 (" :+: (makeRef (Definition (Theory calOfCap))) :+: S ")", S "",
  S "", S "", S "", S "X", S "", S "", S ""],
  [S "IM3 (" :+: (makeRef (Definition (Theory calOfDe))) :+: S ")", S "",
  S "", S "", S "", S "", S "", S "", S ""],
  [S "DD1 (" :+: (makeRef (Definition (Data risk))) :+: S ")", S "", S "",
  S "", S "", S "", S "", S "", S ""],
  [S "DD2 (" :+: (makeRef (Definition (Data hFromt))) :+: S ")", S "", S "",
  S "", S "", S "", S "", S "", S ""],
  [S "DD3 (" :+: (makeRef (Definition (Data loadDF))) :+: S ")", S "", S "",
  S "", S "X", S "", S "", S "", S "X"],
  [S "DD4 (" :+: (makeRef (Definition (Data strDisFac))) :+: S ")", S "", S "",
  S "", S "", S "", S "", S "", S ""],
  [S "DD5 (" :+: (makeRef (Definition (Data nonFL))) :+: S ")", S "", S "",
  S "", S "X", S "", S "", S "", S ""],
  [S "DD6 (" :+: (makeRef (Definition (Data glaTyFac))) :+: S ")", S "", S "",
  S "", S "", S "", S "", S "", S ""],
  [S "DD7 (" :+: (makeRef (Definition (Data dL))) :+: S ")", S "", S "", S "",
  S "", S "X", S "", S "", S ""],
  [S "DD8 (" :+: (makeRef (Definition (Data tolPre))) :+: S ")", S "", S "",
  S "", S "", S "", S "", S "", S ""],
  [S "DD9 (" :+: (makeRef (Definition (Data tolStrDisFac))) :+: S ")", S "",
  S "", S "", S "X", S "", S "", S "", S ""],
  [S "LC1 (in" +:+ (makeRef s8) :+: S ")", S "", S "", S "X", S "", S "",
  S "", S "", S ""],
  [S "LC2 (in" +:+ (makeRef s8) :+: S ")", S "", S "", S "", S "X", S "",
  S "", S "", S "X"],
  [S "LC3 (in" +:+ (makeRef s8) :+: S ")", S "", S "", S "", S "", S "X",
  S "", S "", S ""],
  [S "LC4 (in" +:+ (makeRef s8) :+: S ")", S "", S "", S "", S "", S "",
  S "X", S "", S ""],
  [S "LC5 (in" +:+ (makeRef s8) :+: S ")", S "", S "", S "", S "", S "",
  S "", S "X", S ""],
  [S "R1 (in" +:+ (makeRef s7_1) :+: S ")", S "", S "", S "", S "", S "",
  S "", S "", S ""],
  [S "R2 (in" +:+ (makeRef s7_1) :+: S ")", S "", S "", S "", S "X", S "X",
  S "", S "", S "X"],
  [S "R3 (in" +:+ (makeRef s7_1) :+: S ")", S "", S "", S "", S "", S "",
  S "", S "", S ""],
  [S "R4 (in" +:+ (makeRef s7_1) :+: S ")", S "", S "", S "", S "", S "",
  S "", S "", S ""],
  [S "R5 (in" +:+ (makeRef s7_1) :+: S ")", S "", S "", S "", S "", S "",
  S "", S "", S ""],
  [S "R6 (in" +:+ (makeRef s7_1) :+: S ")", S "", S "", S "", S "", S "",
  S "", S "", S ""]]
  (S "Traceability Matrix Showing the Connections Between Assumptions and"
  +:+ S "Other Items") True

s9_intro2 = 
  [Paragraph $
  S "The purpose of the traceability graphs is also to provide easy" +:+ 
  S "references on what has to be additionally modified if a certain" +:+
  S "component is changed. The arrows in the graphs represent" +:+
  S "dependencies. The component at the tail of an arrow is depended on" +:+
  S "by the component at the head of that arrow. Therefore, if a" +:+
  S "component is changed, the components that it points to should also" +:+
  S "be changed. Figure 2" +:+ sParen (makeRef fig_2) +:+ S "shows the" +:+
  S "dependencies of" +:+ (sMap (map toLower) (thModel ^. defn)) :+: 
  S "s" `sC` (sMap (map toLower) (dataDefn ^. defn)) :+: S "s and" +:+
  (sMap (map toLower) (inModel ^. defn)) :+: S "s on each other." +:+
  S "Figure 3" +:+ sParen (makeRef fig_3) +:+ S "shows the dependencies of" +:+
  (sMap (map toLower) (requirement ^. defn)) :+: S "s on" +:+
  (sMap (map toLower) (thModel ^. defn)) :+: S "s" `sC` 
  (sMap (map toLower) (inModel ^. defn)) :+: S "s" `sC`
  (sMap (map toLower) (dataDefn ^. defn)) :+: S "s and data constraints." +:+
  S "Figure 4" +:+ sParen (makeRef fig_4) +:+ S "shows the dependencies of" +:+
  (sMap (map toLower) (thModel ^. defn)) :+: S "s" `sC` 
  (sMap (map toLower) (inModel ^. defn)) :+: S "s" `sC`
  (sMap (map toLower) (dataDefn ^. defn)) :+: S "s" `sC` 
  (sMap (map toLower) (requirement ^. defn)) :+: S "s and" +:+
  (sMap (map toLower) (likelyChg ^. defn)) :+: S "s on" +:+
  (sMap (map toLower) (assumption ^. defn)) :+: S "s.",
  Paragraph $ 
  S "NOTE: Building a tool to automatically generate the graphical" +:+
  S "representation of the matrix by scanning the labels and reference" +:+.
  S "can be future work"]

fig_2 = Figure (S "Figure 2: Traceability Matrix Showing the Connections" +:+
  S "Between Items of Different Sections") "Trace.png"

fig_3 = Figure (S "Figure 3: Traceability Matrix Showing the Connections" +:+
  S "Between" +:+ (requirement ^. defn) :+: S "s and Other Items") 
  "RTrace.png"

fig_4 = Figure (S "Figure 4: Traceability Matrix Showing the Connections" +:+
  S "Between" +:+ (assumption ^. defn) :+: S "s and Other Items")
  "ATrace.png"

s10 = Section(S "References") [Con s10_list]

s10_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(S "[1]", S "N. Koothoor" `sC` Quote (S "A document drive approach to" +:+
  S "certifying scientific computing software,") +:+ S "Master's thesis" `sC`
  S "McMaster University, Hamilton, Ontario, Canada, 2013."),
  (S "[2]", S "W. S. Smith and L. Lai" `sC` Quote (S "A new requirements" +:+
  S "template for scientific computing,") +:+ S "in Proceedings of the" +:+
  S "First International Workshop on Situational Requirements Engineering" +:+
  S "Processes - Methods, Techniques and Tools to Support Situation-Specific"
  +:+ S "Requirements Engineering Processes, SREP'05 (J.Ralyt" :+: 
  (F Acute 'e') :+: S ", P.Agerfalk, and N.Kraiem, eds.), (Paris, France),"
  +:+ S "pp. 107-121, In conjunction with 13th IEEE International" +:+
  S "Requirements Engineering Conference, 2005."),
  (S "[3]", S "J. Robertson and S. Robertson" `sC` Quote (S "Volere ":+:
  S "requirements specification template edition 16.") :+: S "" +:+ 
  Quote (S "www.cs.uic.edu/ i442/VolereMaterials/templateArchive16/c" +:+ 
  S "Volere template16.pdf") :+: S ", 2012."),
  (S "[4]", S "ASTM Standards Committee" `sC` Quote (S "Standard practice"
  +:+ S "for determining load resistance of glass in buildings,") :+: 
  S " Standard E1300-09a, American Society for Testing and Material (ASTM),"
  +:+ S "2009."),
  (S "[5]", S "ASTM, developed by subcommittee C1408,Book of standards 15.02,"
  +:+ Quote (S "Standard specification for flat glass,C1036.")),
  (S "[6]", S "ASTM, developed by subcommittee C14.08,Book of standards" +:+
  S "15.02" `sC` Quote (S "Specification for heat treated flat glass-Kind"
  +:+ S "HS, kind FT coated and uncoated glass,C1048."))]

s11 = Section(S "Appendix") [Con s11_intro, Con fig_5, Con fig_6]

s11_intro = Paragraph $
  S "This appendix holds the graphs" +:+ sParen ((makeRef fig_5) +:+ S "and" +:+
  (makeRef fig_6)) +:+. S "used for interpolating values needed in the models"

fig_5 = Figure (S "Figure 5:" +:+ (demandq ^. defn) +:+ sParen
  (P (demand ^. symbol)) +:+ S "versus" +:+ (sD ^. term) +:+
  S "versus" +:+ (char_weight ^. term) +:+ sParen
  (P (sflawParamM ^. symbol))) "ASTM_F2248-09.png"

fig_6 = Figure (S "Figure 6: Non dimensional" +:+ 
  (sMap (map toLower) (lateral ^. term)) +:+
  (sMap (map toLower) (load ^. term)) +:+ sParen
  (P (dimlessLoad ^. symbol)) +:+ S "versus" +:+ (ar ^. term) +:+ 
  sParen (P (ar ^. symbol)) +:+ S "versus" +:+ (sdf ^. term) +:+ 
  sParen (P (sdf ^. symbol))) "ASTM_F2248-09_BeasonEtAl.png"
