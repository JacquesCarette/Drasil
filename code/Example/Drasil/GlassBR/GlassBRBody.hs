{-# OPTIONS -Wall #-} 
{-# LANGUAGE FlexibleContexts #-} 
module Example.Drasil.GlassBR.GlassBRBody where
import Data.Char (toLower)
import Control.Lens ((^.))

import Language.Drasil

import Example.Drasil.GlassBR.GlassBRExample
import Example.Drasil.GlassBR.GlassBRSIUnits 

this_si :: [UnitDefn]
this_si = map UU [metre, second] ++ map UU [pascal, newton]

s1, s1_1,  s1_2, s1_3, s2, s2_1, s2_2, s2_3, s3, s3_1, s3_2, s4, s4_1, s4_2,
  s5, s5_1, s5_2, s6, s6_1, s6_1_1, s6_1_2, s6_1_3, s6_2, s6_2_1, s6_2_2, 
  s6_2_3, s6_2_4, s6_2_5, s7, s7_1, s7_2, s8, s9, s10 :: Section
s1_intro, s1_1_intro, s1_1_table, s1_2_intro, s1_2_table, s1_3_table, s2_intro,
  s2_1_intro1, s2_1_intro2, s2_2_intro, s2_3_intro1, s2_3_intro2, s3_intro, 
  s3_1_intro, s3_2_intro, s4_intro, s4_1_bullets, s4_2_intro, s5_intro, 
  s5_1_table, s5_2_bullets, s6_intro, s6_1_intro, s6_1_1_intro, s6_1_1_bullets,
  s6_1_2_intro, s6_1_2_list, s6_1_3_list, s6_2_intro, s6_2_1_intro, 
  s6_2_1_list1, s6_2_1_eq1, s6_2_1_eq2, s6_2_1_eq3, s6_2_1_eq4 ,s6_2_1_list2, 
  s6_2_4_intro, s6_2_5_intro, --s6_2_5_table1, 
  s6_2_5_table2, s6_2_5_intro2, --s6_2_5_table3, 
  s7_1_intro, s7_1_list1, s7_1_table, s7_1_list2, s7_2_intro, 
  s8_list, s9_list, s10_intro, fig_glassbr, fig_2, fig_3 :: Contents


glassBR_srs :: Document  
glassBR_srs = Document ((softwareRS ^. descr) :+: S " for " :+: 
  (gLassBR ^. descr)) (S "Nikitha Krithnan and Spencer Smith") 
  [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10]

s1 = Section 0 (S "Reference Material") [Con s1_intro, Sub s1_1, Sub s1_2, 
  Sub s1_3]

s1_intro = Paragraph (S "This section records information for easy reference.")

s1_1 = Section 1 (S "Table of Units") [Con s1_1_intro, Con s1_1_table]

s1_1_intro = Paragraph $ 
  S "Throughout this document SI (Syst" :+: (F Grave 'e') :+: S "me " :+:
  S "International d'Unit" :+: (F Acute 'e') :+: S "s) is employed as " :+:
  S "the unit system. In addition to the basic units, several derived " :+:
  S "units are employed as described below. For each unit, the symbol is " :+: 
  S "given followed by a description of the unit followed by the SI name."

s1_1_table = Table [S "Symbol", S "Description", S "Name"] (mkTable
  [(\x -> Sy (x ^. unit)),
   (\x -> (x ^. descr)),
   (\x -> S (x ^. name))
  ] this_si)
  (S "Table of Units") True

s1_2 = Section 1 (S "Table of Symbols") [Con s1_2_intro, Con s1_2_table]

s1_2_intro = Paragraph $ 
  S "The table that follows summarizes the symbols used in this " :+:
  S "document along with their units. The symbols are listed in " :+:
  S "alphabetical order." 
  
s1_2_table = Table [S "Symbol", S "Units", S "Description"] (mkTable
  [(\ch -> P (ch ^. symbol)),  
   (\ch -> Sy $ ch ^. unit),
   (\ch -> ch ^. descr)
   ]
  glassBRSymbols)
  (S "Table of Symbols") False

s1_3 = Section 1 (S "Abbreviations and Acronyms") [Con s1_3_table]

s1_3_table = Table [S "Abbreviations", S "Full Form"] (mkTable
  [(\ch -> S $ ch ^. name),
   (\ch -> ch ^. descr)]
  acronyms)
  (S "Abbreviations and Acronyms") False

s2 = Section 0 (S "Introduction") [Con s2_intro, Sub s2_1, Sub s2_2, Sub s2_3]

s2_intro = Paragraph $ 
  S "Software is helpful to efficiently and correctly predict the blast " :+:
  S "risk involved with the " :+: (sMap (map toLower) (glaSlab ^. descr)) :+: 
  S ". The " :+: (sMap (map toLower) (S $ blast ^. name)) :+: S " under " :+:
  S "consideration is " :+: (sMap (map toLower) (blast ^. descr)) :+: S "." :+: 
  S " The software, herein called " :+: (gLassBR ^. descr) :+: S " aims to " :+:
  S "predict the blast risk involved with the " :+: 
  (sMap (map toLower) (glaSlab ^.descr)) :+: S " using an intuitive " :+:
  S "interface. The following section provides an overview of the " :+: 
  (softwareRS ^. descr) :+: S " (" :+: S (softwareRS ^. name) :+: S ") for "
  :+: (gLassBR ^. descr) :+: S ". This section explains the purpose of the " 
  :+: S "document is designed to fulfil, the scope of the requirements and " 
  :+: S "the organization of the document: what the document is based on and "
  :+: S "intended to portray."

s2_1 = Section 1 (S "Purpose of Document") [Con s2_1_intro1, Con s2_1_intro2]

s2_1_intro1 = Paragraph $
  S "The main purpose of this document is to predict whether a given " :+:
  (sMap (map toLower) (glaSlab ^. descr)) :+: S " is likely to resist a " :+:
  S "specified " :+: (sMap (map toLower) (S $ blast ^. name)) :+: S ". " :+:
  S "The goals and " :+: (sMap (map toLower) (theoreticMod ^. descr)) :+:
  S "s used in the " :+: (gLassBR ^. descr) :+: S " code are provided, " :+:
  S "with an emphasis on explicitly identifying " :+: 
  (sMap (map toLower) (assumption ^. descr)) :+: S "s and unambiguous " :+:
  S "definitions. This document is intended to be used as a reference " :+:
  S "to provide all information necessary to understand and verify the " :+:
  S "analysis. The " :+: S (softwareRS ^. name) :+: S " is abstract " :+:
  S "because the contents say what problem is being solved, but not how " :+:
  S "to solve it."

s2_1_intro2 = Paragraph $
  S "This document will be used as a starting point for subsequent " :+:
  S "development phases, including writing the design specification and " :+:
  S "the software verification and validation plan. The design document " :+:
  S "will show how the requirements are to be realized, including " :+:
  S "decisions on the numerical algorithms and programming environment. " :+:
  S "The verification and validation plan will show the steps that will " :+:
  S "be used to increase confidence in the software documentation and " :+:
  S "the implementation."

s2_2 = Section 1 (S "Scope of Requirements") [Con s2_2_intro]

s2_2_intro = Paragraph $
  S "The scope of the requirements includes getting all input parameters " :+:
  S "related to the " :+: (sMap (map toLower) (glaSlab ^. descr)) :+: 
  S " and also the parameters related to " :+: 
  (sMap (map toLower) (S $ blastTy ^. name)) :+: S ". Given the input, " :+:
  (gLassBR ^. descr) :+: S " is intended to use the data and predict " :+:
  S "whether the " :+: (sMap (map toLower) (glaSlab ^. descr)) :+: 
  S " is safe to use or not."

s2_3 = Section 1 (S "Organization of Document") [Con s2_3_intro1, 
  Con s2_3_intro2]

s2_3_intro1 = Paragraph $
  S "The organization of this document follows the template for an " :+: 
  S (softwareRS ^. name) :+: S " for scientific computing software " :+:
  S "proposed by [1] and [2] (in " :+: (makeRef s9) :+: S "), with " :+: 
  S "some aspects taken from Volere template 16 [3]. The presentation " :+:
  S "follows the standard pattern of presenting goals, theories, " :+:
  S "definitions, and " :+: (sMap (map toLower) (assumption ^. descr)) :+:
  S "s. For readers that would like a more bottom up approach, they can " :+:
  S "start reading the " :+: (sMap (map toLower) (dataDefn ^. descr)) :+:
  S "s in " :+: (makeRef s6_2_4) :+: S " and trace back to find any " :+:
  S "additional information they require."

s2_3_intro2 = Paragraph $
  S "The " :+: (sMap (map toLower) (goalStmt ^. descr)) :+: S "s are " :+:
  S "refined to the " :+: (sMap (map toLower) (theoreticMod ^. descr)) :+:
  S "s, and " :+: (sMap (map toLower) (theoreticMod ^. descr)) :+: 
  S "s to the " :+: (sMap (map toLower) (instanceMod ^. descr)) :+: 
  S "s. The " :+: (sMap (map toLower) (dataDefn ^. descr)) :+: S "s " :+:
  S "are used to support the definitions of the different models." 

s3 = Section 0 (S "Stakeholders") [Con s3_intro, Sub s3_1, Sub s3_2]

s3_intro = Paragraph $
  S "This section describes the Stakeholders: the people who have an " :+:
  S "interest in the product."

s3_1 = Section 1 (S "The Client") [Con s3_1_intro]

s3_1_intro = Paragraph $
  S "The client for " :+: (gLassBR ^. descr) :+: S " is a company named " :+:
  S "Entuitive. It is developed by Dr. Manuel Campidelli. The client has " :+:
  S "the final say on acceptance of the product."

s3_2 = Section 1 (S "The Customer") [Con s3_2_intro]

s3_2_intro = Paragraph $
  S "The customers are the end user of " :+: (gLassBR ^. descr) :+: S "."

s4 = Section 0 (S "General System Description") [Con s4_intro, Sub s4_1, 
  Sub s4_2]

s4_intro = Paragraph $
  S "This section provides general information about the system, " :+:
  S "identifies the interface between the system and its environment, " :+:
  S "and describes the user characteristics and the system constraints."

s4_1 = Section 1 (S "User Characteristics") [Con s4_1_bullets]

s4_1_bullets = Enumeration $ Bullet $ map Flat
  [(S "The end user of " :+: (gLassBR ^. descr) :+: S " is expected to " :+:
    S "have completed at least the equivalent of the second year of an " :+:
    S "undergraduate degree in civil or structural engineering."),
  (S "The end user is expected to have an understanding of theory behind " :+:
    (sMap (map toLower) (S (gbr ^. name))) :+: S " and blast risk."),
  (S "The end user is expected to have basic computer literacy to handle " :+:
    S "the software.")]

s4_2 = Section 1 (S "System Constraints") [Con s4_2_intro]

s4_2_intro = Paragraph $
  S (notApp ^. name)

s5 = Section 0 (S "Scope of the Project") [Con s5_intro, Sub s5_1, Sub s5_2]

s5_intro = Paragraph $
  S "This section presents the scope of the project. It describes the " :+:
  S "expected use of " :+: (gLassBR ^. descr) :+: S " as well as the " :+:
  S "inputs and outputs of each action. The use cases are input and " :+:
  S "output, which defines the action of getting the input and displaying " :+:
  S "the output."

s5_1 = Section 1 (S "Product Use Case Table") [Con s5_1_table]

s5_1_table = Table [S "Use Case NO.", S "Use Case Name", S "Actor", 
  S "Input and Output"] (mkTable
  [(\x -> (x!!0)),(\x -> (x!!1)), (\x -> (x!!2)), (\x -> (x!!3))]
  [[S "1", S "Inputs", S "User", S "Characteristics of the " :+:
  (sMap (map toLower) (glaSlab ^. descr)) :+: S " and of the " :+:
  (sMap (map toLower) (S $ blast ^. name)) :+: S ". Details in " :+: 
  (makeRef s5_2)],
  [S "2", S "Output", (gLassBR ^. descr), S "Whether or not the " :+:
  (sMap (map toLower) (glaSlab ^. descr)) :+: S " is safe for the calculated "
  :+: (sMap (map toLower) (S $ load ^. name)) :+: S " and supporting " :+:
  S "calculated values"]])
  (S "Table 1: Use Case Table") True

s5_2 = Section 1 (S "Individual Product Use Cases") [Con s5_2_bullets]

s5_2_bullets = Enumeration $ Bullet $ map Flat
  [(S "Use Case 1 refers to the user providing input to " :+: 
    (gLassBR ^. descr) :+: S " for use within the analysis. There are two " :+:
    S "classes of inputs: " :+: (sMap (map toLower) (S $ glassGeo ^. name)) :+:
    S " and " :+: (sMap (map toLower) (S $ blastTy ^. name)) :+: S ". " :+:
    (glassGeo ^. descr) :+: S " " :+: (blastTy ^. descr) :+: S " These " :+:
    S "parameters describe " :+: (sMap (map toLower) (char_weight ^. descr)) 
    :+: S " and stand off " :+: (sMap (map toLower) (S $ blast ^. name)) :+:
    S ". Another input the user gives is the tolerable value of " :+:
    (sMap (map toLower) (prob_br ^. descr)) :+: S "."),
  (S " Use Case 2 " :+: (gLassBR ^. descr) :+: S " outputs if the " :+:
    (sMap (map toLower) (glaSlab ^. descr)) :+: S " will be safe by " :+:
    S "comparing whether " :+: (sMap (map toLower) (S $ capacity ^. name)) :+:
    S " is greater than " :+: (sMap (map toLower) (S $ demandq ^. name)) :+:
    S ". " :+: (S $ capacity ^.name) :+: S " is the " :+:
    (sMap (map toLower) (capacity ^. descr)) :+: S " and " :+:
    (sMap (map toLower) (S $ demandq ^. name)) :+: S " is the requirement " :+:
    S " which is the " :+: (demandq ^. descr) :+: S ". The second condition "
    :+: S "is to check whether the calculated probability (" :+: 
    (P $ prob_br ^. symbol) :+: S ") is less than the tolerable probability (" 
    :+: (P $ pb_tol ^. symbol) :+: S ") which is obtained from the user " :+:
    S "as an input. If both conditions return true then it's shown that the " 
    :+: (sMap (map toLower) (glaSlab ^. descr)) :+: S " is safe to use, " :+: 
    S "else if both return false then the " :+: 
    (sMap (map toLower) (glaSlab ^. descr)) :+: S " is considered unsafe. " :+:
    S "All the supporting calculated values are also displayed as output.")]

s6 = Section 0 (S "Specific System Description") [Con s6_intro, Sub s6_1,
  Sub s6_2]

s6_intro = Paragraph $ 
  S "This section first presents the problem description, which gives a " :+:
  S "high-level view of the problem to be solved. This is followed by " :+:
  S "the solution characteristics specification, which presents the " :+:
  (sMap (map toLower) (assumption ^. descr)) :+: S "s, theories, definitions."

s6_1 = Section 1 (S "Problem Description") [Con s6_1_intro, Sub s6_1_1, 
  Sub s6_1_2, Sub s6_1_3]

s6_1_intro = Paragraph $ 
  S "A system is needed to efficiently and correctly predict the blast " :+:
  S "risk involved with the glass. " :+: (gLassBR ^. descr) :+: S " is a " :+:
  S "computer program developed to interpret the inputs to give out the " :+:
  S "outputs which predicts whether the " :+: 
  (sMap (map toLower) (glaSlab ^. descr)) :+: S " can withstand the " :+:
  (sMap (map toLower) (S $ blast ^. name)) :+: S " under the conditions."

s6_1_1 = Section 2 (S "Terminology and Definitions") [Con s6_1_1_intro, 
  Con s6_1_1_bullets]
  
s6_1_1_intro = Paragraph $ 
  S "This subsection provides a list of terms that are used in subsequent "
  :+: S "sections and their meaning, with the purpose of reducing ambiguity "
  :+: S "and making it easier to correctly understand the requirements. " :+:
  S "All of the terms are extracted from [4] in " :+: (makeRef s9) :+: S "."

s6_1_1_bullets = Enumeration $ (Number $ 
  map (\c -> Flat $ S (c ^. name) :+: S " - " :+: (c ^. descr)) [aR, gbr, lite] 
  ++ [Nested (S (glassTy ^. name) :+: S ":") 
  (Bullet $ map (\c -> Flat $ S (c ^. name) :+: S " - " :+: (c ^. descr)) 
    [an, ft, hs])] ++
  map (\c -> Flat $ S (c ^. name) :+: S " - " :+: (c ^. descr)) [gtf,lateral] 
  ++ [Nested (S (load ^. name) :+: S":") 
  (Bullet $ map (\c -> Flat $ S (c ^. name) :+: S " - " :+:(c ^. descr)) 
    [specDeLoad, lr, ldl, nfl, glassWL, sdl])] ++ 
  map (\c -> Flat $ S (c ^. name) :+: S " - " :+: (c ^. descr)) 
    [lsf, pb, specA, blaReGLa, eqTNTChar, sD])
  
s6_1_2 = Section 2 (physSysDescr ^. descr) [Con s6_1_2_intro, Con s6_1_2_list, 
  Con fig_glassbr]

s6_1_2_intro = Paragraph $ S "The physical system of " :+: (gLassBR ^. descr) 
  :+: S " as shown in " :+: (makeRef fig_glassbr) :+: S ", includes the " :+:
  S "following elements:"

fig_glassbr = Figure (S "The physical system") "physicalsystimage.png"
  
s6_1_2_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "PS1", (glaSlab ^. descr)), 
  (S "PS2", S "The point of explosion. Where the bomb, or " :+:
    (sMap (map toLower) (blast ^. descr)) :+: S ", is located. The " :+: 
    (sMap (map toLower) (S (take 18 (sD ^. name)))) :+: S "  is the distance" 
    :+: S " between the point of explosion and the glass.")]

s6_1_3 = Section 2 ((goalStmt ^. descr) :+: S "s") [Con s6_1_3_list]

s6_1_3_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "GS1", S "Analyze and predict whether the " :+:
    (sMap (map toLower) (glaSlab ^. descr)) :+: S " under consideration " :+:
    S "will be able to withstand the explosion of a certain degree which " :+:
    S "is calculated based on user input.")]

s6_2 = Section 1 (S "Solution Characteristics Specification") 
  [Con s6_2_intro, Sub s6_2_1, Sub s6_2_2, Sub s6_2_3, Sub s6_2_4, Sub s6_2_5]

s6_2_intro = Paragraph $ S "This section explains all the " :+:
  (sMap (map toLower) (assumption ^. descr)) :+: S "s considered and the " :+:
  (sMap (map toLower) (theoreticMod ^. descr)) :+: S "s which are " :+:
  S "supported by the " :+: (sMap (map toLower) (dataDefn ^. descr)) :+: S "s."
  
s6_2_1 = Section 2 (assumption ^. descr :+: S "s") [Con s6_2_1_intro, 
  Con s6_2_1_list1, Con s6_2_1_eq1, Con s6_2_1_eq2, Con s6_2_1_eq3, 
  Con s6_2_1_eq4, Con s6_2_1_list2]

s6_2_1_intro = Paragraph $ 
  S "This section simplifies the original problem and helps in developing the " 
  :+: (sMap (map toLower) (theoreticMod ^. descr)) :+: 
  S (" " ++ sqbrac (theoreticMod ^. name)) :+: S " by filling in the missing " 
  :+: S "information for the physical system. The numbers given in the " :+:
  S "square brackets refer to the " :+: 
  (sMap (map toLower) (dataDefn ^. descr)) :+:
  S (" " ++ sqbrac (dataDefn ^. name)) :+: S ", or " :+:
  (sMap (map toLower) (instanceMod ^. descr)) :+: 
  S (" " ++ sqbrac (instanceMod ^. name)) :+: S ", in which the respective " 
  :+: (sMap (map toLower) $ assumption ^. descr) :+: S " is used."

s6_2_1_list1 = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "A1", S "The standard E1300-09a for calculation applies only to " :+:
    S "monolithic, laminated, or insulating glass constructions of " :+:
    S "rectangular shape with continuous " :+: 
    (sMap (map toLower) (S $ lateral ^. name)) :+:
    S " support along one, two, three, or four edges. This practice assumes " 
    :+: S "that (1) the supported glass edges for two, three and four-sided " 
    :+: S "support conditions are simply supported and free to slip in plane; "
    :+: S "(2) glass supported on two sides acts as a simply supported beam " 
    :+: S "and (3) glass supported on one side acts as a cantilever."), 
  (S "A2", S "This practice does not apply to any form of wired, patterned, "
    :+: S "etched, sandblasted, drilled, notched, or grooved glass with " :+:
    S "surface and edge treatments that alter the glass strength."),
  (S "A3", S "This system only considers the external explosion scenario " :+:
    S "for its calculations."),
  (S "A4", S "Standard values used for calculation in " :+: (gLassBR ^. descr) 
    :+: S " are: ")]
    
s6_2_1_eq1 = EqnBlock $ (C sflawParamM):=(Int 7)
s6_2_1_eq2 = EqnBlock $ (C sflawParamK):=(Grouping (Dbl 2.86)):*(Int 10)
  :^(Neg (Int 53))
s6_2_1_eq3 = EqnBlock $ (C mod_elas):=(Grouping (Dbl 7.17)):*(Int 10):^(Int 7)
s6_2_1_eq4 = EqnBlock $ (C load_dur):=(Int 3)

  --  (Number $ map (\c -> Flat c) [
  --  (P $ sflawParamM ^. symbol) :+: S " = 7 " :+: Sy (sflawParamM ^. unit), 
  --  (P $ sflawParamK ^. symbol) :+: S " = 2.86 * 10^(-53) " :+: Sy (sflawParamK ^. unit), 
  --  (P $ mod_elas ^. symbol) :+: S " = 7.17 * 10^7 " :+: Sy (mod_elas ^. unit),
  --  (P $ load_dur ^. symbol) :+: S " = 3 " :+: Sy (load_dur ^. unit)]))] ++
s6_2_1_list2 = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "A5", S "Glass under consideration is assumed to be a single " :+:
    (sMap (map toLower) (S $ lite ^. name)) :+: S ". Hence the value of " :+: 
    (P $ loadSF ^. symbol) :+: S " is equal to 1 for all calculations in " :+: 
    (gLassBR ^. descr) :+: S "."),
  (S "A6", S "Boundary conditions for the " :+: 
    (sMap (map toLower) (glaSlab ^. descr)) :+: S " is assumed to be 4-sided "
    :+: S "support for calculations."),
  (S "A7", S "The response type considered in " :+: (gLassBR ^. descr) :+: 
    S " is flexural."),
  (S "A8", S "With reference to A4 the value of " :+:
    (sMap (map toLower) (loadDF ^. descr)) :+: S " (" :+: 
    (P $ loadDF ^. symbol) :+: S ") is a constant in " :+: (gLassBR ^. descr) 
    :+: S ". It is calculated by the equation: " :+: 
    --(P $ loadDF ^. symbol) :+: S " = " :+: (P $ load_dur ^. symbol) :+: 
    S ". Using this, " :+: (P $ loadDF ^. symbol) :+: S " = 0.27.")]
  --equation in sentence

s6_2_2 = Section 2 ((theoreticMod ^. descr) :+: S "s") (map Con s6_2_2_TMods)
  
s6_2_2_TMods :: [Contents]
s6_2_2_TMods = map Definition (map Theory [t1SafetyReq,t2SafetyReq])

s6_2_3 = Section 2 ((instanceMod ^. descr) :+: S "s") (map Con s6_2_3_IMods)

s6_2_3_IMods :: [Contents]
s6_2_3_IMods = map Definition (map Theory [probOfBr,calOfCap,calOfDe])

s6_2_4 = Section 2 ((dataDefn ^. descr) :+: S "s") 
  ((Con s6_2_4_intro):(map Con s6_2_4_DDefns))

s6_2_4_intro = Paragraph $ 
  S "This section collects and defines all the data needed to build the " :+:
  (sMap (map toLower) (instanceMod ^. descr)) :+: S "s."

s6_2_4_DDefns ::[Contents] 
s6_2_4_DDefns = map Definition (map Data [risk,hFromt,loadDF,strDisFac,nonFL,
  glaTyFac,dL,tolPre,tolStrDisFac])

s6_2_5 = Section 2 (S "Data Constraints") [Con s6_2_5_intro, --Con s6_2_5_table1, 
  Con s6_2_5_table2, Con s6_2_5_intro2] --, Con s6_2_5_table3]

s6_2_5_intro = Paragraph $
  S "Table 2 (" :+: --(makeRef s6_2_5_table1) :+: 
  S ") shows the data " :+:
  S "constraints on the input variables. The column of physical constraints "
  :+: S "gives the physical limitations on the range of values that can " :+:
  S " be taken by the variable. The constraints are conservative, to give "
  :+: S "the user of the model the flexibility to experiment with unusual "
  :+: S "situations. The column of typical values is intended to provide " :+:
  S "a feel for a common scenario. The uncertainty column provides an " :+:
  S "estimate of the confidence with which the physical quantities can be " 
  :+: S "measured. This information would be part of the input if one were " 
  :+: S "performing an uncertainty quantification exercise. Table 3 (" :+:
  (makeRef s6_2_5_table2) :+: S ") gives the values of the specification " :+:
  S "parameters used in Table 2 (" :+: --(makeRef s6_2_5_table1) :+: 
  S "). " :+: 
  (P $ ar_max ^. symbol) :+: S " refers to the " :+:
  (sMap (map toLower) (ar_max ^. descr)) :+: S " for the plate of glass."

-- s6_2_5_table1 = Table [S "Var", S "Physical Cons", S "Software Constraints", S "Typical Value",
--  S "Uncertainty"] (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2), (\x -> x!!3),
--  (\x -> x!!4)] [[(P $ plate_len ^. symbol), (P $ plate_len ^. symbol) :+: S " > 0 and " :+: 
--  (P $ plate_len ^. symbol) :+: S "/" :+: (P $ plate_width ^. symbol) :+: S " > 1",
--  (P $ dim_min ^. symbol) :+: S " <= " :+: (P $ plate_len ^. symbol) :+: S " <= " :+: 
--  (P $ dim_max ^. symbol) :+: S " and " :+: (P $ plate_len ^. symbol) :+: S "/" :+: 
--  (P $ plate_width ^. symbol) :+: S " < " :+: (P $ ar_max ^. symbol), S "1500 " :+:
--  Sy (plate_len ^. unit), S "10%"], [(P $ plate_width ^. symbol), (P $ (plate_width ^. symbol)) 
--  :+: S " > 0 and " :+: (P $ plate_width ^. symbol) :+: S " < " :+: (P $ plate_len ^. symbol),
--  (P $ dim_min ^. symbol) :+: S " <= " :+: (P $ plate_width ^. symbol) :+: S " <= " :+: 
--  (P $ dim_max ^.symbol) :+: S " and " :+: (P $ plate_len ^. symbol) :+: S "/" :+: 
--  (P $ plate_width ^. symbol) :+: S " < " :+: (P $ ar_max ^. symbol), S "1200 " :+: 
--  Sy (plate_width ^. unit), S "10%"], [(P $ pb_tol ^. symbol), S "0 < " :+: 
--  (P $ pb_tol ^. symbol) :+: S " < 1", S "-", S "0.008", S "0.1%"], [(P $ char_weight ^. symbol), 
--  (P $ char_weight ^. symbol) :+: S " >= 0", (P $ cWeightMin ^. symbol) :+: S " < " :+: 
--  (P $ char_weight ^. symbol) :+: S " < " :+: (P $ cWeightMax ^. symbol), S "42 " :+: 
--  Sy (char_weight ^. unit), S "10%"],[(P $ tNT ^. symbol), (P $ tNT ^. symbol) :+: 
--  S " > 0", S "-", S "1", S "10%"], [(P $ sd ^. symbol), (P $ sd ^. symbol) :+: S " > 0", 
--  (P $ sd_min ^. symbol) :+: S " < " :+: (P $ sd ^. symbol) :+: S " < " :+: 
--  (P $ sd_max ^. symbol), S "45" :+: Sy (sd ^. unit), S "10%"]])
--  (S "Table 2: Input Variables") True

s6_2_5_table2 = Table [S "Var", S "Value"] (mkTable 
  [(\x -> P $ fst x), (\x -> snd x)] 
  [(dim_min ^. symbol, S "0.1 " :+: Sy (sd ^. unit)), 
  (dim_max ^.symbol, S "0.1 " :+: Sy (sd ^. unit)),(ar_max ^. symbol, S "5"), 
  (cWeightMin ^. symbol, S "4.5 " :+: Sy (cWeightMin ^. unit)),
  (cWeightMax ^. symbol, S "910 " :+: Sy (cWeightMax ^. unit)), 
  (sd_min ^. symbol, S "6 " :+: Sy (sd_min ^. unit)), 
  (sd_max ^. symbol, S "130 " :+: Sy (sd_max ^. unit))])
  (S "Table 3: Specification Parameter Values") True

s6_2_5_intro2 = Paragraph $
  S "Table 4 (" :+: --(makeRef s6_2_5_table3) :+:
  S ") shows the constraints " 
  :+: S "that must be satisfied by the output."

-- s6_2_5_table3 = Table [S "Var", S "Physical Constraints"] (mkTable 
--  [(\x -> P $ fst(x)), (\x -> snd(x))] 
--  [(prob_br ^. symbol, S "0 < " :+: (P $ prob_br ^. symbol) :+: S " < 1")])
--  (S "Table4: Output Variables") True

s7 = Section 0 ((requirement ^. descr) :+: S "s") [Sub s7_1, Sub s7_2]

s7_1 = Section 1 (S "Functional " :+: (requirement ^. descr) :+: S "s") 
  [Con s7_1_intro, Con s7_1_list1, Con s7_1_table, Con s7_1_list2]

s7_1_intro = Paragraph $
  S "The following section provides the functional " :+:
  (requirement ^. descr) :+: S "s, the business tasks that the software " :+:
  S "is expected to complete."


s7_1_list1 = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(S "R1", S "Input the following quantities, which define the glass " :+:
    S "dimensions, type of glass, tolerable probability of failure and " :+:
    S "the characteristics of the " :+: 
    (sMap (map toLower) (S $ blast ^. name)) :+: S ":")]

s7_1_table = Table [S "Symbol", S "Units", S "Description"] (mkTable
  [(\ch -> P (ch ^. symbol)),  
   (\ch -> Sy $ ch ^. unit),
   (\ch -> ch ^. descr)
   ]
  [plate_len,plate_width,glass_type,pb_tol,sdx,sdy,sdz,nom_thick,tNT,
  char_weight])
  (S "Input Parameters") False

s7_1_list2 = Enumeration $ Simple $
  [(S "R2", Nested (S "The system shall set the known values as follows: ") 
    (Bullet $ map (\c -> Flat c) 
      [(P $ sflawParamM ^. symbol) :+: S ", " :+: (P $ sflawParamK ^. symbol) 
      :+: S ", " :+: (P $ mod_elas ^. symbol) :+: S ", " :+: 
      (P $ load_dur ^. symbol) :+: S " following A4",
      (P $ loadDF ^. symbol) :+: S " following A8",
      (P $ loadSF ^. symbol) :+: S " following A5"]))] ++
  map (\(a,b) -> (a, Flat b))
  [(S "R3", S "The system shall check the entered input values to ensure " :+:
    S "that they do not exceed the data constraints mentioned in " :+: 
    (makeRef s6_2_5) :+: S ". If any of the input parameters is out of " :+:
    S "bounds, an error message is displayed and the calculations stop."),
  (S "R4", S "Output the input quantities from R1 and the known quantities "
    :+: S "from R2."),
  (S "R5", S "If " :+: (P $ is_safe1 ^. symbol) :+: S " and " :+:
    (P $ is_safe2 ^. symbol) :+: S " (from " :+: 
    (makeRef (Definition (Theory t1SafetyReq))) :+: S " T1 and " :+: 
    (makeRef (Definition (Theory t2SafetyReq))) :+: S " T2) are true, " :+:
    S "output the message " :+: Quote (safeMessage ^. descr) :+: S " If " :+:
    S "the condition is false, then output the message " :+: 
    Quote (notSafe ^. descr))] ++
  [(S "R6", Nested (S "Output the following quantities:")
    (Bullet $ 
      [Flat $ (prob_br ^. descr) :+: S " (" :+: (P $ prob_br ^. symbol) :+: 
        S ") (" :+: (makeRef (Definition (Theory probOfBr))) :+: S ")"] ++
      [Flat $ (lRe ^. descr) :+: S " (" :+: (P $ lRe ^. symbol) :+: S ") (" 
        :+: (makeRef (Definition (Theory calOfCap))) :+: S ")"] ++
      [Flat $ (demand ^. descr) :+: S " (" :+: (P $ demand ^. symbol) :+: 
        S ") (" :+: (makeRef (Definition (Theory calOfDe))) :+: S ")"] ++
      [Flat $ (act_thick ^. descr) :+: S " (" :+: (P $ act_thick ^. symbol) 
        :+: S ") (" :+: (makeRef (Definition (Data hFromt))) :+: S ")"] ++
      map (\c -> Flat $ (c ^. descr) :+: S " (" :+: (P $ c ^. symbol) :+: 
        S ") (" :+: (makeRef (Definition (Data c))) :+: S ")")
      [loadDF,strDisFac,nonFL]++
      [Flat $ (gTF ^. descr) :+: S " (" :+: (P $ gTF ^. symbol) :+: 
        S ") (" :+: (makeRef (Definition (Data glaTyFac))) :+: S ")"] ++
      map (\c -> Flat $ (c ^. descr) :+: S " (" :+: (P $ c ^. symbol) :+: 
        S ") (" :+: (makeRef (Definition (Data c))) :+: S ")")
      [dL,tolPre,tolStrDisFac] ++
      [Flat $ (ar ^. descr) :+: S " (" :+: (P $ ar ^. symbol) :+: 
        --S " = a/b)"
        S ")"]))]

s7_2 = Section 1 (S "Nonfunctional " :+: (requirement ^. descr) :+: S "s") 
  [Con s7_2_intro]

s7_2_intro = Paragraph $
    S "Given the small size, and relative simplicity, of this problem, " :+:
    S "performance is not a priority. Any reasonable implementation will " :+:
    S "be very quick and use minimal storage. Rather than performance, " :+:
    S "the priority nonfunctional requirements are correctness, " :+:
    S "verifiability, understandability, reusability, maintainability and " :+:
    S "portability."

s8 = Section 0 ((likelyChange ^. descr) :+: S "s") [Con s8_list]

s8_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(S "LC1", S "A3 - The system currently only calculates for external " :+:
    S "blast risk. In the future calculations can be added for the " :+:
    S "internal blast risk."),
  (S "LC2", S "A4, A8 - Currently the values for " :+: 
    (P $ sflawParamM ^. symbol) :+: S ", " :+: (P $ sflawParamK ^. symbol) :+:
    S ", and " :+: (P $ mod_elas ^. symbol) :+: S " are assumed to be the " :+:
    S "same for all glass. In the future these values can be changed to " :+:
    S "variable inputs."),
  (S "LC3", S "A5 - The software may be changed to accommodate more than " :+:
    S "a single " :+: (sMap (map toLower) (S $ lite ^. name)) :+: S "."),
  (S "LC4", S "A6 - The software may be changed to accommodate more " :+:
    S "boundary conditions than 4-sided support."),
  (S "LC5", S "A7 - The software may be changed to consider more than just " 
    :+: S "flexure of the glass.")]

s9 = Section 0 (S "References") [Con s9_list]

s9_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(S "[1]", S "N. Koothoor, " :+: Quote (S "A document drive approach to " :+:
    S "certifying scientific computing software,") :+: S " Master's thesis, "
  :+: S "McMaster University, Hamilton, Ontario, Canada, 2013."),
  (S "[2]", S "W. S. Smith and L. Lai, " :+: Quote (S "A new requirements " :+:
    S "template for scientific computing,") :+: S " in Proceedings of the " :+:
  S "First International Workshop on Situational Requirements Engineering " :+:
  S "Processes - Methods, Techniques and Tools to Support Situation-Specific "
  :+: S "Requirements Engineering Processes, SREP'05 (J.Ralyt" :+: 
    (F Acute 'e') :+: S ", P.Agerfalk, and N.Kraiem, eds.), (Paris, France), " 
  :+: S "pp. 107-121, In conjunction with 13th IEEE International " :+:
  S "Requirements Engineering Conference, 2005."),
  (S "[3]", S "J. Robertson and S. Robertson, " :+: Quote (S "Volere ":+:
    S "requirements specification template edition 16.") :+: S " " :+: 
  Quote (S "www.cs.uic.edu/ i442/VolereMaterials/templateArchive16/c " :+: 
    S "Volere template16.pdf") :+: S ", 2012."),
  (S "[4]", S "ASTM Standards Committee, " :+: Quote (S "Standard practice " 
    :+: S "for determining load resistance of glass in buildings,") :+: 
  S " Standard E1300-09a, American Society for Testing and Material (ASTM), "
  :+: S "2009."),
  (S "[5]", S "ASTM, developed by subcommittee C1408,Book of standards 15.02, " 
    :+: Quote (S "Standard specification for flat glass,C1036.")),
  (S "[6]", S "ASTM, developed by subcommittee C14.08,Book of standards " :+:
  S "15.02, " :+: Quote (S "Specification for heat treated flat glass-Kind " 
    :+: S "HS, kind FT coated and uncoated glass,C1048."))]

s10 = Section 0 (S "Appendix") [Con s10_intro, Con fig_2, Con fig_3]

s10_intro = Paragraph $
  S "This appendix holds the graphs (" :+: (makeRef fig_2) :+: S " and " :+:
  (makeRef fig_3) :+: S ") used for interpolating values needed in the models."

fig_2 = Figure (S "Figure 2: " :+: (demandq ^. descr) :+: S " (" :+: 
  P (demand ^. symbol) :+: S ") versus " :+: (S $ sD ^. name) :+:
  S " versus " :+: (char_weight ^. descr) :+: S " (" :+: 
  P (sflawParamM ^. symbol) :+: S ")") "ASTM_F2248-09.png"

fig_3 = Figure (S "Figure 3: Non dimensional " :+: 
  (sMap (map toLower) (S $ lateral ^. name)) :+: S " " :+:
  (sMap (map toLower) (S $ load ^. name)) :+: S " (" :+: 
  P (dimlessLoad ^. symbol) :+: S ") versus " :+: (ar ^. descr) :+: S " (" :+:
  P (ar ^. symbol) :+: S ") versus " :+: (sdf ^. descr) :+: S  " (" :+: 
  P (sdf ^. symbol) :+: S ")") "ASTM_F2248-09_BeasonEtAl.png"
