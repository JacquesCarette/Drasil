{-# OPTIONS -Wall #-} 
{-# LANGUAGE FlexibleContexts #-} 
module Example.Drasil.GlassBR.GlassBRBody where
import Data.Char (toLower)
import Language.Drasil.Printing.Helpers
import Example.Drasil.GlassBR.GlassBRExample
import Language.Drasil.Spec (Sentence(..),sMap, Accent(..)) --May need to update imports to hide Ref.
                            --More likely setup an API or something to
                            --Restrict access for novice users.
import Language.Drasil.Unit (Unit(..), UnitDefn(..))
import Example.Drasil.GlassBR.GlassBRSIUnits 
import Language.Drasil.Chunk
import Control.Lens ((^.))
import Language.Drasil.Misc
import Language.Drasil.Document
import Language.Drasil.Reference
import Language.Drasil.Instances ()

this_si :: [UnitDefn]
this_si = map UU [metre, second] ++ map UU [pascal, newton]

s1, s1_1,  s1_2, s1_3, s2, s2_1, s2_2, s2_3, s3, s3_1, s3_2, s4, s4_1, s4_2,
  s5, s5_1, s5_2, s6, s6_1, s6_1_1, s6_1_2, s6_1_3, s6_2, s6_2_1, s6_2_2, 
  s6_2_3, s6_2_4, s6_2_5, s7, s7_1, s7_2, s8, s9, s10 :: Section
s1_intro, s1_1_intro, s1_1_table, s1_2_intro, s1_2_table, s1_3_table, s2_intro,
  s2_1_intro1, s2_1_intro2, s2_2_intro, s2_3_intro1, s2_3_intro2, s3_intro, 
  s3_1_intro, s3_2_intro, s4_intro, s4_1_bullets, s4_2_intro, s5_intro, s5_1_table, 
  s5_2_bullets, s6_intro, s6_1_intro, s6_1_1_intro, s6_1_1_bullets, s6_1_2_intro,
  s6_1_2_list, s6_1_3_list, s6_2_intro, s6_2_1_intro, s6_2_1_list, s6_2_4_intro,
  s6_2_5_intro, s6_2_5_table1, s6_2_5_table2, s6_2_5_intro2, s6_2_5_table3,
  s7_1_intro, s7_1_list1, s7_1_table, s7_1_list2, s7_2_intro, s8_list, s9_list, s10_intro, 
  fig_glassbr, fig_2, fig_3 :: Contents


glassBR_srs :: Document  
glassBR_srs = Document (S "Software Requirements Specification for Glass-BR")
          (S "Nikitha Krithnan and Spencer Smith") [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10]

s1 = Section 0 (S "Reference Material") [Con s1_intro, Sub s1_1, Sub s1_2, Sub s1_3]

s1_intro = Paragraph (S "This section records information for easy reference.")

s1_1 = Section 1 (S "Table of Units") [Con s1_1_intro, Con s1_1_table]

s1_1_intro = Paragraph (S "Throughout this document SI (Syst" :+: 
           (F Grave 'e') :+: S "me International d'Unit" :+:
           (F Acute 'e') :+: S "s) is employed as the unit system." :+:
           S " In addition to the basic units, several derived units are" :+: 
           S " employed as described below. For each unit, the symbol is" :+: 
           S " given followed by a description of the unit followed by " :+: 
           S "the SI name.")

s1_1_table = Table [S "Symbol", S "Description", S "Name"] (mkTable
  [(\x -> Sy (x ^. unit)),
   (\x -> (x ^. descr)),
   (\x -> S (x ^. name))
  ] this_si)
  (S "Table of Units") True

s1_2 = Section 1 (S "Table of Symbols") [Con s1_2_intro, Con s1_2_table]

s1_2_intro = Paragraph $ 
  S "The table that follows summarizes the symbols used in this " :+:
  S "document along with their units.  The symbols are listed in " :+:
  S "alphabetical order." 
  
s1_2_table = Table [S "Symbol", S "Units", S "Description"] (mkTable
  [(\ch -> U (ch ^. symbol)),  
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
  S "Software is helpful to efficiently and correctly predict the glass slab. " :+:
  S "The blast under consideration is any type of man-made explosion. The software, " :+:
  S "herein called " :+: (gLassBR ^. descr) :+: S " aims to predict the blast risk " :+:
  S "involved with the glass slab using an intuitive interface. The following section " :+:
  S "provides an overview of the " :+: (softwareRS ^. descr) :+: S " (" :+: S (softwareRS ^. name) :+:
  S ") for " :+: (gLassBR ^. descr) :+: S ". This section explains the purpose the " :+:
  S "document is designed to fulfil, the scope of the requirements and the organization " :+:
  S "of the document: what the document is based on and intended to portray."

s2_1 = Section 1 (S "Purpose of Document") [Con s2_1_intro1, Con s2_1_intro2]

s2_1_intro1 = Paragraph $
  S "The main purpose of this document is to predict whether a given glass slab is likely" :+:
  S " to resist a specified blast. The goals and theoretical models used in the " :+: (gLassBR ^. descr) :+:
  S " code are provided, with an emphasis on explicitly identifying assumptions and " :+:
  S "unambiguous definitions. This document is intended to be used as a reference to " :+:
  S "provide all information necessary to understand and verify the analysis. The " :+: 
  S (softwareRS ^. name) :+: S " is abstract because the contents say what problem " :+:
  S "is being solved, but not how to solve it."

s2_1_intro2 = Paragraph $
  S "This document will be used as a starting point for subsequent development phases, " :+:
  S "including writing the design specification and the software verification and validation " :+:
  S "plan. The design document will show how the requirements are to be realized, including " :+:
  S "decisions on the numerical algorithms and programming environment. The verification and " :+:
  S "validation plan will show the steps that will be used to increase confidence in the " :+:
  S "software documentation and the implementation."

s2_2 = Section 1 (S "Scope of Requirements") [Con s2_2_intro]

s2_2_intro = Paragraph $
  S "The scope of the requirements includes getting all input parameters related to the " :+:
  S "glass slab and also the parameters related to blast type. Given the input, " :+:
  (gLassBR ^. descr) :+: S " is intended to use the data and predict whether the " :+:
  S "glass slab is safe to use or not."

s2_3 = Section 1 (S "Organization of Document") [Con s2_3_intro1, Con s2_3_intro2]

s2_3_intro1 = Paragraph $
  S "The organization of this document follows the template for an " :+: S (softwareRS ^. name) :+:
  S " for scientific computing software proposed by [1] and [2] (in " :+: (makeRef s9) :+:
  S "), with some aspects taken from Volere template 16 [3]. The presentation follows the " :+: 
  S "standard pattern of presenting goals, theories, definitions, and assumptions. For readers " :+:
  S "that would like a more bottom up approach, they can start reading the data definitions in " :+:
  (makeRef s6_2_4) :+: S " and trace back to find any additional information they require."

s2_3_intro2 = Paragraph $
  S "The goal statements are refined to the theoretical models, and theoretical models to " :+: 
  S "the instance models. The data definitions are used to support the definitions of the " :+: 
  S "different models." 

s3 = Section 0 (S "Stakeholders") [Con s3_intro, Sub s3_1, Sub s3_2]

s3_intro = Paragraph $
  S "This section describes the Stakeholders: the people who have an interest in the product."

s3_1 = Section 1 (S "The Client") [Con s3_1_intro]

s3_1_intro = Paragraph $
  S "The client for " :+: (gLassBR ^. descr) :+: S " is a company named Entuitive. It is developed by Dr. Manuel " :+:
  S "Campidelli. The client has the final say on acceptance of the product."

s3_2 = Section 1 (S "The Customer") [Con s3_2_intro]

s3_2_intro = Paragraph $
  S "The customers are the end user of " :+: (gLassBR ^. descr) :+: S "."

s4 = Section 0 (S "General System Description") [Con s4_intro, Sub s4_1, Sub s4_2]

s4_intro = Paragraph $
  S "This section provides general information about the system, identifies the interface " :+:
  S "between the system and its environment, and describes the user characteristics and the " :+:
  S "system constraints."

s4_1 = Section 1 (S "User Characteristics") [Con s4_1_bullets]

s4_1_bullets = Enumeration $ Bullet $ map Flat
  [(S "The end user of " :+: (gLassBR ^. descr) :+: S " is expected to have completed at " :+: 
    S "least the equivalent of the second year of an undergraduate degree in civil or structural engineering."),
  (S "The end user is expected to have an understanding of theory behind glass breakage and blast risk."),
  (S "The end user is expected to have basic computer literacy to handle the software.")]

s4_2 = Section 1 (S "System Constraints") [Con s4_2_intro]

s4_2_intro = Paragraph $
  S (notApp ^. name)

s5 = Section 0 (S "Scope of the Project") [Con s5_intro, Sub s5_1, Sub s5_2]

s5_intro = Paragraph $
  S "This section presents the scope of the project. It describes the expected use of " :+:
  (gLassBR ^. descr) :+: S " as well as the inputs and outputs of each action. The use cases are " :+:
  S "input and output, which defines the action of getting the input and displaying the " :+:
  S "output."

s5_1 = Section 1 (S "Product Use Case Table") [Con s5_1_table]

s5_1_table = Table [S "Use Case NO.", S "Use Case Name", S "Actor", S "Input and Output"] (mkTable
  [(\x -> (x!!0)),(\x -> (x!!1)), (\x -> (x!!2)), (\x -> (x!!3))]
  [[S "1", S "Inputs", S "User", S "Characteristics of the glass slab and of the blast. Details in " :+: (makeRef s5_2) :+: S "."],
  [S "2", S "Output", (gLassBR ^. descr), S "Whether or not the glass slab is safe for the calculated load and supporting calculated values"]])
  (S "Table 1: Use Case Table") True

s5_2 = Section 1 (S "Individual Product Use Cases") [Con s5_2_bullets]

s5_2_bullets = Enumeration $ Bullet $ map Flat
  [(S "Use Case 1 refers to the user providing input to " :+: (gLassBR ^. descr) :+: 
    S " for use within the analysis. There are two classes of inputs: glass geometry and " :+:
    S "blast type. The glass geometry based inputs include the dimensions of the glass plane, " :+:
    S "glass type and response type. The blast type input includes parameters like weight of " :+:
    S "charge, TNT equivalent factor and stand off distance from the point of explosion. " :+:
    S "These parameters describe charge weight and stand off blast. Another input the user " :+:
    S "gives is the tolerable value of probability of breakage."),
  (S " Use Case 2 " :+: (gLassBR ^. descr) :+: S " outputs if the glass slab will be safe by " :+:
    S "comparing whether capacity > demand. Capacity is the load resistance calculated and demand " :+:
    S "is the requirement which is the 3 second duration equivalent pressure. The second condition is to check whether " :+:
    S "the calculated probability (" :+: (U $ prob_br ^. symbol) :+: S ") is less than the tolerable " :+: 
    S "probability (" :+: (U $ pb_tol ^. symbol) :+: S ") which is obtained from the user as an input. " :+:
    S "If both conditions return true then it's shown that the glass slab is safe to use, else if " :+:
    S "both return false then the glass slab is considered unsafe. All the supporting calculated " :+:
    S "values are also displayed as output.")]

s6 = Section 0 (S "Specific System Description") [Con s6_intro, Sub s6_1, Sub s6_2]

s6_intro = Paragraph $ S "This section first presents the problem " :+:
  S "description, which gives a high-level view of the problem to be solved" :+:
  S ". This is followed by the solution characteristics specification, " :+:
  S "which presents the assumptions, theories, definitions."

s6_1 = Section 1 (S "Problem Description") [Con s6_1_intro, Sub s6_1_1, Sub s6_1_2, Sub s6_1_3]

s6_1_intro = Paragraph $ S "A system is needed to efficiently and correctly" :+:
  S " predict the blast risk involved with the glass. " :+: (gLassBR ^. descr) :+:
  S " is a computer program " :+: S "developed to interpret the inputs to give " :+:
  S "out the outputs which predicts whether the glass slab can withstand the " :+:
  S "blast under the conditions."

s6_1_1 = Section 2 (S "Terminology and Definitions") [Con s6_1_1_intro, Con s6_1_1_bullets]
  
s6_1_1_intro = Paragraph $ S "This subsection provides a list of terms that " :+:
  S "are used in subsequent sections and their meaning, with the purpose of ":+:
  S "reducing ambiguity and making it easier to correctly understand the ":+:
  S "requirements. All of the terms are extracted from [4] in " :+: (makeRef s9) :+: S "."

s6_1_1_bullets = Enumeration $ (Number $ map (\c -> Flat $ S (c ^. name) :+:
  S " - " :+: (c ^. descr)) [aR, gbr, lite] ++ [Nested (S (glassTy ^. name) :+: S ":") 
  (Bullet $ map (\c -> Flat $ S (c ^. name) :+: S " - " :+: (c ^. descr)) [an, ft, hs])] ++
  map (\c -> Flat $ S (c ^. name) :+: S " - " :+: (c ^. descr)) [gtf,lateral] ++
  [Nested (S (load ^. name) :+: S":") (Bullet $ map (\c -> Flat $ S (c ^. name) :+: S " - " :+:
  (c ^. descr)) [specDeLoad, lr, ldl, nfl, glassWL, sdl])] ++ map (\c -> Flat $ S (c ^. name) :+:
  S " - " :+: (c ^. descr)) [lsf, pb, specA, blaReGLa, eqTNTChar, sD])
  
s6_1_2 = Section 2 (physSysDescr ^. descr) [Con s6_1_2_intro, Con s6_1_2_list, Con fig_glassbr]

s6_1_2_intro = Paragraph $ S "The physical system of " :+: (gLassBR ^. descr) :+:
 S " as shown in " :+: (makeRef fig_glassbr) :+: S ", includes the following elements:"

fig_glassbr = Figure (S "The physical system") "physicalsystimage.png"
  
s6_1_2_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "PS1", S "Glass slab"), 
  (S "PS2", S "The point of explosion. Where the bomb,or any man made explosive, " :+:
   S "is located. " :+: S "The stand off distance is the distance" :+:
   S " between the point of explosion and the glass.")]

s6_1_3 = Section 2 ((goalStmt ^. descr) :+: S "s") [Con s6_1_3_list]

s6_1_3_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "GS1", S "Analyze and predict whether the glass slab under " :+:
  S "consideration will be able to withstand the explosion of a certain degree " :+:
  S "which is calculated based on user input.")]

s6_2 = Section 1 (S "Solution Characteristics Specification") 
  [Con s6_2_intro, Sub s6_2_1, Sub s6_2_2, Sub s6_2_3, Sub s6_2_4, Sub s6_2_5]

s6_2_intro = Paragraph $ S "This section explains all the assumptions" :+:
  S " considered and the " :+: (sMap (map toLower) (theoreticMod ^. descr)) :+:
  S " which are supported by the data definitions." 
  
s6_2_1 = Section 2 (assumption ^. descr :+: S "s") [Con s6_2_1_intro, Con s6_2_1_list]

s6_2_1_intro = Paragraph $ S "This section simplifies the original problem " :+:
  S "and helps in developing the theoretical model by filling in the " :+:
  S "missing information for the physical system. The numbers given in the " :+:
  S "square brackets refer to the " :+: (sMap (map toLower) (dataDefn ^. descr)) :+:
  S (" " ++ sqbrac (dataDefn ^. name)) :+: S ", or " :+:
  (sMap (map toLower) (instanceMod ^. descr)) :+: S (" " ++ 
  sqbrac (instanceMod ^. name)) :+: S ", in which the respective " :+: 
  (sMap (map toLower) $ assumption ^. descr) :+: S " is used."

s6_2_1_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "A1", S "The standard E1300-09a for calculation applies only to monolithic, " :+:
    S "laminated, or insulating glass constructions of rectangular shape with continuous " :+:
    S "lateral support along one, two, three, or four edges. This practice assumes " :+:
    S "that (1) the supported glass edges for two, three and four-sided support conditions are " :+:
    S "simply supported and free to slip in plane; (2) glass supported on two sides " :+:
    S "acts as a simply supported beam and (3) glass supported on one side acts as " :+:
    S "a cantilever."), 
  (S "A2", S "This practice does not apply to any form of wired, patterned, etched, " :+:
    S "sandblasted, drilled, notched, or grooved glass with surface and edge treatments " :+:
    S "that alter the glass strength."),
  (S "A3", S "This system only considers the external explosion scenario for its calculations.")] ++
  [(S "A4", Nested (S "Standard values used for calculation in " :+: (gLassBR ^. descr) :+: S " are: ") 
    (Number $ map (\c -> Flat c) [
    (U $ sflawParamM ^. symbol) :+: S " = 7 " :+: Sy (sflawParamM ^. unit), 
    (U $ sflawParamK ^. symbol) :+: S " = 2.86 * 10^(-53) " :+: Sy (sflawParamK ^. unit), 
    (U $ mod_elas ^. symbol) :+: S " = 7.17 * 10^7 " :+: Sy (mod_elas ^. unit),
    (U $ load_dur ^. symbol) :+: S " = 3 " :+: Sy (load_dur ^. unit)]))] ++
  map (\(a,b) -> (a, Flat b)) [
  (S "A5", S "Glass under consideration is assumed to be a single lite. Hence the value of " :+:
    (U $ loadSF ^. symbol) :+: S " is equal to 1 for all calculations in " :+: (gLassBR ^. descr) :+: S "."),
  (S "A6", S "Boundary conditions for the glass slab is assumed to be 4-sided support for " :+:
    S "calculations."),
  (S "A7", S "The response type considered in " :+: (gLassBR ^. descr) :+: S " is flexural."),
  (S "A8", S "With reference to A4 the value of load distribution factor (" :+: (U $ loadDF ^. symbol) :+:
    S ") is a constant in " :+: (gLassBR ^. descr) :+: S ". It is calculated by the equation: " :+: 
    (U $ loadDF ^. symbol) :+: S " = " :+: (U $ load_dur ^. symbol) :+: S ". Using this, " :+: 
    (U $ loadDF ^. symbol) :+: S " = 0.27.")]
  --equation in sentence

s6_2_2 = Section 2 ((theoreticMod ^. descr) :+: S "s") (map Con s6_2_2_TMods)
  
s6_2_2_TMods :: [Contents]
s6_2_2_TMods = map Definition (map Theory [t1SafetyReq,t2SafetyReq])

s6_2_3 = Section 2 ((instanceMod ^. descr) :+: S "s") (map Con s6_2_3_IMods)

s6_2_3_IMods :: [Contents]
s6_2_3_IMods = map Definition (map Theory [probOfBr,calOfCap,calOfDe])

s6_2_4 = Section 2 ((dataDefn ^. descr) :+: S "s") ((Con s6_2_4_intro):(map Con s6_2_4_DDefns))

s6_2_4_intro = Paragraph $ S "This section collects and defines all the data " :+:
  S "needed to build the instance models."

s6_2_4_DDefns ::[Contents] 
s6_2_4_DDefns = map Definition (map Data [risk,hFromt,loadDF,strDisFac,nonFL,glaTyFac,dL,tolPre,
  tolStrDisFac])

s6_2_5 = Section 2 (S "Data Constraints") [Con s6_2_5_intro, Con s6_2_5_table1, Con s6_2_5_table2, Con s6_2_5_intro2, Con s6_2_5_table3]

s6_2_5_intro = Paragraph $
  S "Table 2 (" :+: (makeRef s6_2_5_table1) :+: S ") shows the data constraints on the input variables. " :+:
  S "The column of physical constraints gives the physical limitations on the range of values that " :+:
  S "can be taken by the variable. The constraints are conservative, to give the user of the model " :+: 
  S "the flexibility to experiment with unusual situations. The column of typical values is intended " :+:
  S "to provide a feel for a common scenario. The uncertainty column provides an estimate of the " :+:
  S "confidence with which the physical quantities can be measured. This information would be part " :+:
  S "of the input if one were performing an uncertainty quantification exercise. Table 3 (" :+:
  (makeRef s6_2_5_table2) :+: S ") gives the values of the specification parameters used in " :+: 
  S "Table 2 (" :+: (makeRef s6_2_5_table1) :+: S "). " :+: (U $ ar_max ^. symbol) :+: 
  S " refers to the maximum aspect ratio for the plate of glass."

s6_2_5_table1 = Table [S "Var", S "Physical Cons", S "Software Constraints", S "Typical Value",
  S "Uncertainty"] (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2), (\x -> x!!3),
  (\x -> x!!4)] [[(U $ plate_len ^. symbol), (U $ plate_len ^. symbol) :+: S " > 0 and " :+: 
  (U $ plate_len ^. symbol) :+: S "/" :+: (U $ plate_width ^. symbol) :+: S " > 1",
  (U $ dim_min ^. symbol) :+: S " <= " :+: (U $ plate_len ^. symbol) :+: S " <= " :+: 
  (U $ dim_max ^. symbol) :+: S " and " :+: (U $ plate_len ^. symbol) :+: S "/" :+: 
  (U $ plate_width ^. symbol) :+: S " < " :+: (U $ ar_max ^. symbol), S "1500 " :+:
  Sy (plate_len ^. unit), S "10%"], [(U $ plate_width ^. symbol), (U $ (plate_width ^. symbol)) 
  :+: S " > 0 and " :+: (U $ plate_width ^. symbol) :+: S " < " :+: (U $ plate_len ^. symbol),
  (U $ dim_min ^. symbol) :+: S " <= " :+: (U $ plate_width ^. symbol) :+: S " <= " :+: 
  (U $ dim_max ^.symbol) :+: S " and " :+: (U $ plate_len ^. symbol) :+: S "/" :+: 
  (U $ plate_width ^. symbol) :+: S " < " :+: (U $ ar_max ^. symbol), S "1200 " :+: 
  Sy (plate_width ^. unit), S "10%"], [(U $ pb_tol ^. symbol), S "0 < " :+: 
  (U $ pb_tol ^. symbol) :+: S " < 1", S "-", S "0.008", S "0.1%"], [(U $ char_weight ^. symbol), 
  (U $ char_weight ^. symbol) :+: S " >= 0", (U $ cWeightMin ^. symbol) :+: S " < " :+: 
  (U $ char_weight ^. symbol) :+: S " < " :+: (U $ cWeightMax ^. symbol), S "42 " :+: 
  Sy (char_weight ^. unit), S "10%"],[(U $ tNT ^. symbol), (U $ tNT ^. symbol) :+: 
  S " > 0", S "-", S "1", S "10%"], [(U $ sd ^. symbol), (U $ sd ^. symbol) :+: S " > 0", 
  (U $ sd_min ^. symbol) :+: S " < " :+: (U $ sd ^. symbol) :+: S " < " :+: 
  (U $ sd_max ^. symbol), S "45" :+: Sy (sd ^. unit), S "10%"]])
  (S "Table 2: Input Variables") True

s6_2_5_table2 = Table [S "Var", S "Value"] (mkTable [(\x -> U $ fst(x)),
  (\x -> snd(x))] [(dim_min ^. symbol, S "0.1 " :+: Sy (sd ^. unit)), (dim_max ^.symbol, S "0.1 " :+:
  Sy (sd ^. unit)),(ar_max ^. symbol, S "5"), (cWeightMin ^. symbol, S "4.5 " :+: Sy (cWeightMin ^. unit)),
  (cWeightMax ^. symbol, S "910 " :+: Sy (cWeightMax ^. unit)), (sd_min ^. symbol, S "6 " :+: 
  Sy (sd_min ^. unit)), (sd_max ^. symbol, S "130 " :+: Sy (sd_max ^. unit))])
  (S "Table 3: Specification Parameter Values") True

s6_2_5_intro2 = Paragraph $
  S "Table 4 (" :+: (makeRef s6_2_5_table3) :+: S ") shows the constraints that must be satisfied by the output."

s6_2_5_table3 = Table [S "Var", S "Physical Constraints"] (mkTable [(\x -> U $ fst(x)),
  (\x -> snd(x))] [(prob_br ^. symbol, S "0 < " :+: (U $ prob_br ^. symbol) :+: S " < 1")])
  (S "Table4: Output Variables") True

s7 = Section 0 (S "Requirements") [Sub s7_1, Sub s7_2]

s7_1 = Section 1 (S "Functional Requirements") [Con s7_1_intro, Con s7_1_list1, Con s7_1_table, Con s7_1_list2]

s7_1_intro = Paragraph $
  S "The following section provides the functional requirements, the business tasks that the software " :+:
  S "is expected to complete."


s7_1_list1 = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(S "R1", S "Input the following quantities, which define the glass dimensions, type of glass, " :+:
    S "tolerable probability of failure and the characteristics of the blast:")]

s7_1_table = Table [S "Symbol", S "Units", S "Description"] (mkTable
  [(\ch -> U (ch ^. symbol)),  
   (\ch -> Sy $ ch ^. unit),
   (\ch -> ch ^. descr)
   ]
  [plate_len,plate_width,glass_type,pb_tol,sdx,sdy,sdz,nom_thick,tNT,char_weight])
  (S "Input Parameters") False

s7_1_list2 = Enumeration $ Simple $
  [(S "R2", Nested (S "The system shall set the known values as follows: ") 
    (Bullet $ map (\c -> Flat c) 
      [(U $ sflawParamM ^. symbol) :+: S ", " :+: (U $ sflawParamK ^. symbol) :+: S ", " 
      :+: (U $ mod_elas ^. symbol) :+: S ", " :+: (U $ load_dur ^. symbol) :+: S " following A4",
      (U $ loadDF ^. symbol) :+: S " following A8",
      (U $ loadSF ^. symbol) :+: S " following A5"]))] ++
  map (\(a,b) -> (a, Flat b))
  [(S "R3", S "The system shall check the entered input values to ensure that they do not exceed the " :+:
    S "data constraints mentioned in " :+: (makeRef s6_2_5) :+: S ". If any of the " :+:
    S "input parameters is out of bounds, an error message is displayed and the calculations stop."),
  (S "R4", S "Output the input quantities from R1 and the known quantities from R2."),
  (S "R5", S "If is_safe1 and is_safe2 (from " :+: (makeRef (Definition (Theory t1SafetyReq))) :+: 
    S " T1 and " :+: (makeRef (Definition (Theory t2SafetyReq))) :+: S " T2) are true, output the message " :+: 
    Quote (S "For the given input parameters, the glass is considered safe.") :+: S " If the condition " :+: 
    S "is false, then output the message " :+: Quote (S "For the given input parameters, the glass is " :+:
    S "NOT considered safe."))] ++
  [(S "R6", Nested (S "Output the following quantities:")
    (Bullet $ 
      [Flat $ (prob_br ^. descr) :+: S " (" :+: (U $ prob_br ^. symbol) :+: S ") (" :+: (makeRef (Definition (Theory probOfBr))) :+: S " )"] ++
      [Flat $ (lRe ^. descr) :+: S " (" :+: (U $ lRe ^. symbol) :+: S ") (" :+: (makeRef (Definition (Theory calOfCap))) :+: S " )"] ++
      [Flat $ (demand ^. descr) :+: S " (" :+: (U $ demand ^. symbol) :+: S ") (" :+: (makeRef (Definition (Theory calOfDe))) :+: S " )"] ++
      [Flat $ (act_thick ^. descr) :+: S " (" :+: (U $ act_thick ^. symbol) :+: S ") (" :+: (makeRef (Definition (Data hFromt))) :+: S " )"] ++
      map (\c -> Flat $ (c ^. descr) :+: S " (" :+: (U $ c ^. symbol) :+: S ") (" :+: (makeRef (Definition (Data c))) :+: S " )")
      [loadDF,strDisFac,nonFL]++
      [Flat $ (gTF ^. descr) :+: S " (" :+: (U $ gTF ^. symbol) :+: S ") (" :+: (makeRef (Definition (Data glaTyFac))) :+: S " )"] ++
      map (\c -> Flat $ (c ^. descr) :+: S " (" :+: (U $ c ^. symbol) :+: S ") (" :+: (makeRef (Definition (Data c))) :+: S " )")
      [dL,tolPre,tolStrDisFac] ++
      [Flat $ (ar ^. descr) :+: S " (" :+: (U $ ar ^. symbol) :+: S " = a/b)"]))]

s7_2 = Section 1 (S "Nonfunctional Requirements") [Con s7_2_intro]

s7_2_intro = Paragraph $
    S "Given the small size, and relative simplicity, of this problem, performance is not a priority. Any " :+:
    S "reasonable implementation will be very quick and use minimal storage. Rather than performance, the " :+:
    S "priority nonfunctional requirements are correctness, verifiability, understandability, reusability, " :+:
    S "maintainability and portability."

s8 = Section 0 (S "Likely Changes") [Con s8_list]

s8_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(S "LC1", S "A3 - The system currently only calculates for external blast risk. In the future calculations " :+:
    S "can be added for the internal blast risk."),
  (S "LC2", S "A4, A8 - Currently the values for " :+: (U $ sflawParamM ^. symbol) :+: S ", " :+: (U $ sflawParamK ^. symbol) :+:
  S ", and " :+: (U $ mod_elas ^. symbol) :+: S " are assumed to be the same for all glass. In the future these " :+:
  S "values can be changed to variable inputs."),
  (S "LC3", S "A5 - The software may be changed to accommodate more than a single lite."),
  (S "LC4", S "A6 - The software may be changed to accommodate more boundary conditions than 4-sided support."),
  (S "LC5", S "A7 - The software may be changed to consider more than just flexure of the glass.")]

s9 = Section 0 (S "References") [Con s9_list]

s9_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b))
  [(S "[1]", S "N. Koothoor, " :+: Quote (S "A document drive approach to certifying scientific computing software,") :+:
    S " Master's thesis, McMaster University, Hamilton, Ontario, Canada, 2013."),
  (S "[2]", S "W. S. Smith and L. Lai, " :+: Quote (S "A new requirements template for scientific computing,") :+:
    S " in Proceedings of the First International Workshop on Situational Requirements Engineering Processes " :+:
    S "- Methods, Techniques and Tools to Support Situation-Specific Requirements Engineering Processes, " :+:
    S "SREP'05 (J.Ralyt" :+: (F Acute 'e') :+: S ", P.Agerfalk, and N.Kraiem, eds.), (Paris, France), pp. 107-121, " :+:
    S "In conjunction with 13th IEEE International Requirements Engineering Conference, 2005."),
  (S "[3]", S "J. Robertson and S. Robertson, " :+: Quote (S "Volere requirements specification template edition 16.") :+:
    S " " :+: Quote (S "www.cs.uic.edu/ i442/VolereMaterials/templateArchive16/c Volere template16.pdf") :+: S ", 2012."),
  (S "[4]", S "ASTM Standards Committee, " :+: Quote (S "Standard practice for determining load resistance of glass in " :+:
    S "buildings,") :+: S " Standard E1300-09a, American Society for Testing and Material (ASTM), 2009."),
  (S "[5]", S "ASTM, developed by subcommittee C1408,Book of standards 15.02, " :+: Quote (S "Standard specification for " :+:
    S "flat glass,C1036.")),
  (S "[6]", S "ASTM, developed by subcommittee C14.08,Book of standards 15.02, " :+: Quote (S "Specification for heat " :+:
    S "treated flat glass-Kind HS, kind FT coated and uncoated glass,C1048."))]

s10 = Section 0 (S "Appendix") [Con s10_intro, Con fig_2, Con fig_3]

s10_intro = Paragraph $
  S "This appendix holds the graphs (" :+: (makeRef fig_2) :+: S " and " :+: (makeRef fig_3) :+: S ") used for interpolating values needed in the models."

fig_2 = Figure (S "Figure 2: 3 second equivalent pressure (" :+: U (demand ^. symbol) :+: S ") versus Stand off " :+:
  S "distance (SD) versus charge weight (" :+: U (sflawParamM ^. symbol) :+: S ")") "ASTM_F2248-09.png"

fig_3 = Figure (S "Figure 3: Non dimensional lateral load (" :+: U (dimlessLoad ^. symbol) :+: S ") versus Aspect " :+:
  S "ratio versus Stress distribution factor (" :+: U (sdf ^. symbol) :+: S ")") "ASTM_F2248-09_BeasonEtAl.png"
