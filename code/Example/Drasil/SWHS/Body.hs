{-# OPTIONS -Wall #-} 
{-# LANGUAGE FlexibleContexts #-} 
module Example.Drasil.SWHS.Body where

import Data.Char (toLower)
import Data.List (intersperse)
import Control.Lens ((^.))

import Example.Drasil.SWHS.Unitals
import Example.Drasil.SWHS.Concepts
import Example.Drasil.SWHS.TModel1
import Example.Drasil.SWHS.TModel2
import Example.Drasil.SWHS.TModel3
import Example.Drasil.SWHS.DataDefs
import Example.Drasil.SWHS.Units

import Language.Drasil
import Language.Drasil.SI_Units 

--Redundant import warnings

this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

--Will there be a table of contents?

s1, s1_1, s1_2, s1_3, s2, s2_1, s2_2, s2_3, s3, s3_1, s3_2, s4, s4_1,
  s4_1_1, s4_1_2, s4_1_3, s4_2, s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5,
  s4_2_6, s5, s5_1, s5_2, s6, s7 :: Section

s1_intro, s1_1_intro, s1_1_table, s1_2_intro, s1_2_table, s1_3_table,
  s2_2_contents, s3_intro, s3_1_contents, s3_2_contents, s4_intro, 
  s4_1_intro, s4_1_1_intro, s4_1_1_bullets, s4_1_2_intro, s4_1_2_list,
  fig_tank, s4_1_3_intro, s4_1_3_list, s4_2_intro, s4_2_1_intro, 
  s4_2_1_list, s4_2_2_intro, s4_2_3_intro, s4_2_4_intro, table1, s5_intro,
  s5_2_contents, s6_list, s7_table :: Contents
  
s2_intro, s2_1_contents, s2_3_contents, s4_2_5_intro, s4_2_6_intro, 
  s5_1_list, s7_intro :: [Contents]

swhs_srs :: Document
swhs_srs = Document (S "Software Requirements Specification for Solar Water" :+:
           S " Heating Systems with Phase Change Material")
           (S "Thulasi Jegatheesan, Brooks MacLachlan, and Spencer Smith")
           [s1, s2, s3, s4, s5, s6, s7]

--It is sometimes hard to remember to add new sections both here and above.

-- Beginning of title could be automated

s1 = Section 0 (S "Reference Material") [Con s1_intro, Sub s1_1, Sub s1_2, Sub s1_3]

s1_intro = Paragraph (S "This section records information for easy reference.")

s1_1 = Section 1 (S "Table of Units") [Con s1_1_intro, Con s1_1_table]

s1_1_intro = Paragraph (S "Throughout this document SI (Syst" :+:
             (F Grave 'e') :+: S "me International d'Unit" :+:
             (F Acute 'e') :+: S "s) is employed as the unit system" :+:
             S ". In addition to the basic units, several derived " :+:
             S "units are used as described below. For each unit, " :+:
             S "the symbol is given followed by a description of the" :+:
             S " unit followed by the SI name.")

-- General paragraph except for SI units

s1_1_table = Table [S "Symbol", S "Description", S "Name"] (mkTable
  [(\x -> Sy (x ^. unit)),
   (\x -> (x ^. descr)),
   (\x -> S (x ^. name))
   ] this_si)
  (S "Table of Units") False
  
-- Is it possible to make tables look nicer? I.e. \hline

s1_2 = Section 1 (S "Table of Symbols") [Con s1_2_intro, Con s1_2_table]

s1_2_intro = Paragraph (S "The table that follows summarizes the" :+:
             S " symbols used in this document along with their units" :+:
             S ". The choice of symbols was made to be consistent" :+:
             S " with the heat transfer literature and with existing" :+:
             S " documentation for " :+: (sMap (map toLower) (progName ^. descr)) :+: S "s. The " :+:
             S "symbols are listed in alphabetical order.")

-- "heat transfer" is specific.

s1_2_table = Table [S "Symbol", S "Unit", S "Description"] (mkTable
  [(\ch -> U (ch ^. symbol)),
   (\ch -> Sy (ch ^. unit)),
   (\ch -> ch ^. descr)
   ] swhsSymbols)
   (S "Table of Symbols") False

-- if the lambdas end up being very similar for every table, can this be simplified?
  
s1_3 = Section 1 (S "Abbreviations and Acronyms") [Con s1_3_table]

s1_3_table = Table [S "Symbol", S "Description"] (mkTable
  [(\ch -> S (ch ^. name)),
   (\ch -> ch ^. descr)
   ] acronyms)
   (S "Abbrevations and Acronyms") False
   
s2 = Section 0 (S "Introduction") ((map Con s2_intro)++[Sub s2_1, Sub s2_2, Sub s2_3])

s2_intro = [Paragraph (S "Due to increasing cost, diminishing " :+:
           S "availability, and negative environmental impact of " :+:
           S "fossil fuels, there is a higher demand for renewable" :+:
           S " energy sources and energy storage technology. Solar water " :+:
           S "heating systems incorporating " :+: (phsChgMtrl ^. descr) :+:
           S " (" :+: S (phsChgMtrl ^. name) :+: S ") " :+:
           S "use a renewable energy source and provide a novel way of " :+:
           S "storing energy. Solar water heating systems with PCM improve" :+:
           S " over the traditional solar heating systems because of their" :+:
           S " smaller size. The smaller size is possible because of the " :+:
           S "ability of PCM to store thermal energy as latent heat, which " :+:
           S "allows higher thermal energy storage capacity per unit weight."),
           Paragraph (S " The following section provides an overview of the " :+:
           (srs ^. descr) :+: S " (" :+: S (srs ^. name) :+: 
           S ") for a solar water heating system that incorporates PCM. The " :+:
           S "developed program will be referred to as " :+: (progName ^. descr) :+:
           S " (" :+: S (progName ^. name) :+: S "). This section " :+:
           S "explains the purpose of this document, the scope of the system" :+:
           S ", the organization of the document and the characteristics of " :+:
           S "the intended readers.")]

-- The first part of this paragraph is specific.

s2_1 = Section 1 (S "Purpose of Document") (map Con s2_1_contents)

s2_1_contents = [Paragraph (S "The main purpose of this document is to " :+:
                S "describe the modelling of solar water heating systems " :+:
                S "incorporating PCM. The " :+: (sMap (map toLower) (goalStmt ^. descr)) :+: S "s and " :+: (sMap (map toLower) (thModel ^. descr)) :+: S "s used" :+:
                S " in the " :+: S (progName ^. name) :+: S " code are " :+:
                S "provided, with an emphasis on explicitly identifying " :+:
                (sMap (map toLower) (assumption ^. descr)) :+: S "s and unambiguous definitions. This document " :+:
                S "is intended to be used as a reference to provide ad hoc" :+:
                S " access to all information necessary to understand and " :+:
                S "verify the model. The " :+: S (srs ^. name) :+: S " is abstract because the contents" :+:
                S " say what problem is being solved, but do not say how to " :+:
                S "solve it."), 
                Paragraph (S "This document will be used as a starting point " :+:
                S "for subsequent development phases, including writing the " :+:
                S "design specification and the software verification and " :+:
                S "validation plan. The design document will show how the " :+:
                S "requirements are to be realized, including decisions on " :+:
                S "the numerical algorithms and programming environment. " :+:
                S "The verification and validation plan will show the steps" :+:
                S " that will be used to increase confidence in the software" :+:
                S " documentation and the implementation. Although the " :+:
                S (srs ^. name) :+:
                S " fits in a series of documents that follow the so-called " :+:
                S "waterfall model, the actual development process is not " :+:
                S "constrained in any way. Even when the process is not " :+:
                S "waterfall, as Parnas and Clements [citation] point out, " :+:
                S "the most logical way to present the documentation is " :+:
                S "still to " :+: Quote (S "fake") :+: S " a rational design process.")]

-- This paragraph is mostly general
---- "solar water heating systems incorporating PCM" is something that is
---- repeated and seems like it could be captured.				
--How to italicize words in sentence?
--How to cite?

s2_2 = Section 1 (S "Scope of Requirements") [Con s2_2_contents]

s2_2_contents = Paragraph (S "The scope of the requirements is limited " :+:
                S "to thermal analysis of a single solar water heating" :+:
                S " tank incorporating PCM. Given the appropriate inputs, " :+:
                S "the code for " :+: S (progName ^. name) :+: S " is " :+:
                S "intended to predict the temperature and energy " :+:
                S "histories for the water and the PCM. This entire document" :+:
                S " is written assuming that the substances inside the " :+:
                S "heating tank are water and " :+: S (phsChgMtrl ^. name) :+: S ".")

-- Lots of specific concepts in this paragraph that can likely be captured.
----Heating tank, temperature and energy

s2_3 = Section 1 (S "Organization of Document") (map Con s2_3_contents)

s2_3_contents = [Paragraph (S "The organization of this document follows" :+:
                S " the template for an " :+: S (srs ^. name) :+: S " for scientific computing " :+:
                S "software proposed by [citation] and [citation]. The " :+:
                S "presentation follows the standard pattern for presenting " :+:
                (sMap (map toLower) (goalStmt ^. descr)) :+: S "s, " :+: (sMap (map toLower) (thModel ^. descr)) :+: 
                S "s, " :+: (sMap (map toLower) (dataDefn ^. descr)) :+: S "s, and " :+: 
                (sMap (map toLower) (assumption ^. descr)) :+: S "s. For readers" :+:
                S " that would like a more bottom up approach, they can start" :+:
                S " reading the " :+: (sMap (map toLower) (inModel ^. descr)) :+: S "s in " :+: makeRef s4_2_5 :+:
                S " and trace back to find any additional information they " :+:
                S "require. The " :+: (sMap (map toLower) (inModel ^. descr)) :+: S "s provide the " :+: (ordDiffEq ^. descr) :+:
                S " (" :+: S (ordDiffEq ^. name) :+: S "s) and algebraic equations that model " :+:
                S "the solar water heating system with PCM. " :+: S (progName ^. name) :+:
                S " solves these " :+: S (ordDiffEq ^. name) :+: S "s."),
                Paragraph (S "The " :+: (sMap (map toLower) (goalStmt ^. descr)) :+: S "s are refined to the " :+:
                (sMap (map toLower) (thModel ^. descr)) :+: S "s, and " :+: (sMap (map toLower) (thModel ^. descr)) :+: S "s to the " :+:
                (sMap (map toLower) (inModel ^. descr)) :+: S "s. The " :+: 
                (sMap (map toLower) (inModel ^. descr)) :+: S "s (" :+: makeRef s4_2_5 :+:
                S ") to be solved are referred to as IM1 to IM4.")]

-- Mostly general paragraphs, except for "solar water heating system with PCM" (could be "incorporating PCM").
-- IM1 to IM4 : reference later

-- how to cite/reference?

-- If all SRS have the same basic layout, is it possible to automate
-- the sectioning? This would also improve the tediousness of declaring 
-- LayoutObjs

s3 = Section 0 (S "General System Description") [Con s3_intro, Sub s3_1, Sub s3_2]

s3_intro = Paragraph (S "This section provides general information about " :+:
           S "the system, identifies the interfaces between the system and" :+:
           S " its environment, and describes the user characteristics and" :+:
           S " the system constraints.")

-- Completely general paragraph

s3_1 = Section 1 (S "User Characteristics") [Con s3_1_contents]

s3_1_contents = Paragraph (S "The end user of " :+: S (progName ^. name) :+: S " should have an " :+:
                S "understanding of undergraduate Level 1 Calculus and " :+:
                S "Physics.")

-- Can we capture "UG Level 1 Calculus" and "UG Level 1 Physics"? 

s3_2 = Section 1 (S "System Constraints") [Con s3_2_contents]

s3_2_contents = Paragraph (S "There are no system constraints.")

-- Could be none or some. Not general in that sense, but nothing specific to SWHS.

s4 = Section 0 (S "Specific System Description") [Con s4_intro, Sub s4_1, Sub s4_2]

s4_intro = Paragraph (S "This section first presents the problem " :+:
           S "description, which gives a high-level view of the problem" :+:
           S " to be solved. This is followed by the solution " :+:
           S "characteristics specification, which presents the " :+:
           (sMap (map toLower) (assumption ^. descr)) :+: S "s, " :+: (sMap (map toLower) (thModel ^. descr)) :+: 
           S "s, " :+: (sMap (map toLower) (genDefn ^. descr)) :+: S "s, " :+: 
           (sMap (map toLower) (dataDefn ^. descr)) :+: S "s, and finally the " :+:
           (sMap (map toLower) (inModel ^. descr)) :+: S "s (" :+: S (ordDiffEq ^. name) :+: S "s) that model the solar water heating" :+:
           S " tank with PCM.")

-- SWHS with PCM comes up again. 

s4_1 = Section 1 (S "Problem Description") [Con s4_1_intro, Sub s4_1_1, Sub s4_1_2, Sub s4_1_3]

s4_1_intro = Paragraph (S (progName ^. name) :+: S " is a computer program developed to " :+:
             S "investigate the effect of employing PCM within a solar " :+:
             S "water heating tank.")

-- specific to SWHS... "employing PCM within a SWHT" is "SWHS incorporating PCM" backwards.

s4_1_1 = Section 2 (S "Terminology and Definitions") [Con s4_1_1_intro, Con s4_1_1_bullets]

s4_1_1_intro = Paragraph (S "This subsection provides a list of terms " :+:
               S "that are used in the subsequent sections and their " :+:
               S "meaning, with the purpose of reducing ambiguity and " :+:
               S "making it easier to correctly understand the requirements:")

-- Completely general paragraph.

s4_1_1_bullets = Enumeration (Bullet $ map (\c -> Flat (S (c ^. name) :+:
  S ": " :+: (c ^. descr))) [heat_flux, phase_change_material, specific_heat, thermal_conduction, transient])

--Is this how I should be doing this BulletList?
--For now, added Concepts.hs to hold ConceptChunks.
--Included heat flux and specific heat even though they are already in SWHSUnits

s4_1_2 = Section 2 (S "Physical System Description") [Con s4_1_2_intro, Con s4_1_2_list, Con fig_tank]

s4_1_2_intro = Paragraph (S "The physical system of " :+: S (progName ^. name) :+: S ", as shown in " :+:
               (makeRef fig_tank) :+: S ", includes the following elements:")

-- General paragraph.

s4_1_2_list = Enumeration (Simple $ [(S (physSyst ^. name) :+: S "1", Flat (S "Tank containing water.")),
              (S (physSyst ^. name) :+: S "2", Flat (S "Heating coil at bottom of tank. (" :+:
              U (ht_flux_C ^. symbol) :+: S " represents the " :+: (ht_flux_C ^. descr) :+:
              S " into the water.)")),
              (S (physSyst ^. name) :+: S "3", Flat (S (phsChgMtrl ^. name) :+: S " suspended in tank. (" :+: U (ht_flux_P ^. symbol) :+:
              S " represents the " :+: (ht_flux_P ^. descr) :+: S " from the water.)"))])

fig_tank = Figure (S "Solar water heating tank, with " :+: (ht_flux_C ^. descr) :+:
           S " of " :+: U (ht_flux_C ^. symbol) :+: S " and " :+: (ht_flux_P ^. descr) :+:
           S " of " :+: U (ht_flux_P ^. symbol)) "Tank.png"

-- Lots of specifics...

s4_1_3 = Section 2 ((goalStmt ^. descr) :+: S "s") [Con s4_1_3_intro, Con s4_1_3_list]

s4_1_3_intro = Paragraph (S "Given the temperature of the coil, initial " :+:
               S "conditions for the temperature of the water and the " :+: S (phsChgMtrl ^. name) :+: S ", " :+:
               S "and material properties, the " :+: (sMap (map toLower) (goalStmt ^. descr)) :+: S "s are:")

s4_1_3_list = Enumeration (Simple [(S (goalStmt ^. name) :+: S "1", Flat (S "predict the water temperature over time;")),
              (S (goalStmt ^. name) :+: S "2", Flat (S "predict the PCM temperature over time;")),
              (S (goalStmt ^. name) :+: S "3", Flat (S "predict the change in the energy of the water over time;")),
              (S (goalStmt ^. name) :+: S "4", Flat (S "predict the change in the energy of the PCM over time"))])

--Given how frequently these sorts of lists occur, could they be semi automated?
--Would only type "goalStmt ^. name" once, and then a list of the right side statements.

s4_2 = Section 1 (S "Solution Characteristics Specification") [Con s4_2_intro, Sub s4_2_1, Sub s4_2_2, Sub s4_2_3, Sub s4_2_4, Sub s4_2_5, Sub s4_2_6]

s4_2_intro = Paragraph (S "The " :+: (sMap (map toLower) (inModel ^. descr)) :+: S "s (" :+: S (ordDiffEq ^. name) :+: S "s) that govern " :+:
             S (progName ^. name) :+: S " are" :+:
             S " presented in " :+: (makeRef s4_2_5) :+: S ". The information" :+:
             S " to understand the meaning of the " :+: (sMap (map toLower) (inModel ^. descr)) :+: S "s and their " :+:
             S "derivation is also presented, so that the " :+: (sMap (map toLower) (inModel ^. descr)) :+: S "s " :+:
             S "can be verified.")

-- General paragraph.

s4_2_1 = Section 2 (assumption ^. descr :+: S "s") [Con s4_2_1_intro, Con s4_2_1_list]

s4_2_1_intro = Paragraph (S "This section simplifies the original problem " :+:
               S "and helps in developing the " :+: (sMap (map toLower) (thModel ^. descr)) :+: S " by filling" :+:
               S " in the missing information for the physical system. The" :+:
               S " numbers given in the square brackets refer to the " :+:
               (sMap (map toLower) (thModel ^. descr)) :+: S " [" :+: S (thModel ^. name) :+: S "], " :+: (sMap (map toLower) (genDefn ^. descr)) :+: S " [" :+:
               S (genDefn ^. name) :+: S "], " :+: (sMap (map toLower) (dataDefn ^. descr)) :+: S " [" :+: S (dataDefn ^. name) :+:
               S "], " :+: (sMap (map toLower) (inModel ^. descr)) :+: S " [" :+: S (inModel ^. name) :+: S "], or " :+: (sMap (map toLower) (likelyChg ^. descr)) :+:
               S " [" :+: S (likelyChg ^. name) :+: S "], in which the respective " :+: (sMap (map toLower) (assumption ^. descr)) :+: S " is used.") 

-- General paragraph, but can "physical system" be referenced? physSyst is "Physical System Description"

s4_2_1_list = Enumeration (Simple [(S (assumption ^. name) :+: S "1", Flat (S "The only form of energy that is " :+:
              S "relevant for this problem is thermal energy. All other " :+:
              S "forms of energy, such as mechanical energy, are assumed " :+:
              S "to be negligible [" :+: (makeRef s4_2_2_T1) :+: S "].")),
              (S (assumption ^. name) :+: S "2", Flat (S "All heat transfer coefficients are constant over" :+:
              S " time [GD1].")),
              (S (assumption ^. name) :+: S "3", Flat (S "The water in the tank is fully  mixed, so the " :+:
              S "temperature is the same throughout the entire tank [GD2, DD2].")),
              (S (assumption ^. name) :+: S "4", Flat (S "The " :+: S (phsChgMtrl ^. name) :+: S " has the same temperature throughout [GD2, DD2, LC1].")),
              (S (assumption ^. name) :+: S "5", Flat (S "Density of the water and " :+: S (phsChgMtrl ^. name) :+: S " have no spatial " :+:
              S "variation; that is, they are each constant over their " :+:
              S "entire " :+: (volume ^. descr) :+: S " [GD2].")),
              (S (assumption ^. name) :+: S "6", Flat (S "Specific heat capacity of the water and PCM have" :+:
              S " no spatial variation; that is, they are each constant " :+:
              S "over their entire " :+: (volume ^. descr) :+: S " [GD2].")),
              (S (assumption ^. name) :+: S "7", Flat (S "Newton's law of convective cooling applies " :+:
              S "between the coil and the water [" :+: makeRef s4_2_4_DD1 :+: S "].")),
              (S (assumption ^. name) :+: S "8", Flat (S "The temperature of the heating coil is constant " :+:
              S "over time [" :+: makeRef s4_2_4_DD1 :+: S ", LC2].")),
              (S (assumption ^. name) :+: S "9", Flat (S "The temperature of the heating coil does not " :+:
              S "vary along its length [" :+: makeRef s4_2_4_DD1 :+: S ", LC3].")),
              (S (assumption ^. name) :+: S "10", Flat (S "Newton's law of convective cooling applies " :+:
              S "between the water and the PCM [DD2].")),
              (S (assumption ^. name) :+: S "11", Flat (S "The model only accounts for charging of the tank" :+:
              S ", not discharging. The temperature of the water and PCM " :+:
              S "can only increase, or remain constant; they do not decrease" :+:
              S ". This implies that the " :+: (temp_init ^. descr) :+: S " (A12) is " :+:
              S "less than (or equal) to the temperature of the coil [IM1, LC4].")),
              (S (assumption ^. name) :+: S "12", Flat (S "The " :+: (temp_init ^. descr) :+: S " of the water and the " :+:
              S "PCM is the same [IM1, IM2, LC5].")),
              (S (assumption ^. name) :+: S "13", Flat (S "The simulation will start with the " :+: S (phsChgMtrl ^. name) :+: S " in solid" :+:
              S " form [IM2, IM4].")),
              (S (assumption ^. name) :+: S "14", Flat (S "The operating temperature range of the system is " :+:
              S "such that the water is always in liquid form. That is, the " :+:
              S "temperature will not drop below the melting point of water," :+:
              S " or rise above its boiling point [IM1, IM3].")),
              (S (assumption ^. name) :+: S "15", Flat (S "The tank is perfectly insulated so that there is " :+:
              S "no heat loss from the tank [IM1, LC6].")),
              (S (assumption ^. name) :+: S "16", Flat (S "No internal heat is generated by either the water" :+:
              S " or the PCM; therefore, the " :+: (vol_ht_gen ^. descr) :+: S " is " :+:
              S "zero [IM1, IM2].")),
              (S (assumption ^. name) :+: S "17", Flat (S "The volume change of the " :+: S (phsChgMtrl ^. name) :+: S " due to melting is " :+:
              S "negligible [IM2].")),
              (S (assumption ^. name) :+: S "18", Flat (S "The " :+: S (phsChgMtrl ^. name) :+: S " is either in a liquid or solid state, but" :+:
              S " not a gas [IM2, IM4]."))])

-- Can booktabs colored links be used? The box links completely cover nearby punctuation.

s4_2_2 = Section 2 (thModel ^. descr :+: S "s") [Con s4_2_2_intro, Con s4_2_2_T1, Con s4_2_2_T2, Con s4_2_2_T3]

s4_2_2_intro = Paragraph (S "This section focuses on the general equations" :+:
               S " and laws that " :+: S (progName ^. name) :+: S " is based on.")

-- General paragraph

--Theory has to be RelationChunk....
--No way to include "Source" or "Ref. By" sections?

--No subsubsubsections... may make things difficult for derivation sections coming up

s4_2_3 = Section 2 (genDefn ^. descr :+: S "s") [Con s4_2_3_intro]

s4_2_3_intro = Paragraph (S "This section collects the laws and equations " :+:
               S "that will be used in deriving the " :+: (sMap (map toLower) (dataDefn ^. descr)) :+: S "s, which" :+:
               S " in turn are used to build the " :+: (sMap (map toLower) (inModel ^. descr)) :+: S "s.")

-- General paragraph, just need to reference lowercase concepts.
 
-- s4_2_3_GDs :: [LayoutObj]
-- s4_2_3_GDs = map Definition (map General [gd1NewtonCooling])

--General definitions not yet implemented

s4_2_4 = Section 2 (dataDefn ^. descr :+: S "s") [Con s4_2_4_intro, Con s4_2_4_DD1, Con s4_2_4_DD2, Con s4_2_4_DD3, Con s4_2_4_DD4]

s4_2_4_intro = Paragraph (S "This section collects and defines all the " :+:
               S "data needed to build the " :+: (sMap (map toLower) (inModel ^. descr)) :+: S "s. The dimension" :+:
               S " of each quantity is also given.")

-- General paragraph, just need to reference lowercase instance model.

s4_2_4_DD1, s4_2_4_DD2, s4_2_4_DD3, s4_2_4_DD4 :: Contents
s4_2_4_DD1 = Definition (Data dd1HtFluxC)
s4_2_4_DD2 = Definition (Data dd2HtFluxP)
s4_2_4_DD3 = Definition (Data dd3HtFusion)
s4_2_4_DD4 = Definition (Data dd4MeltFrac)

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear

s4_2_5 = Section 2 (inModel ^. descr :+: S "s") (map Con s4_2_5_intro)

s4_2_5_intro = [Paragraph (S "This section transforms the problem defined" :+:
               S " in " :+: (makeRef s4_1) :+: S " into one which" :+:
               S " is expressed in mathematical terms. It uses concrete " :+:
               S "symbols defined in " :+: (makeRef s4_2_4) :+:
               S " to replace the abstract symbols in the models identified" :+:
               S " in " :+: (makeRef s4_2_2) :+: S " and " :+:
               (makeRef s4_2_3) :+: S "."), 
               Paragraph (S "The goals GS1 to GS4 are solved by IM1 to IM4." :+:
               S " The solutions for IM1 and IM2 are coupled since the " :+:
               S "solution for " :+: U (temp_W ^. symbol) :+: S " and " :+:
               U (temp_PCM ^. symbol) :+: S " depend on one another. IM3 " :+:
               S "can be solved once IM1 has been solved. The solution of " :+:
               S "IM2 and IM4 are also coupled, since the temperature and " :+:
               S "energy of the " :+: S (phsChgMtrl ^. name) :+: S " depend on the phase change.")]

--Instance Models aren't implemented yet

-- Some specific info here on the order in which IMs are solved... probably can be captured.

s4_2_6 = Section 2 (S "Data Constraints") ((map Con s4_2_6_intro)++[Con table1])

s4_2_6_intro = [Paragraph ((makeRef table1) :+: S " show the data " :+:
               S "constraints on the input and output variables, respectively" :+:
               S ". The column for physical constraints gives the physical " :+:
               S "limitations on the range of values that can be taken by " :+:
               S "the variable. The column for software constraints restricts" :+:
               S " the range of inputs to reasonable values. The constraints" :+:
               S " are conservative, to give the user of the model the " :+:
               S "flexibility to experiment with unusual situations. The " :+:
               S "column of typical values is intended to provide a feel " :+:
               S "for a common scenario. The uncertainty column provides " :+:
               S "an estimate of the confidence with which the physical " :+:
               S "quantities can be measured. This information would be " :+:
               S "part of the input if one were performing an uncertainty " :+:
               S "quantification exercise."), Paragraph (S "The specification" :+:
               S " parameters in " :+: makeRef table1 :+: S " are listed in " :+:
               S "Table 2.")]

-- Completely general paragraph.
--Reference Table 2 above

inputVar :: [UnitalChunk]
inputVar = [tank_length, diam, pcm_vol, pcm_SA, pcm_density, temp_melt_P,
  htCap_S_P, htCap_L_P, htFusion, coil_SA, temp_C, w_density, htCap_W, 
  coil_HTC, pcm_HTC, temp_init, time_final]
  
-- typicalValues :: [Sentence]
-- typicalValues = [S "1.5 " :+: Sy (metre ^. unit), S "0.412 " :+: Sy (metre ^. unit),
  -- S "0.05 " :+: Sy (m_3 ^. unit), S "1.2 " :+: Sy (m_2 ^. unit), S "1007 " :+:
  -- Sy (densityU ^. unit), S "44.2 " :+: Sy (centigrade ^. unit), S "1760 " :+:
  -- Sy (heat_capacity ^. unit), S "2270 " :+: Sy (heat_capacity ^. unit), 
  -- S "211600 " :+: Sy (specificE ^. unit), S "0.12 " :+: Sy (m_2 ^. unit),
  -- S "50 " :+: Sy (centigrade ^. unit), S "1000 " :+: Sy (densityU ^. unit),
  -- S "4186 " :+: Sy (heat_capacity ^. unit), S "1000 " :+: Sy (heat_transfer ^. unit),
  -- S "1000 " :+: Sy (heat_transfer ^. unit), S "40 " :+: Sy (centigrade ^. unit),
  -- S "50000 " :+: Sy (second ^. unit)]
  
  --Typical values and constraints must be added to UC definitions for mkTable to work here.

table1 = Table [S "Var", S "Physical Constraints", S "Software Constraints",
         S "Typical Value", S "Uncertainty"] (mkTable
         [\ch -> U (ch ^. symbol),
         \ch -> Sy (ch ^. unit),
         \ch -> Sy (ch ^. unit),
         \ch -> Sy (ch ^. unit),
         \ch -> Sy (ch ^. unit)] inputVar)
         (S "Input Variables") True

--Add constraints (and typical values) to the knowledge capture of each variable, so that lambdas can be used to extract constraints?
-- Add "Uncertainty" to UnitalChunks??
--Other Notes:
----Will there be a way to have asterisks for certain pieces of the table?

--Tables 2 and 3 will be delayed for now bc they are similar to table 1

-- s4_2_7 = Section 2 (S "Properties of a Correct Solution") s4_2_7_deriv

-- s4_2_7_deriv = [Paragraph (S "A correct solution must exhibit the law " :+:
               -- S "of conservation of energy. This law is represented by " :+:
               -- makeRef s4_2_2_T1 :+: S ". Since there is no internal heat" :+:
               -- S "generation (A16), " :+: S "GD2 can be simplified to"), 
               -- EqnBlock (U (ht_flux_in ^. symbol) :+: U (in_SA ^. symbol) :+: 
               -- S "-" :+: U (ht_flux_out ^. symbol) :+: U (out_SA ^. symbol) :+: 
               -- S "=" :+: U (mass ^. symbol) :+: U (htCap ^. symbol) :+:
               -- ((S "d" :+: U (temp ^. symbol)) :/: (S "d" :+: U (time ^. symbol))) :+: S "."),
               -- Paragraph (S "Applying this to the water in the system, the " :+:
               -- S "equation becomes"),
               -- EqnBlock (U (ht_flux_C ^. symbol) :+: U (coil_SA ^. symbol) :+:
               -- S "-" :+: U (ht_flux_P ^. symbol) :+: U (pcm_SA ^. symbol) :+:
               -- S "=" :+: U (w_mass ^. symbol) :+: U (htCap_W ^. symbol) :+:
               -- ((S "d" :+: U (temp_W ^. symbol)) :/: (S "d" :+: U (time ^. symbol))) :+: S "."),
               -- Paragraph (S "With regards to PCM, the only heat flux is " :+:
               -- S "between PCM and water. Thus, GD2 for solid PCM is"),
               -- EqnBlock (U (ht_flux_P ^. symbol) :+: U (pcm_SA ^. symbol) :+:
               -- S "=" :+: U (pcm_mass ^. symbol) :+: U (ht_flux_S_P ^. symbol) :+:
               -- ((S "d" :+: U (temp_PCM ^. symbol)) :/: (S "d" :+: U (time ^. symbol))) :+: S "."),
               -- Paragraph (S "Similarly, GD2 for liquid PCM yields"),
               -- EqnBlock (U (ht_flux_P ^. symbol) :+: U (pcm_SA ^. symbol) :+:
               -- S "=" :+: U (pcm_mass ^. symbol) :+: U (htCap_L_P ^. symbol) :+:
               -- ((S "d" :+: U (temp_PCM ^. symbol)) :/: (S "d" :+: U (time ^. symbol))) :+: S "."),
               -- Paragraph (S "Adding equations (7) and (8) results in an overall" :+:
               -- S " energy balance equation for the case where PCM is solid."),
               -- EqnBlock (U (ht_flux_C ^. symbol) :+: U (coil_SA ^. symbol) :+:
               -- S "=" :+: U (w_mass ^. symbol) :+: U (htCap_W ^. symbol) :+:
               -- ((S "d" :+: U (temp_W ^. symbol)) :/: (S "d" :+: U (time ^. symbol))) :+:
               -- S "+" :+: U (pcm_mass ^. symbol) :+: U (ht_flux_S_P ^. symbol) :+:
               -- ((S "d" :+: U (temp_PCM ^. symbol)) :/: (S "d" :+: U (time ^. symbol)))),
               -- Paragraph (S "Substituting " :+: makeRef s4_2_4_DD1 :+: S "yields"),
               -- EqnBlock (U (coil_HTC ^. symbol) :+: U (coil_SA ^. symbol) :+:
               -- S "(" :+: U (temp_C ^. symbol) :+: S "-" :+: U (temp_W ^. symbol) :+:
               -- S "(" :+: U (time ^. symbol) :+: S "))=" :+: U (w_mass ^. symbol) :+:
               -- U (htCap_W ^. symbol) :+: ((S "d" :+: U (temp_W ^. symbol)) :/:
               -- (S "d" :+: U (time ^. symbol))) :+: S "+" :+: U (pcm_mass ^. symbol) :+:
               -- U (ht_flux_S_P ^. symbol) :+: ((S "d" :+: U (temp_PCM ^. symbol)) :/:
               -- (S "d" :+: U (time ^. symbol))) :+: S "."),
               -- Paragraph (S "Similarly, the overall energy balance for " :+:
               -- S "liquid PCM is"),
               -- EqnBlock (U (coil_HTC ^. symbol) :+: U (coil_SA ^. symbol) :+:
               -- S "(" :+: U (temp_C ^. symbol) :+: S "-" :+: U (temp_W ^. symbol) :+:
               -- S "(" :+: U (time ^. symbol) :+: S "))=" :+: U (w_mass ^. symbol) :+:
               -- U (htCap_W ^. symbol) :+: ((S "d" :+: U (temp_W ^. symbol)) :/:
               -- (S "d" :+: U (time ^. symbol))) :+: S "+" :+: U (pcm_mass ^. symbol) :+:
               -- U (ht_flux_L_P ^. symbol) :+: ((S "d" :+: U (temp_PCM ^. symbol)) :/:
               -- (S "d" :+: U (time ^. symbol))) :+: S "."),
               -- Paragraph (S "For the case where PCM is in the process of " :+:
               -- S "melting, its temperature does not change (IM2). Thus, " :+:
               -- S "the overall energy balance is simply equation (7), which " :+:
               -- S "when combined with " :+: makeRef s4_2_4_DD1 :+: S " and " :+:
               -- makeRef s4_2_4_DD2 :+: " becomes"),
               -- EqnBlock (U (coil_HTC ^. symbol) :+: U (coil_SA ^. symbol) :+:
               -- S "(" :+: U (temp_C ^. symbol) :+: S "-" :+: U (temp_W ^. symbol) :+:
               -- S "(" :+: U (time ^. symbol) :+: S "))-" :+: U (pcm_HTC ^. symbol) :+:
               -- U (pcm_SA ^. symbol) :+: S "(" :+: U (temp_W ^. symbol) :+:
               -- S "(" :+: U (time ^. symbol) :+: S ")-" :+: U (temp_melt_P ^. symbol) :+:
               -- S ")=" :+: U (w_mass ^. symbol) :+: U (htCap_W ^. symbol) :+:
               -- ((S "d" :+: U (temp_W ^. symbol)) :/: (S "d" :+: U (time ^. symbol))) :+: S "."),
               -- Paragraph (S "Equations (9) and (11) can be used as \"sanity\"" :+:
               -- S " checks to gain confidence in any solution computed by " :+:
               -- S (progName ^. name) :+: S "."]
 
--I much prefer a reference like "T1" compared to the reference above
----which does "Definition T:Cote"

--How to do fractions in equations?
---- I feel like :/: should work but it is not being recognized...
--Should I even be using EqnBlock?

s5 = Section 0 ((requirement ^. descr) :+: S "s") [Con s5_intro, Sub s5_1, Sub s5_2]

s5_intro = Paragraph (S "This section provides the functional " :+: (sMap (map toLower) (requirement ^. descr)) :+: S "s" :+:
           S ", the business tasks that the software is expected to complete" :+:
           S ", and the nonfunctional " :+: (sMap (map toLower) (requirement ^. descr)) :+:S "s, the qualities that " :+:
           S "the software is expected to exhibit.")

-- Completely general paragraph.

s5_1 = Section 1 (S "Functional " :+: (requirement ^. descr) :+: S "s") (map Con s5_1_list)

s5_1_list = [Enumeration (Simple [(S (requirement ^. name) :+: S "1", Flat (S "Input the following quantities, " :+:
            S "which define the tank parameters, material properties" :+:
            S " and initial conditions:"))]), 
            (Table [S "symbol", S "unit", S "description"] (mkTable
            [(\ch -> U (ch ^. symbol)),
            (\ch -> Sy (ch ^. unit)),
            (\ch -> ch ^. descr)
            ] inputVar) (S "Input Variable Requirement") False),
            Enumeration (Simple [(S (requirement ^. name) :+: S "2", Flat (S "Use the inputs in R1 to find the mass needed " :+:
            S "for IM1 to IM4, as follows, where " :+: U (w_vol ^. symbol) :+: S " is " :+:
            S "the " :+: (w_vol ^. descr) :+: S " and " :+: U (tank_vol ^. symbol) :+:
            S " is the " :+: (tank_vol ^. descr) :+: S "."))]),
            EqnBlock (U (w_mass ^. symbol) :+: S "=" :+: U (w_vol ^. symbol) :+:
            U (w_density ^. symbol) :+: S "=(" :+: U (tank_vol ^. symbol) :+:
            S "-" :+: U (pcm_vol ^. symbol) :+: S ")" :+: U (w_density ^. symbol) :+:
            S "=(" :+:  S "(" :+: U (diam ^. symbol) :+: S "/2)" :+: S "2" :+:
            U (tank_length ^. symbol) :+: S "-" :+: U (pcm_vol ^. symbol) :+:
            S ")" :+: U (w_density ^. symbol)),
            EqnBlock (U (pcm_mass ^. symbol) :+: S "=" :+: U (pcm_vol ^. symbol) :+:
            U (pcm_density ^. symbol) :+: S ","),
            Enumeration (Simple [(S (requirement ^. name) :+: S "3", Flat (S "Verify that the inputs satisfy the required physical" :+:
            S " constraints shown in " :+: makeRef table1 :+: S ".")),
            (S (requirement ^. name) :+: S "4", Flat (S "Output the input quantities and derived quantities " :+:
            S "in the following list: the quantities from R1, the masses " :+:
            S "from R2, " :+: U (tau_W ^. symbol) :+: S " (from IM1), " :+: U (eta ^. symbol) :+: S " (from IM1), " :+:
            U (tau_S_P ^. symbol) :+: S " (from IM2) and " :+: U (tau_L_P ^. symbol) :+:
            S " (from IM2).")),
            (S (requirement ^. name) :+: S "5", Flat (S "Calculate and output the " :+: (temp_W ^. descr) :+: S " (" :+:
            U (temp_W ^. symbol) :+: S "(" :+: U (time ^. symbol) :+: S ")) " :+:
            S "over the simulation time (from IM1).")),
            (S (requirement ^. name) :+: S "6", Flat (S "Calculate and output the " :+: (temp_PCM ^. descr) :+: S " (" :+:
            U (temp_PCM ^. symbol) :+: S "(" :+: U (time ^. symbol) :+: S ")) " :+:
            S "over the simulation time (from IM2).")),
            (S (requirement ^. name) :+: S "7", Flat (S "Calculate and output the " :+: (w_E ^. descr) :+: S " (" :+:
            U (w_E ^. symbol) :+: S "(" :+: U (time ^. symbol) :+: S ")) " :+:
            S "over the simulation time (from IM3).")),
            (S (requirement ^. name) :+: S "8", Flat (S "Calculate and output the " :+: (pcm_E ^. descr) :+: S " (" :+:
            U (pcm_E ^. symbol) :+: S "(" :+: U (time ^. symbol) :+: S ")) " :+:
            S "over the simulation time (from IM4).")),
            (S (requirement ^. name) :+: S "9", Flat (S "Calculate and output the time at which the " :+: S (phsChgMtrl ^. name) :+: S " begins" :+:
            S " to melt " :+: U (t_init_melt ^. symbol) :+: S " (from IM2).")),
            (S (requirement ^. name) :+: S "10", Flat (S "Calculate and output the time at which the " :+: S (phsChgMtrl ^. name) :+:
            S " stops melting " :+: U (t_final_melt ^. symbol) :+: S " (from IM2)."))])
            ]

--How to include pi?
--How to add exponents?
--How to add equations in SimpleLists? 

s5_2 = Section 1 (S "Nonfunctional " :+: (requirement ^. descr) :+: S "s") [Con s5_2_contents]

s5_2_contents = Paragraph (S "Given the small size, and relative simplicity" :+:
                S ", of this problem, performance is not a priority. Any " :+:
                S "reasonable implementation will be very quick and use " :+:
                S "minimal storage. Rather than performance, the priority " :+:
                S "nonfunctional " :+: (sMap (map toLower) (requirement ^. descr)) :+: S "s are correctness, verifiability" :+:
                S ", understandability, reusability, and maintainability.")

-- Specific info here... potentially can be captured though. Might require dropping
-- some of the explanantion about small size, etc.

s6 = Section 0 ((likelyChg ^. descr) :+: S "s") [Con s6_list]

s6_list = Enumeration (Simple [(S (likelyChg ^. name) :+: S "1", Flat (S "A4 - " :+: S (phsChgMtrl ^. name) :+: S " is actually a poor thermal " :+:
          S "conductor, so the assumption of uniform " :+: (temp_PCM ^. descr) :+: S " is " :+:
          S "not likely.")), 
          (S (likelyChg ^. name) :+: S "2", Flat (S "A8 - The " :+: (temp_C ^. descr) :+:
          S " will change over the course of the day, depending" :+:
          S " on the energy received from the sun.")),
          (S (likelyChg ^. name) :+: S "3", Flat (S "A9 - The temperature of the water in the coil will " :+:
          S "actually change along its length as the water cools.")),
          (S (likelyChg ^. name) :+: S "4", Flat (S "A11 - The model currently only accounts for charging " :+:
          S "of the tank. A more complete model would also account for " :+:
          S "discharging of the tank.")),
          (S (likelyChg ^. name) :+: S "5", Flat (S "A12 - To add more flexibility to the simulation, " :+:
          S "the " :+: (temp_init ^. descr) :+: S " of the water and the PCM could be " :+:
          S "allowed to have different values.")),
          (S (likelyChg ^. name) :+: S "6", Flat (S "A15 - Any real tank cannot be perfectly insulated " :+:
          S "and will lose heat."))])

--add referencing to assumptions?		
  
s7 = Section 0 (S "Traceability Matrix") ((map Con s7_intro)++[Con s7_table])

s7_intro = [Paragraph (S "The purpose of this matrix is to provide an easy " :+:
           S "reference on what has to be additionally modified if a certain" :+:
           S " component is changed. Every time a component is changed, the " :+:
           S "items in the column of that component that are marked with an " :+:
           Quote (S "X") :+: S " should be modified as well."),
           Paragraph (S "NOTE: The traceability matrix shown in " :+: makeRef s7_table :+:
           S " is not the full traceability matrix for " :+: S (progName ^. name) :+:
           S ". It is a subset developed to fit the matrix in one page. " :+:
           S "Building a tool to automatically generate the graphical " :+:
           S "representation of the matrix by scanning the labels and " :+:
           S "reference can be future work.")]

-- Completely general paragraph.

s7_table = Table [S "", S "T1", S "T2", S "T3", S "A1", S "A2", S "A3", 
           S "A4", S "GD1", S "GD2", S "DD1", S "DD2", S "DD3", S "DD4"]
           [[S "GD1", S "", S "", S "", S "", S "X", S "", S "", S "", S "",
           S "", S "", S "", S ""],
           [S "GD2", S "X", S "", S "", S "", S "X", S "X", S "X", S "X",
           S "", S "", S "", S "", S ""],
           [S "DD1", S "", S "", S "", S "", S "", S "", S "", S "X", S "",
           S "", S "", S "", S ""],
           [S "DD2", S "", S "", S "", S "", S "", S "X", S "X", S "X", S "",
           S "", S "", S "", S ""],
           [S "DD3", S "", S "", S "", S "", S "", S "", S "", S "", S "",
           S "", S "", S "", S ""],
           [S "DD4", S "", S "", S "", S "", S "", S "", S "", S "", S "",
           S "", S "", S "X", S ""],
           [S "IM1", S "", S "", S "", S "", S "", S "", S "", S "", S "X",
           S "X", S "X", S "", S ""],
           [S "IM2", S "", S "", S "", S "", S "", S "", S "", S "", S "X",
           S "", S "X", S "", S "X"],
           [S "IM3", S "", S "X", S "", S "", S "", S "", S "", S "", S "",
           S "", S "", S "", S ""],
           [S "IM4", S "", S "X", S "X", S "", S "", S "", S "", S "", S "",
           S "", S "X", S "X", S "X"],
           [S "T1", S "", S "", S "", S "X", S "", S "", S "", S "", S "",
           S "", S "", S "", S ""],
           [S "T2", S "", S "", S "X", S "", S "", S "", S "", S "", S "",
           S "", S "", S "", S ""],
           [S "T3", S "", S "", S "", S "", S "", S "", S "", S "", S "",
           S "", S "", S "", S "X"]]
           (S "Traceability Matrix Showing the Connections Between Items " :+:
           S "of Different Sections") True

-- This table being automated = The Dream!!

--References?
