{-# OPTIONS -Wall #-} 
{-# LANGUAGE FlexibleContexts #-} 
module Example.Drasil.SWHS.Body where

import Data.Char (toLower)
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
  s4_2_6, s4_2_7, s5, s5_1, s5_2, s6, s7 :: Section

s1_intro, s1_1_intro, s1_1_table, s1_2_intro, s1_2_table, s1_3_table,
  s2_2_contents, s3_intro, s3_1_contents, s3_2_contents, s4_intro, 
  s4_1_intro, s4_1_1_intro, s4_1_1_bullets, s4_1_2_intro, s4_1_2_list,
  fig_tank, s4_1_3_intro, s4_1_3_list, s4_2_intro, s4_2_1_intro, 
  s4_2_1_list, s4_2_2_intro, s4_2_3_intro, s4_2_4_intro, s4_2_6_intro, 
  table1, s5_intro, s5_2_contents, s6_list, s7_intro1, s7_table1, s7_table2,
  s7_table3, s7_fig1, s7_fig2 :: Contents
  
s2_intro, s2_1_contents, s2_3_contents, s4_2_3_deriv, s4_2_5_intro, 
  s4_2_5_deriv1, s4_2_5_deriv2, s4_2_7_deriv, s5_1_list, s7_intro2 :: [Contents]

swhs_srs :: Document
swhs_srs = Document (S "Software Requirements Specification for Solar Water" :+:
           S " Heating Systems with Phase Change Material")
           (S "Thulasi Jegatheesan, Brooks MacLachlan, and Spencer Smith")
           [s1, s2, s3, s4, s5, s6, s7]

--It is sometimes hard to remember to add new sections both here and above.

-- Beginning of title could be automated

s1 = Section 0 (S "Reference Material") [Con s1_intro, Sub s1_1, Sub s1_2, 
     Sub s1_3]

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
             S " with the " :+: (heat_trans ^. descr) :+: S " literature " :+:
             S "and with existing documentation for " :+: (sMap (map toLower)
             (progName ^. descr)) :+: S "s. The symbols are listed in " :+:
             S "alphabetical order.")

-- "heat transfer" is specific.

s1_2_table = Table [S "Symbol", S "Unit", S "Description"] (mkTable
  [(\ch -> P (ch ^. symbol)),
   (\ch -> Sy (ch ^. unit)),
   (\ch -> ch ^. descr)
   ] swhsSymbols)
   (S "Table of Symbols") False

-- if the lambdas end up being similar for every table, can this be simplified?
  
s1_3 = Section 1 (S "Abbreviations and Acronyms") [Con s1_3_table]

s1_3_table = Table [S "Symbol", S "Description"] (mkTable
  [(\ch -> S (ch ^. name)),
   (\ch -> ch ^. descr)
   ] acronyms)
   (S "Abbrevations and Acronyms") False
   
s2 = Section 0 (S "Introduction") ((map Con s2_intro)++[Sub s2_1, Sub s2_2, 
     Sub s2_3])

s2_intro = [Paragraph (S "Due to increasing cost, diminishing " :+:
           S "availability, and negative environmental impact of " :+:
           S "fossil fuels, there is a higher demand for renewable" :+:
           S " energy sources and energy storage technology. " :+:
           (swhs_pcm ^. descr) :+: S " (" :+: S (phsChgMtrl ^. name) :+: 
           S ") use a renewable energy source and provide a novel way of " :+:
           S "storing energy. " :+: S (swhs_pcm ^. name) :+: S " improve" :+:
           S " over the traditional " :+: (sMap (map toLower) (progName ^. 
           descr)) :+: S "s because of their smaller size. The smaller size" :+:
           S " is possible because of the ability of " :+: 
           S (phsChgMtrl ^. name) :+: S " to store " :+:
           (sMap (map toLower) (S (thermal_energy ^. name))) :+: S " as " :+:
           (sMap (map toLower) (S (latent_heat ^. name))) :+: S ", which " :+:
           S "allows higher " :+: (sMap (map toLower) (S (thermal_energy ^. 
           name))) :+: S " storage capacity per unit weight."),
           Paragraph (S " The following section provides an overview of the" :+:
           S " " :+: (srs ^. descr) :+: S " (" :+: S (srs ^. name) :+:
           S ") for " :+: S (swhs_pcm ^. name) :+: S ". The developed " :+:
           S "program will be referred to as " :+: (progName ^. descr) :+:
           S " (" :+: S (progName ^. name) :+: S "). This section explains " :+:
           S "the purpose of this document, the scope of the system, the " :+:
           S "organization of the document and the characteristics of " :+:
           S "the intended readers.")]

-- ConceptChunks... Sometimes capitalized, sometimes not, sometimes plural, 
-- sometimes not, sometimes need to be used in different tenses. How to 
-- accomodate all this?
-- The first part of this paragraph is specific.

s2_1 = Section 1 (S "Purpose of Document") (map Con s2_1_contents)

s2_1_contents = [Paragraph (S "The main purpose of this document is to " :+:
                S "describe the modelling of " :+: S (swhs_pcm ^. name) :+:
                S ". The " :+: (sMap (map toLower) (goalStmt ^. descr)) :+: 
                S "s and " :+: (sMap (map toLower) (thModel ^. descr)) :+: 
                S "s used in the " :+: S (progName ^. name) :+: S " code " :+:
                S "are provided, with an emphasis on explicitly identifying" :+:
                S " " :+: (sMap (map toLower) (assumption ^. descr)) :+: 
                S "s and unambiguous definitions. This document is intended" :+:
                S " to be used as a reference to provide ad hoc access to " :+:
                S "all information necessary to understand and verify the " :+:
                S "model. The " :+: S (srs ^. name) :+: S " is abstract " :+: 
                S "because the contents say what problem is being solved, " :+:
                S "but do not say how to solve it."),
                Paragraph (S "This document will be used as a starting " :+:
                S "point for subsequent development phases, including " :+: 
                S "writing the design specification and the software " :+:
                S "verification and validation plan. The design document " :+:
                S "will show how the " :+: (sMap (map toLower) (requirement ^.
                descr)) :+: S "s are to be realized, including decisions " :+:
                S "on the numerical algorithms and programming environment." :+:
                S " The verification and validation plan will show the " :+:
                S "steps that will be used to increase confidence in the " :+:
                S "software documentation and the implementation. Although " :+:
                S "the " :+: S (srs ^. name) :+: S " fits in a series of " :+: 
                S "documents that follow the so-called waterfall model, the" :+:
                S " actual development process is not constrained in any " :+:
                S "way. Even when the process is not waterfall, as Parnas " :+:
                S "and Clements [citation] point out, the most logical way " :+:
                S "to present the documentation is still to " :+:
                Quote (S "fake") :+: S " a rational design process.")]

-- This paragraph is mostly general			
--How to italicize words in sentence?
--How to cite?

s2_2 = Section 1 (S "Scope of Requirements") [Con s2_2_contents]

s2_2_contents = Paragraph (S "The scope of the requirements is limited " :+:
                S "to " :+: (sMap (map toLower) (S (thermal_analysis ^.
                name))) :+: S " of a single " :+: (tank_pcm ^. descr) :+: 
                S ". Given the appropriate inputs, the code for " :+:
                S (progName ^. name) :+: S " is intended to predict the " :+:
                (temp ^. descr) :+: S " and " :+: (sMap (map toLower) (S 
                (thermal_energy ^. name))) :+: S " histories for the " :+:
                (sMap (map toLower) (S (water ^. name))) :+: S " and the " :+:
                S (phsChgMtrl ^. name) :+: S ". This entire document" :+:
                S " is written assuming that the substances inside the " :+:
                (sMap (map toLower) (tank ^. descr)) :+: S " are " :+:
                (sMap (map toLower) (S (water ^. name))) :+: S " and " :+:
                S (phsChgMtrl ^. name) :+: S ".")

-- Lots of specific concepts in this paragraph that can likely be captured.
---- Heating tank, temperature and energy
-- The fact that "PCM" must always be capital is especially making things 
-- difficult with concept chunks involving PCM.

s2_3 = Section 1 (S "Organization of Document") (map Con s2_3_contents)

s2_3_contents = [Paragraph (S "The organization of this document follows" :+:
                S " the template for an " :+: S (srs ^. name) :+: S " for " :+:
                S "scientific computing software proposed by [citation] and" :+:
                S " [citation]. The presentation follows the standard " :+:
                S "pattern for presenting " :+: (sMap (map toLower) (goalStmt ^.
                descr)) :+: S "s, " :+: (sMap (map toLower) (thModel ^. 
                descr)) :+: S "s, " :+: (sMap (map toLower) (dataDefn ^.
                descr)) :+: S "s, and " :+: (sMap (map toLower) (assumption ^.
                descr)) :+: S "s. For readers that would like a more bottom" :+: 
                S " up approach, they can start reading the " :+: (sMap (map  
                toLower) (inModel ^. descr)) :+: S "s in " :+: 
                makeRef s4_2_5 :+: S " and trace back to find any " :+:
                S "additional information they require. The " :+:
                (sMap (map toLower) (inModel ^. descr)) :+: S "s provide " :+:
                S "the " :+: (ordDiffEq ^. descr) :+: S " (" :+: S (ordDiffEq ^.
                name) :+: S "s) and algebraic equations that model the " :+:
                S (swhs_pcm ^. name) :+: S ". " :+: S (progName ^. name) :+:
                S " solves these " :+: S (ordDiffEq ^. name) :+: S "s."),
                Paragraph (S "The " :+: (sMap (map toLower) (goalStmt ^. descr))
                :+: S "s are refined to the " :+: (sMap (map toLower) 
                (thModel ^. descr)) :+: S "s, and " :+: (sMap (map toLower) 
                (thModel ^. descr)) :+: S "s to the " :+: (sMap (map toLower)
                (inModel ^. descr)) :+: S "s. The " :+: (sMap (map toLower) 
                (inModel ^. descr)) :+: S "s (" :+: makeRef s4_2_5 :+:
                S ") to be solved are referred to as IM1 to IM4.")]


-- The swhs_pcm reference at the end of the first paragraph would be better if 
-- singular, but concept is plural.
-- IM1 to IM4 : reference later

-- how to cite/reference?

-- If all SRS have the same basic layout, is it possible to automate
-- the sectioning? This would also improve the tediousness of declaring 
-- LayoutObjs

s3 = Section 0 (S "General System Description") [Con s3_intro, Sub s3_1, 
     Sub s3_2]

s3_intro = Paragraph (S "This section provides general information about " :+:
           S "the system, identifies the interfaces between the system and" :+:
           S " its environment, and describes the user characteristics and" :+:
           S " the system constraints.")

-- Completely general paragraph

s3_1 = Section 1 (S "User Characteristics") [Con s3_1_contents]

s3_1_contents = Paragraph (S "The end user of " :+: S (progName ^. name) :+: 
                S " should have an understanding of undergraduate Level 1 " :+:
                S "Calculus and Physics.")

-- Should "UG Level 1 Calculus" and "UG Level 1 Physics" be concepts?

s3_2 = Section 1 (S "System Constraints") [Con s3_2_contents]

s3_2_contents = Paragraph (S "There are no system constraints.")

-- Could be none or some. 
-- Not general in that sense, but nothing specific to SWHS.

s4 = Section 0 (S "Specific System Description") [Con s4_intro, Sub s4_1, 
     Sub s4_2]

s4_intro = Paragraph (S "This section first presents the problem " :+:
           S "description, which gives a high-level view of the problem" :+:
           S " to be solved. This is followed by the solution " :+:
           S "characteristics specification, which presents the " :+:
           (sMap (map toLower) (assumption ^. descr)) :+: S "s, " :+: 
           (sMap (map toLower) (thModel ^. descr)) :+: S "s, " :+: 
           (sMap (map toLower) (genDefn ^. descr)) :+: S "s, " :+: 
           (sMap (map toLower) (dataDefn ^. descr)) :+: S "s, and finally " :+:
           S "the " :+: (sMap (map toLower) (inModel ^. descr)) :+: S "s (" :+:
           S (ordDiffEq ^. name) :+: S "s) that model the " :+: 
           S (swhs_pcm ^. name) :+: S ".")

-- The swhs_pcm reference at the end would be better if singular, but concept is
-- plural.

s4_1 = Section 1 (S "Problem Description") [Con s4_1_intro, Sub s4_1_1, 
       Sub s4_1_2, Sub s4_1_3]

s4_1_intro = Paragraph (S (progName ^. name) :+: S " is a computer program " :+:
             S "developed to investigate the effect of employing " :+:
             S (phsChgMtrl ^. name) :+: S " within a " :+: 
             (sMap (map toLower) (tank ^. descr)) :+: S ".")

s4_1_1 = Section 2 (S "Terminology and Definitions") [Con s4_1_1_intro, 
         Con s4_1_1_bullets]

s4_1_1_intro = Paragraph (S "This subsection provides a list of terms " :+:
               S "that are used in the subsequent sections and their " :+:
               S "meaning, with the purpose of reducing ambiguity and " :+:
               S "making it easier to correctly understand the requirements:")

-- Completely general paragraph.

s4_1_1_bullets = Enumeration (Bullet $ map (\c -> Flat (S (c ^. name) :+:
                 S ": " :+: (c ^. descr))) [heat_flux, phase_change_material,
                 specific_heat, thermal_conduction, transient])

--Is this how I should be doing this BulletList?
--For now, added Concepts.hs to hold ConceptChunks.
--Included heat flux and specific heat even though they are already in SWHSUnits

s4_1_2 = Section 2 (physSyst ^. descr) [Con s4_1_2_intro, Con s4_1_2_list, 
         Con fig_tank]

s4_1_2_intro = Paragraph (S "The physical system of " :+: S (progName ^. name)
               :+: S ", as shown in " :+: (makeRef fig_tank) :+:
               S ", includes the following elements:")

-- General paragraph.

s4_1_2_list = Enumeration (Simple $ [(S (physSyst ^. name) :+: S "1", Flat
              (S (tank ^. name) :+: S " containing " :+: (sMap (map toLower) 
              (S (water ^. name))) :+: S ".")),
              (S (physSyst ^. name) :+: S "2", Flat (S (coil ^. name) :+: 
              S " at bottom of " :+: (sMap (map toLower) (S (tank ^. name))) :+:
              S ". (" :+: P (ht_flux_C ^. symbol) :+: S " represents the " :+:
              (ht_flux_C ^. descr) :+: S ".)")),
              (S (physSyst ^. name) :+: S "3", Flat (S (phsChgMtrl ^. name) :+: 
              S " suspended in " :+: (sMap (map toLower) (S (tank ^. name))) :+:
              S ". (" :+: P (ht_flux_P ^. symbol) :+: S " represents the " :+:
              (ht_flux_P ^. descr) :+: S ".)"))])

fig_tank = Figure ((tank ^. descr) :+: S ", with " :+: (ht_flux_C ^. descr) :+:
           S " of " :+: P (ht_flux_C ^. symbol) :+: S " and " :+: 
           (ht_flux_P ^. descr) :+: S " of " :+: P (ht_flux_P ^. symbol)) 
           "../../../Tank.png"

s4_1_3 = Section 2 ((goalStmt ^. descr) :+: S "s") [Con s4_1_3_intro, 
         Con s4_1_3_list]

s4_1_3_intro = Paragraph (S "Given the " :+: (temp_C ^. descr) :+: S ", " :+:
               S "initial conditions for the " :+: (temp_W ^. descr) :+:
               S " and the " :+: (temp_PCM ^. descr) :+: S ", and " :+:
               S "material properties, the " :+:
               (sMap (map toLower) (goalStmt ^. descr)) :+: S "s are:")

s4_1_3_list = Enumeration (Simple [(S (goalStmt ^. name) :+: S "1", Flat 
              (S "Predict the " :+: (temp_W ^. descr) :+: S " over " :+:
              (time ^. descr) :+: S ".")),
              (S (goalStmt ^. name) :+: S "2", Flat (S "Predict the " :+:
              (temp_PCM ^. descr) :+: S " over " :+: (time ^. descr) :+: 
              S ".")),
              (S (goalStmt ^. name) :+: S "3", Flat (S "Predict the " :+:
              (w_E ^. descr) :+: S " over " :+: (time ^. descr) :+: S ".")),
              (S (goalStmt ^. name) :+: S "4", Flat (S "Predict the " :+:
              (pcm_E ^. descr) :+: S " over " :+: (time ^. descr) :+: S "."))])

--Given how frequently these sorts of lists occur, could they be semi automated?
--Would only type "goalStmt ^. name" once, and then a list of the right side 
--statements.

s4_2 = Section 1 (S "Solution Characteristics Specification") [Con s4_2_intro, 
       Sub s4_2_1, Sub s4_2_2, Sub s4_2_3, Sub s4_2_4, Sub s4_2_5, Sub s4_2_6, 
       Sub s4_2_7]

s4_2_intro = Paragraph (S "The " :+: (sMap (map toLower) (inModel ^. descr)) :+:
             S "s (" :+: S (ordDiffEq ^. name) :+: S "s) that govern " :+:
             S (progName ^. name) :+: S " are" :+: S " presented in " :+: 
             (makeRef s4_2_5) :+: S ". The information to understand the " :+:
             S "meaning of the " :+: (sMap (map toLower) (inModel ^. descr)) :+:
             S "s and their derivation is also presented, so that the " :+:
             (sMap (map toLower) (inModel ^. descr)) :+: S "s " :+:
             S "can be verified.")

-- General paragraph.

s4_2_1 = Section 2 (assumption ^. descr :+: S "s") [Con s4_2_1_intro, 
         Con s4_2_1_list]

s4_2_1_intro = Paragraph (S "This section simplifies the original problem " :+:
               S "and helps in developing the " :+: (sMap (map toLower) 
               (thModel ^. descr)) :+: S " by filling in the missing " :+:
               S "information for the physical system. The numbers given in" :+:
               S " the square brackets refer to the " :+: (sMap (map toLower) 
               (thModel ^. descr)) :+: S " [" :+: S (thModel ^. name) :+: 
               S "], " :+: (sMap (map toLower) (genDefn ^. descr)) :+: 
               S " [" :+: S (genDefn ^. name) :+: S "], " :+: (sMap (map 
               toLower) (dataDefn ^. descr)) :+: S " [" :+: S (dataDefn ^.
               name) :+: S "], " :+: (sMap (map toLower) (inModel ^. descr)) :+:
               S " [" :+: S (inModel ^. name) :+: S "], or " :+: (sMap (map 
               toLower) (likelyChg ^. descr)) :+: S " [" :+: S (likelyChg ^. 
               name) :+: S "], in which the respective " :+:
               (sMap (map toLower) (assumption ^. descr)) :+: S " is used.") 

-- General paragraph

s4_2_1_list = Enumeration (Simple [(S (assumption ^. name) :+: S "1", Flat 
              (S "The only form of energy that is relevant for this problem" :+:
              S " is " :+: (sMap (map toLower) (S (thermal_energy ^. name))) :+:
              S ". All other forms of energy, such as " :+: (sMap (map toLower) 
              (S (mech_energy ^. name))) :+: S ", are assumed to be " :+:
              S "negligible [" :+: (makeRef s4_2_2_T1) :+: S "].")),
              (S (assumption ^. name) :+: S "2", Flat (S "All " :+: 
              (heat_trans ^. descr) :+: S " coefficients are constant over " :+:
              (time ^. descr) :+: S " [GD1].")),
              (S (assumption ^. name) :+: S "3", Flat (S "The " :+: (sMap 
              (map toLower) (S (water ^. name))) :+: S " in the " :+: (sMap 
              (map toLower) (S (tank ^. name))) :+: S " is fully mixed, so " :+:
              S "the " :+: (temp_W ^. descr) :+: S " is the same throughout" :+:
              S " the entire " :+: (sMap (map toLower) (S (tank ^. name))) :+:
              S " [GD2, " :+: makeRef s4_2_4_DD2 :+: S "].")),
              (S (assumption ^. name) :+: S "4", Flat (S "The " :+: (temp_PCM ^.
              descr) :+: S " is the same throughout the " :+: (pcm_vol ^. 
              descr) :+: S " [GD2, " :+: makeRef s4_2_4_DD2 :+: S ", LC1].")),
              (S (assumption ^. name) :+: S "5", Flat (S "The " :+: 
              (w_density ^. descr) :+: S " and " :+: (pcm_density ^. descr) :+:
              S " have no spatial variation; that is, they are each " :+:
              S "constant over their entire " :+: (volume ^. descr) :+: 
              S " [GD2].")),
              (S (assumption ^. name) :+: S "6", Flat (S "The " :+: (htCap_W ^.
              descr) :+: S ", " :+: (htCap_S_P ^. descr) :+: S ", and " :+: 
              (htCap_L_P ^. descr) :+: S " have no spatial variation; that " :+:
              S "is, they are each constant over their entire " :+:
              (volume ^. descr) :+: S " [GD2].")),
              (S (assumption ^. name) :+: S "7", Flat ((law_cooling ^.
              descr) :+: S " applies between the " :+: (sMap (map toLower) (S
              (coil ^. name))) :+: S " and the " :+: (sMap (map toLower) (S
              (water ^. name))) :+: S " [" :+: makeRef s4_2_4_DD1 :+: S "].")),
              (S (assumption ^. name) :+: S "8", Flat (S "The " :+: (temp_C ^. 
              descr) :+: S " is constant over " :+: (time ^. descr) :+:
              S " [" :+: makeRef s4_2_4_DD1 :+: S ", LC2].")),
              (S (assumption ^. name) :+: S "9", Flat (S "The " :+: (temp_C ^.
              descr) :+: S " does not vary along its length [" :+:
              makeRef s4_2_4_DD1 :+: S ", LC3].")),
              (S (assumption ^. name) :+: S "10", Flat ((law_cooling ^. 
              descr) :+: S " applies between the " :+: (sMap (map toLower) (S
              (water ^. name))) :+: S " and the " :+: S (phsChgMtrl ^. name) :+:
              S " [" :+: makeRef s4_2_4_DD2 :+: S "].")),
              (S (assumption ^. name) :+: S "11", Flat (S "The model only " :+:
              S "accounts for " :+: (sMap (map toLower) (charging ^. descr)) :+:
              S ", not " :+: (sMap (map toLower) (S (discharging ^. name))) :+:
              S ". The " :+: (temp_W ^. descr) :+: S " and " :+: 
              (temp_PCM ^. descr) :+: S " can only increase, or remain " :+:
              S "constant; they do not decrease. This implies that the " :+:
              (temp_init ^. descr) :+: S " (A12) is less than (or equal)" :+:
              S " to the " :+: (temp_C ^. descr) :+: S " [IM1, LC4].")),
              (S (assumption ^. name) :+: S "12", Flat (S "The " :+:
              (temp_init ^. descr) :+: S " of the " :+: (sMap (map toLower) (S 
              (water ^. name))) :+: S " and the " :+: S (phsChgMtrl ^. name) :+:
              S " is the same" :+: S " [IM1, IM2, LC5].")),
              (S (assumption ^. name) :+: S "13", Flat (S "The simulation " :+:
              S "will start with the " :+: S (phsChgMtrl ^. name) :+:
              S " in a " :+: (sMap (map toLower) (solid ^. descr)) :+:
              S " [IM2, IM4].")),
              (S (assumption ^. name) :+: S "14", Flat (S "The operating " :+:
              (temp ^. descr) :+: S " range of the system is " :+: S "such " :+:
              S "that the " :+: (sMap (map toLower) (S (water ^. name))) :+:
              S " is always in " :+: (liquid ^. descr) :+: S ". That is, " :+:
              S "the " :+: (temp ^. descr) :+: S " will not drop below the " :+:
              (temp_melt ^. descr) :+: S " of " :+: (sMap (map toLower) 
              (S (water ^. name))) :+: S ", or rise above its " :+: 
              (temp_boil ^. descr) :+: S " [IM1, IM3].")),
              (S (assumption ^. name) :+: S "15", Flat (S "The " :+:
              (sMap (map toLower) (S (tank ^. name))) :+: S " is " :+: 
              S (perfect_insul ^. name) :+: S " so that there is no heat " :+:
              S "loss from the " :+: (sMap (map toLower) (S (tank ^. name))) :+:
              S " [IM1, LC6].")),
              (S (assumption ^. name) :+: S "16", Flat (S "No internal heat" :+:
              S " is generated by either the " :+: (sMap (map toLower) 
              (S (water ^. name))) :+: S " or the " :+: S (phsChgMtrl ^.
              name) :+: S "; therefore, the " :+: (vol_ht_gen ^. descr) :+:
              S " is zero [IM1, IM2].")),
              (S (assumption ^. name) :+: S "17", Flat (S "The volume " :+: 
              S "change of the " :+: S (phsChgMtrl ^. name) :+: S " due to " :+: 
              (sMap (map toLower) (S (melting ^. name))) :+: 
              S " is negligible [IM2].")),
              (S (assumption ^. name) :+: S "18", Flat (S "The " :+: 
              S (phsChgMtrl ^. name) :+: S " is either in a " :+:
              (liquid ^. descr) :+: S " or a " :+: (solid ^. descr) :+:
              S " but not a " :+: (gaseous ^. descr) :+: S " [IM2, IM4]."))])
              

-- Can booktabs colored links be used? The box links completely cover nearby 
-- punctuation.

s4_2_2 = Section 2 (thModel ^. descr :+: S "s") [Con s4_2_2_intro, 
         Con s4_2_2_T1, Con s4_2_2_T2, Con s4_2_2_T3]

s4_2_2_intro = Paragraph (S "This section focuses on the general equations" :+:
               S " and laws that " :+: S (progName ^. name) :+: 
               S " is based on.")

-- General paragraph

-- Theory has to be RelationChunk....
-- No way to include "Source" or "Ref. By" sections?

-- No subsubsubsections... may make things difficult for derivation sections
-- coming up

s4_2_3 = Section 2 (genDefn ^. descr :+: S "s") 
         ((Con s4_2_3_intro):(map Con s4_2_3_deriv))

s4_2_3_intro = Paragraph (S "This section collects the laws and equations " :+:
               S "that will be used in deriving the " :+: (sMap (map toLower) 
               (dataDefn ^. descr)) :+: S "s, which in turn are used to " :+:
               S "build the " :+: (sMap (map toLower) (inModel ^. descr)) :+: 
               S "s.")

-- General paragraph
 
-- s4_2_3_GDs :: [LayoutObj]
-- s4_2_3_GDs = map Definition (map General [gd1NewtonCooling])

--General definitions not yet implemented

s4_2_3_deriv = [Paragraph (S "Detailed derivation of simplified rate of " :+:
               S "change of " :+: (temp ^. descr) :+: S ":"),
               Paragraph (S "Integrating " :+: makeRef s4_2_2_T1 :+: 
               S " over a " :+: (volume ^. descr) :+: S " (" :+:
               P (volume ^. symbol) :+: S "), we have:"),
               EqnBlock (Neg (UnaryOp (Integral (Just (C volume), Nothing))
               ((C gradient) :. (C thFluxVect))) + UnaryOp (Integral (Just 
               (C volume), Nothing)) (C vol_ht_gen) := UnaryOp (Integral (Just
               (C volume), Nothing)) ((C density) * (C htCap) * Deriv (C temp) 
               (C time))),
               Paragraph (S "Applying " :+: (gauss_div ^. descr) :+: S " to" :+:
               S " the first term over the " :+: (surface ^. descr) :+:
               S " " :+: P (surface ^. symbol) :+: S " of the " :+: 
               (volume ^. descr) :+: S ", with " :+: P (thFluxVect ^. 
               symbol) :+: S " as the " :+: (thFluxVect ^. descr) :+:
               S " for the " :+: (surface ^. descr) :+: S " and " :+:
               P (norm_vect ^. symbol) :+: S " as a " :+: (norm_vect ^.
               descr) :+: S ":"),
               EqnBlock (Neg (UnaryOp (Integral (Just (C surface), Nothing)) 
               ((C thFluxVect) :. (C norm_vect))) + UnaryOp (Integral (Just 
               (C volume), Nothing)) (C vol_ht_gen) := UnaryOp (Integral (Just 
               (C volume), Nothing)) ((C density) * (C htCap) * Deriv (C temp) 
               (C time))),
               Paragraph (S "We consider an arbitrary " :+: (volume ^. 
               descr) :+: S ". The " :+: (vol_ht_gen ^. descr) :+: S "is " :+:
               S "assumed constant. Then (1) can be written as:"),
               EqnBlock ((C ht_flux_in) * (C in_SA) - (C ht_flux_out) * 
               (C out_SA) + (C vol_ht_gen) * (C volume) := UnaryOp (Integral 
               (Just (C volume), Nothing)) ((C density) * (C htCap) * Deriv
               (C temp) (C time))),
               Paragraph (S "Where " :+: P (ht_flux_in ^. symbol) :+: S ", " :+:
               P (ht_flux_out ^. symbol) :+: S ", " :+: P (in_SA ^. symbol) :+:
               S ", and " :+: P (out_SA ^. symbol) :+: S " are explained in" :+:
               S " GD2. Assuming " :+: P (density ^. symbol) :+: S ", " :+:
               P (htCap ^. symbol) :+: S " and " :+: P (temp ^. symbol) :+:
               S " are constant over the " :+: (volume ^. descr) :+: S ", " :+: 
               S "which is true in our case by " :+: (assumption ^. descr) :+:
               S "s (A3), (A4), (A5), and (A6), we have:"),
               EqnBlock ((C density) * (C htCap) * (C volume) * Deriv (C temp) 
               (C time) := (C ht_flux_in) * (C in_SA) - (C ht_flux_out) * 
               (C out_SA) + (C vol_ht_gen) * (C volume)),
               Paragraph (S "Using the fact that " :+: P (density ^. symbol) :+:
               S "=" :+: P (mass ^. symbol) :+: S "/" :+: 
               P (volume ^. symbol) :+: S ", (2) can be written as:"),
               EqnBlock ((C mass) * (C htCap) * Deriv (C temp) (C time) :=
               (C ht_flux_in) * (C in_SA) - (C ht_flux_out) * (C out_SA) + 
               (C vol_ht_gen) * (C volume))]

-- Created a unitalChunk for "S"... should I add it to table of symbols?
-- Add references to above when available (assumptions, GDs)
-- Replace relevant Derivs with the regular derivative when it is available

s4_2_4 = Section 2 (dataDefn ^. descr :+: S "s") [Con s4_2_4_intro, 
         Con s4_2_4_DD1, Con s4_2_4_DD2, Con s4_2_4_DD3, Con s4_2_4_DD4]

s4_2_4_intro = Paragraph (S "This section collects and defines all the " :+:
               S "data needed to build the " :+: (sMap (map toLower) (inModel ^.
               descr)) :+: S "s. The dimension of each quantity is also given.")

-- General paragraph

s4_2_5 = Section 2 (inModel ^. descr :+: S "s") ((map Con s4_2_5_intro) ++ 
         (map Con s4_2_5_deriv1) ++ (map Con s4_2_5_deriv2))

s4_2_5_intro = [Paragraph (S "This section transforms the problem defined" :+:
               S " in " :+: (makeRef s4_1) :+: S " into one which" :+:
               S " is expressed in mathematical terms. It uses concrete " :+:
               S "symbols defined in " :+: (makeRef s4_2_4) :+:
               S " to replace the abstract symbols in the models identified" :+:
               S " in " :+: (makeRef s4_2_2) :+: S " and " :+:
               (makeRef s4_2_3) :+: S "."), 
               Paragraph (S "The goals GS1 to GS4 are solved by IM1 to IM4." :+:
               S " The solutions for IM1 and IM2 are coupled since the " :+:
               S "solution for " :+: P (temp_W ^. symbol) :+: S " and " :+:
               P (temp_PCM ^. symbol) :+: S " depend on one another. IM3 " :+:
               S "can be solved once IM1 has been solved. The solution of " :+:
               S "IM2 and IM4 are also coupled, since the " :+: 
               (temp_PCM ^. descr) :+: S " and " :+: (pcm_E ^. descr) :+:
               S " depend on the " :+: (sMap (map toLower) (S (phs_change ^. 
               name))) :+: S ".")]

-- Instance Models aren't implemented yet
-- Some specific info here on the order in which IMs are solved... maybe can be 
-- captured.

s4_2_5_deriv1 = [Paragraph (S "Derivation of the energy balance on " :+: 
                (sMap (map toLower) (S (water ^. name))) :+: S ":"),
                Paragraph (S "To find the rate of change of " :+: P (temp_W ^.
                symbol) :+: S ", we look at the energy balance on " :+:
                (sMap (map toLower) (S (water ^. name))) :+: S ". The " :+: 
                (volume ^. descr) :+: S " being considered is the " :+:
                (w_vol ^. descr) :+: S " " :+: P (w_vol ^. symbol) :+:
                S ", which has " :+: (w_mass ^. descr) :+: S " " :+: 
                P (w_mass ^. symbol) :+: S " and " :+: (htCap_W ^. descr) :+: 
                S ", " :+: P (htCap_W ^. symbol) :+: S ". " :+: P (ht_flux_C ^. 
                symbol) :+: S " represents the " :+: (ht_flux_C ^. descr) :+:
                S " and " :+: P (ht_flux_P ^. symbol) :+: S " represents " :+:
                S "the " :+: (ht_flux_P ^. descr) :+: S ", over " :+:
                (coil_SA ^. descr) :+: S " and " :+: (pcm_SA ^. descr) :+:
                S " of " :+: P (coil_SA ^. symbol) :+: S " and " :+: 
                P (pcm_SA ^. symbol) :+: S ", respectively. No " :+: 
                (heat_trans ^. descr) :+: S " occurs to the outside of the " :+:
                (sMap (map toLower) (S (tank ^. name))) :+: S ", since it " :+:
                S "has been assumed to be " :+: S (perfect_insul ^. name) :+: 
                S " (A15)." :+: S " Assuming no " :+: (vol_ht_gen ^. descr) :+: 
                S " (A16), " :+: P (vol_ht_gen ^. symbol) :+: S "=0. " :+:
                S "Therefore, the equation for GD2 can be written as:"),
                EqnBlock ((C w_mass) * (C htCap_W) * Deriv (C temp_W) (C time) 
                := (C ht_flux_C) * (C coil_SA) - (C ht_flux_P) * (C pcm_SA)),
                Paragraph(S "Using " :+: makeRef s4_2_4_DD1 :+: S " and " :+:
                makeRef s4_2_4_DD2 :+: S " for " :+: P (ht_flux_C ^. symbol) :+:
                S " and " :+: P (ht_flux_P ^. symbol) :+: S " respectively," :+:
                S " this can be written as:"),
                EqnBlock ((C w_mass) * (C htCap_W) * Deriv (C temp_W) (C time) 
                := (C coil_HTC) * (C coil_SA) * ((C temp_C) - (C temp_W)) -
                (C pcm_HTC) * (C pcm_SA) * ((C temp_W) - (C temp_PCM))),
                Paragraph (S "Dividing (3) by " :+: P (w_mass ^. symbol) :+:
                P (htCap_W ^. symbol) :+: S ", we obtain:"),
                EqnBlock (Deriv (C temp_W) (C time) := ((C coil_HTC) * 
                (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) - 
                (C temp_W)) - ((C pcm_mass) * (C pcm_SA)) / ((C w_mass) *
                (C htCap_W)) * ((C temp_W) - (C temp_PCM))),
                Paragraph (S "Factoring the negative sign out of the second" :+:
                S " term of the " :+: S (rightSide ^. name) :+: S " of " :+:
                S "Equation (4) and multiplying it by " :+: 
                P (coil_HTC ^. symbol) :+: P (coil_SA ^. symbol) :+: S "/" :+: 
                P (coil_HTC ^. symbol) :+: P (coil_SA ^. symbol) :+: 
                S " yields:"),
                EqnBlock (Deriv (C temp_W) (C time) := ((C coil_HTC) * 
                (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) - 
                (C temp_W)) + ((C coil_HTC) * (C coil_SA)) / ((C coil_HTC) * 
                (C coil_SA)) * ((C pcm_HTC) * (C pcm_SA)) / ((C w_mass) * 
                (C htCap_W)) * ((C temp_PCM) - (C temp_W))),
                Paragraph (S "Which simplifies to:"),
                EqnBlock (Deriv (C temp_W) (C time) := ((C coil_HTC) * 
                (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) -
                (C temp_W)) + ((C pcm_HTC) * (C pcm_SA)) / ((C coil_HTC) *
                (C coil_SA)) * ((C coil_HTC) * (C coil_SA)) / ((C w_mass) * 
                (C htCap_W)) * ((C temp_PCM) - (C temp_W))),
                Paragraph (S "Setting " :+: P (tau_W ^. symbol) :+: S "=" :+:
                P (w_mass ^. symbol) :+: P (htCap_W ^. symbol) :+: S "/" :+:
                P (coil_HTC ^. symbol) :+: P (coil_SA ^. symbol) :+: 
                S " and " :+: P (eta ^. symbol) :+: S "=" :+: P (pcm_HTC ^. 
                symbol) :+: P (pcm_SA ^. symbol) :+: S "/" :+: P (coil_HTC ^.
                symbol) :+: P (coil_SA ^. symbol) :+: S ", Equation (5) can" :+:
                S " be written as:"),
                EqnBlock (Deriv (C temp_W) (C time) := (1 / (C tau_W)) *
                ((C temp_C) - (C temp_W)) + ((C eta) / (C tau_W)) *
                ((C temp_PCM) - (C temp_W))),
                Paragraph (S "Finally, factoring out 1/" :+: P (tau_W ^. 
                symbol) :+: S ", we are left with the governing " :+:
                S (ordDiffEq ^. name) :+: S " for IM1:"),
                EqnBlock (Deriv (C temp_W) (C time) := (1 / (C tau_W)) *
                (((C temp_C) - (C temp_W)) + (C eta) * ((C temp_PCM) - 
                (C temp_W))))
                ]

-- Should "energy balance" be a concept?
-- Add IM, GD, A, and EqnBlock references when available
-- Replace Derivs with regular derivative when available
-- Fractions in paragraph?

s4_2_5_deriv2 = [Paragraph (S "Detailed derivation of the energy balance on" :+:
                S " the " :+: S (phsChgMtrl ^. name) :+: S " during " :+: 
                (sMap (map toLower) (sens_heat ^. descr)) :+: S " phase:"),
                Paragraph (S "To find the rate of change of " :+: P (temp_PCM ^.
                symbol) :+: S ", we look at the energy balance on the " :+: 
                S (phsChgMtrl ^. name) :+: S ". The " :+: (volume ^. descr) :+:
                S " being considered is the " :+: (pcm_vol ^. descr) :+: 
                S ", " :+: P (pcm_vol ^. symbol) :+: S ". The derivation " :+:
                S "that follows is initially for the " :+: (sMap (map toLower) 
                (S (solid ^. name))) :+: S " " :+: S (phsChgMtrl ^. name) :+:
                S ". The " :+: (pcm_mass ^. descr) :+: S " is " :+: 
                P (pcm_mass ^. symbol) :+: S " and the " :+: (htCap_S_P ^. 
                descr) :+: S " is " :+: P (htCap_S_P ^. symbol) :+: S ". " :+:
                S "The " :+: (ht_flux_P ^. descr) :+: S " is " :+: 
                P (ht_flux_P ^. symbol) :+: S " over " :+: (pcm_SA ^. descr) :+:
                S " " :+: P (pcm_SA ^. symbol) :+: S ". There is no " :+: 
                (ht_flux_out ^. descr) :+: S ". Assuming no " :+: (vol_ht_gen ^.
                descr) :+: S " (A16), " :+: P (vol_ht_gen ^. symbol) :+: 
                S "=0, the equation for GD2 can be written as:"),
                EqnBlock ((C pcm_mass) * (C htCap_S_P) * Deriv (C temp_PCM) 
                (C time) := (C ht_flux_P) * (C pcm_SA)),
                Paragraph (S "Using " :+: makeRef s4_2_4_DD2 :+: S " for " :+:
                P (ht_flux_P ^. symbol) :+: S ", this equation can be " :+:
                S "written as:"),
                EqnBlock ((C pcm_mass) * (C htCap_S_P) * Deriv (C temp_PCM) 
                (C time) := (C pcm_HTC) * (C pcm_SA) * ((C temp_W) - 
                (C temp_PCM))),
                Paragraph (S "Dividing by " :+: P (pcm_mass ^. symbol) :+:
                P (htCap_S_P ^. symbol) :+: S " we obtain:"),
                EqnBlock (Deriv (C temp_PCM) (C time) := ((C pcm_HTC) * 
                (C pcm_SA)) / ((C pcm_mass) * (C htCap_S_P)) * ((C temp_W) - 
                (C temp_PCM))),
                Paragraph (S "Setting " :+: P (tau_S_P ^. symbol) :+: S "=" :+:
                P (pcm_mass ^. symbol) :+: P (htCap_S_P ^. symbol) :+: S "/" :+:
                P (pcm_HTC ^. symbol) :+: P (pcm_SA ^. symbol) :+: S ", " :+:
                S "this can be written as:"),
                EqnBlock (Deriv (C temp_PCM) (C time) := (1 / (C tau_S_P)) *
                ((C temp_W) - (C temp_PCM))),
                Paragraph (S "Equation (6) applies for the " :+: (sMap (map 
                toLower) (S (solid ^. name))) :+: S " " :+: S (phsChgMtrl ^. 
                name) :+: S ". In the case where all of the " :+:
                S (phsChgMtrl ^. name) :+: S " is melted, the same " :+:
                S "derivation applies, except that " :+: P (htCap_S_P ^. 
                symbol) :+: S " is replaced by " :+: P (htCap_L_P ^. symbol) :+:
                S ", and thus " :+: P (tau_S_P ^. symbol) :+: S " is " :+: 
                S "replaced by " :+: P (tau_L_P ^. symbol) :+: S ". " :+:
                S "Although a small change in surface area would be " :+:
                S "expected with " :+: (sMap (map toLower) (S (melting ^. 
                name))) :+: S ", this is not included, since the " :+:
                (volume ^. descr) :+: S " change of the " :+: S (phsChgMtrl ^. 
                name) :+: S " with " :+: (sMap (map toLower) (S (melting ^. 
                name))) :+: S " is assumed to be negligible (A17)."),
                Paragraph (S "In the case where " :+: P (temp_PCM ^. symbol) :+:
                S "=" :+: P (temp_melt_P ^. symbol) :+: S " and not all of " :+:
                S "the " :+: S (phsChgMtrl ^. name) :+: S " is melted, the " :+:
                (temp_PCM ^. descr) :+: S " does not change. Therefore, in " :+:
                S "this case d" :+: P (temp_PCM ^. symbol) :+: S "/d" :+:
                P (time ^. symbol) :+: S "=0."),
                Paragraph (S "This derivation does not consider the " :+: 
                (sMap (map toLower) (S (boiling ^. name))) :+: S " of the " :+: 
                S (phsChgMtrl ^. name) :+: S ", as the " :+: S (phsChgMtrl ^. 
                name) :+: S " is assumed to either be in" :+: S " a " :+:
                (solid ^. descr) :+: S " or a " :+: (liquid ^. descr) :+: 
                S " (A18).")]

-- Add GD, A, and EqnBlock references when available
-- Replace Derivs with regular derivative when available
-- Derivative notation in paragraph?

s4_2_6 = Section 2 (S "Data Constraints") [Con s4_2_6_intro, Con table1]

s4_2_6_intro = Paragraph ((makeRef table1) :+: S " show the data " :+:
               S "constraints on the input and output variables, " :+:
               S "respectively. The column for physical constraints gives " :+: 
               S "the physical limitations on the range of values that can " :+:
               S "be taken by the variable. The column for software " :+:
               S "constraints restricts the range of inputs to reasonable " :+:
               S "values. The constraints are conservative, to give the " :+:
               S "user of the model the flexibility to experiment with " :+:
               S "unusual situations. The column of typical values is " :+:
               S "intended to provide a feel for a common scenario. The " :+:
               S "uncertainty column provides an estimate of the confidence" :+:
               S " with which the physical quantities can be measured. This" :+:
               S " information would be part of the input if one were " :+:
               S "performing an uncertainty quantification exercise.")

-- Completely general paragraph.
-- I do not think Table 2 will end up being necessary for the Drasil version
---- The info from table 2 will likely end up in table 1.

inputVar :: [UnitalChunk]
inputVar = [tank_length, diam, pcm_vol, pcm_SA, pcm_density, temp_melt_P,
  htCap_S_P, htCap_L_P, htFusion, coil_SA, temp_C, w_density, htCap_W, 
  coil_HTC, pcm_HTC, temp_init, time_final]
  
-- Typical values and constraints must be added to UC definitions for mkTable 
-- to work here.

table1 = Table [S "Var", S "Physical Constraints", S "Software Constraints",
         S "Typical Value", S "Uncertainty"] (mkTable
         [\ch -> P (ch ^. symbol),
         \ch -> Sy (ch ^. unit),
         \ch -> Sy (ch ^. unit),
         \ch -> Sy (ch ^. unit),
         \ch -> Sy (ch ^. unit)] inputVar)
         (S "Input Variables") True

-- Add constraints (and typical values) to the knowledge capture of each 
-- variable, so that lambdas can be used to extract constraints?
-- Add "Uncertainty" to UnitalChunks??
-- Other Notes:
---- Will there be a way to have asterisks for certain pieces of the table?

--Tables 2 and 3 will be delayed for now bc they are similar to table 1

s4_2_7 = Section 2 (S "Properties of a Correct Solution") (map Con s4_2_7_deriv)

s4_2_7_deriv = [Paragraph (S "A correct solution must exhibit the " :+: 
               (sMap (map toLower) (S (law_cons_energy ^. name))) :+:
               S ". This means that the " :+: (w_E ^. descr) :+:
               S " should equal the difference between " :+:
               S " the total energy input from the " :+: (sMap (map toLower) 
               (S (coil ^. name))) :+: S " and the energy output to the " :+:
               S (phsChgMtrl ^. name) :+: S ". This can be shown as an " :+:
               S "equation by taking " :+: makeRef s4_2_4_DD1 :+: S " and " :+:
               makeRef s4_2_4_DD2 :+: S ", multiplying each by their " :+:
               S "respective surface area of " :+: (heat_transfer ^. descr) :+:
               S ", and integrating each over the simulation " :+: (time ^. 
               descr) :+: S ", as follows:"),
               EqnBlock ((C w_E) := UnaryOp (Integral (Just 0, Just (C time))) 
               ((C coil_HTC) * (C coil_SA) * ((C temp_C) - FCall (C temp_W) 
               [C time])) - UnaryOp (Integral (Just 0, Just (C time))) 
               ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) -
               (FCall (C temp_PCM) [C time])))),
               Paragraph (S "In addition, the " :+: (pcm_E ^. descr) :+: 
               S " should equal the energy input to the " :+: S (phsChgMtrl ^. 
               name) :+: S " from the " :+: (sMap (map toLower) (S (water ^.
               name))) :+: S ". This can be expresse as"),
               EqnBlock ((C pcm_E) := UnaryOp (Integral (Just 0, Just (C time)))
               ((C pcm_HTC) * (C pcm_SA) * ((FCall (C temp_W) [C time]) - (FCall
               (C temp_PCM) [C time])))),
               Paragraph (S "Equations (reference) and (reference) can be " :+:
               S "used as " :+: Quote (S "sanity") :+: S "checks to gain " :+: 
               S "confidence in any solution computed by " :+: S (progName ^. 
               name) :+: S ". The relative error between the results " :+:
               S "computed by " :+: S (progName ^. name) :+: S " and the " :+:
               S "results calculated from the " :+: S (rightSide ^. name) :+: 
               S " of these equations should be less than 0.001% (R9).")]

-- Remember to insert references in above derivation when available
-- 


s5 = Section 0 ((requirement ^. descr) :+: S "s") [Con s5_intro, Sub s5_1, 
     Sub s5_2]

s5_intro = Paragraph (S "This section provides the functional " :+: (sMap (map 
           toLower) (requirement ^. descr)) :+: S "s, the business tasks " :+:
           S "that the software is expected to complete, and the " :+:
           S "nonfunctional " :+: (sMap (map toLower) (requirement ^. 
           descr)) :+:S "s, the qualities that the software is expected to " :+:
           S "exhibit.")

-- Completely general paragraph.

s5_1 = Section 1 (S "Functional " :+: (requirement ^. descr) :+: S "s") 
       (map Con s5_1_list)

s5_1_list = [Enumeration (Simple [(S (requirement ^. name) :+: S "1", Flat 
            (S "Input the following quantities, which define the " :+:
            (sMap (map toLower) (S (tank ^. name))) :+: 
            S " parameters, material properties and initial conditions:"))]), 
            (Table [S "symbol", S "unit", S "description"] (mkTable
            [(\ch -> P (ch ^. symbol)),
            (\ch -> Sy (ch ^. unit)),
            (\ch -> ch ^. descr)
            ] inputVar) (S "Input Variable " :+: (requirement ^. descr)) False),
            Enumeration (Simple [(S (requirement ^. name) :+: S "2", Flat 
            (S "Use the inputs in R1 to find the " :+: (mass ^. descr) :+:
            S " needed for IM1 to IM4, as follows, where " :+:
            P (w_vol ^. symbol) :+: S " is the " :+:(w_vol ^. descr) :+:
            S " and " :+: P (tank_vol ^. symbol) :+: S " is the " :+:
            (tank_vol ^. descr) :+: S "."))]),
            EqnBlock ((C w_mass) := (C w_vol) * (C w_density) := ((C tank_vol) -
            (C pcm_vol)) * (C w_density) := (((C diam) / 2) * (C tank_length) - 
            (C pcm_vol)) * (C w_density)),
            EqnBlock ((C pcm_mass) := (C pcm_vol) * (C pcm_density)),
            Enumeration (Simple [(S (requirement ^. name) :+: S "3", Flat 
            (S "Verify that the inputs satisfy the required physical" :+:
            S " constraints shown in " :+: makeRef table1 :+: S ".")),
            (S (requirement ^. name) :+: S "4", Flat (S "Output the input" :+: 
            S " quantities and derived quantities in the following list: "  :+:
            S "the quantities from R1, the " :+: (mass ^. descr) :+: S "es " :+:
            S "from R2, " :+: P (tau_W ^. symbol) :+: S " (from IM1), " :+: 
            P (eta ^. symbol) :+: S " (from IM1), " :+: P (tau_S_P ^. 
            symbol) :+: S " (from IM2) and " :+: P (tau_L_P ^. symbol) :+:
            S " (from IM2).")),
            (S (requirement ^. name) :+: S "5", Flat (S "Calculate and " :+:
            S "output the " :+: (temp_W ^. descr) :+: S " (" :+: P (temp_W ^.
            symbol) :+: S "(" :+: P (time ^. symbol) :+: S ")) " :+:
            S "over the simulation " :+: (time ^. descr) :+: S " (from IM1).")),
            (S (requirement ^. name) :+: S "6", Flat (S "Calculate and " :+: 
            S "output the " :+: (temp_PCM ^. descr) :+: S " (" :+:
            P (temp_PCM ^. symbol) :+: S "(" :+: P (time ^. symbol) :+:
            S ")) over the simulation " :+: (time ^. descr) :+: 
            S " (from IM2).")),
            (S (requirement ^. name) :+: S "7", Flat (S "Calculate and " :+: 
            S " output the " :+: (w_E ^. descr) :+: S " (" :+: P (w_E ^. 
            symbol) :+: S "(" :+: P (time ^. symbol) :+: S ")) " :+:
            S "over the simulation " :+: (time ^. descr) :+: S " (from IM3).")),
            (S (requirement ^. name) :+: S "8", Flat (S "Calculate and " :+: 
            S "output the " :+: (pcm_E ^. descr) :+: S " (" :+: P (pcm_E ^.
            symbol) :+: S "(" :+: P (time ^. symbol) :+: S ")) over the " :+:
            S "simulation " :+: (time ^. descr) :+: S " (from IM4).")),
            (S (requirement ^. name) :+: S "9", Flat (S "Verify that the " :+:
            S "energy outputs (" :+: P (w_E ^. symbol) :+: S "(" :+: P (time ^. 
            symbol) :+: S ") and " :+: P (pcm_E ^. symbol) :+: S "(" :+:
            P (time ^. symbol) :+: S ")) follow the " :+: (sMap (map toLower) 
            (S (law_cons_energy ^. name))) :+: S ", as outlined in " :+: 
            makeRef s4_2_7 :+: S ", with relative error no greater than " :+:
            S "0.001%.")),
            (S (requirement ^. name) :+: S "10", Flat (S "Calculate and " :+: 
            S "output the " :+: (time ^. descr) :+: S " at which the " :+: 
            S (phsChgMtrl ^. name) :+: S " begins to melt " :+:
            P (t_init_melt ^. symbol) :+: S " (from IM2).")),
            (S (requirement ^. name) :+: S "11", Flat (S "Calculate and " :+: 
            S "output the " :+: (time ^. descr) :+: S " at which the " :+: 
            S (phsChgMtrl ^. name) :+:
            S " stops " :+: (sMap (map toLower) (S (melting ^. name))) :+: 
            S " " :+: P (t_final_melt ^. symbol) :+: S " (from IM2)."))])
            ]

--How to include pi?
--How to add exponents?

s5_2 = Section 1 (S "Nonfunctional " :+: (requirement ^. descr) :+: S "s") 
       [Con s5_2_contents]

s5_2_contents = Paragraph (S "Given the small size, and relative simplicity" :+:
                S ", of this problem, performance is not a priority. Any " :+:
                S "reasonable implementation will be very quick and use " :+:
                S "minimal storage. Rather than performance, the priority " :+:
                S "nonfunctional " :+: (sMap (map toLower) (requirement ^. 
                descr)) :+: S "s are correctness, verifiability" :+:
                S ", understandability, reusability, and maintainability.")

-- Specific info here... potentially can be captured though. Might require 
-- dropping some of the explanantion about small size, etc.

s6 = Section 0 ((likelyChg ^. descr) :+: S "s") [Con s6_list]

s6_list = Enumeration (Simple [(S (likelyChg ^. name) :+: S "1", Flat 
          (S "A4 - " :+: S (phsChgMtrl ^. name) :+: S " is actually a poor " :+:
          (sMap (map toLower) (S (thermal_conductor ^. name))) :+: S ", so " :+:
          S "the " :+: (sMap (map toLower) (assumption ^. descr)) :+:
          S " of uniform " :+: (temp_PCM ^. descr) :+: S " is not likely.")),
          (S (likelyChg ^. name) :+: S "2", Flat (S "A8 - The " :+: (temp_C ^. 
          descr) :+: S " will change over the course of the day, depending" :+:
          S " on the energy received from the sun.")),
          (S (likelyChg ^. name) :+: S "3", Flat (S "A9 - The " :+: (temp_C ^. 
          descr) :+: S " will actually change along its length as the " :+:
          (sMap (map toLower) (S (water ^. name))) :+: S " within it cools.")),
          (S (likelyChg ^. name) :+: S "4", Flat (S "A11 - The model " :+:
          S "currently only accounts for " :+: (sMap (map toLower) (charging ^. 
          descr)) :+: S ". A more complete model would also account for " :+:
          (sMap (map toLower) (discharging ^. descr)) :+: S ".")),
          (S (likelyChg ^. name) :+: S "5", Flat (S "A12 - To add more " :+:
          S " flexibility to the simulation, the " :+: (temp_init ^. descr) :+:
          S " of the " :+: (sMap (map toLower) (S (water ^. name))) :+: 
          S " and the " :+: S (phsChgMtrl ^. name) :+: S " could be " :+:
          S "allowed to have different values.")),
          (S (likelyChg ^. name) :+: S "6", Flat (S "A15 - Any real " :+:
          (sMap (map toLower) (S (tank ^. name))) :+: S " cannot be " :+:
          S (perfect_insul ^. name) :+: S " and will lose heat."))])

--add referencing to assumptions?
  
s7 = Section 0 (S "Traceability Matrices and Graphs") ([Con s7_intro1, 
     Con s7_table1, Con s7_table2, Con s7_table3] ++ (map Con s7_intro2) ++ 
     [Con s7_fig1, Con s7_fig2])

s7_intro1 = Paragraph (S "The purpose of the traceability matrices is to " :+: 
            S "provide easy references on what has to be additionally " :+:
            S "modified if a certain component is changed. Every time a " :+:
            S "component is changed, the items in the column of that " :+:
            S "component that are marked with an " :+: Quote (S "X") :+: 
            S " should be modified as well. " :+: makeRef s7_table1 :+:
            S " shows the dependencies of " :+: (sMap (map toLower) (thModel ^. 
            descr)) :+: S "s, " :+: (sMap (map toLower) (genDefn ^. descr)) :+:
            S "s, " :+: (sMap (map toLower) (dataDefn ^. descr)) :+: S "s, " :+:
            S "and " :+: (sMap (map toLower) (inModel ^. descr)) :+: S "s " :+:
            S "with each other. " :+: makeRef s7_table2 :+: S " shows the " :+:
            S "dependencies of " :+: (sMap (map toLower) (inModel ^. descr)) :+:
            S "s, " :+: (sMap (map toLower) (requirement ^. descr)) :+:
            S "s, and data constraints on each other. " :+: 
            makeRef s7_table3 :+: S " shows the dependencies of " :+: 
            (sMap (map toLower) (thModel ^. descr)) :+: S "s, " :+:
            (sMap (map toLower) (genDefn ^. descr)) :+: S "s, " :+: 
            (sMap (map toLower) (dataDefn ^. descr)) :+: S "s, " :+:
            (sMap (map toLower) (inModel ^. descr)) :+: S "s, and " :+: 
            (sMap (map toLower) (likelyChg ^. descr)) :+: S "s on the " :+:
            (assumption ^. descr) :+: S "s.")

-- Completely general paragraph.

s7_table1 = Table [S "", makeRef s4_2_2_T1, makeRef s4_2_2_T2, 
            makeRef s4_2_2_T3, S "GD1", S "GD2", makeRef s4_2_4_DD1, 
            makeRef s4_2_4_DD2, makeRef s4_2_4_DD3, makeRef s4_2_4_DD4, S "IM1",
            S "IM2", S "IM3", S "IM4"]
            [[makeRef s4_2_2_T1, S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S "", S ""],
            [makeRef s4_2_2_T2, S "", S "", S "X", S "", S "", S "", S "", S "",
            S"", S "", S "", S "", S ""],
            [makeRef s4_2_2_T3, S "", S "", S "", S "", S "", S "", S "", S "", 
            S "", S "", S "", S "", S ""],
            [S "GD1", S "", S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S ""],
            [S "GD2", S "X", S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S ""],
            [makeRef s4_2_4_DD1, S "", S "", S "", S "X", S "", S "", S "",
            S "", S "", S "", S "", S "", S ""],
            [makeRef s4_2_4_DD2, S "", S "", S "", S "X", S "", S "", S "",
            S"", S "", S "", S "", S "", S ""],
            [makeRef s4_2_4_DD3, S "", S "", S "", S "", S "", S "", S "", S "",
            S"", S "", S "", S "", S ""],
            [makeRef s4_2_4_DD4, S "", S "", S "", S "", S "", S "", S "",
            S "X", S "", S "", S "", S "", S ""],
            [S "IM1", S "", S "", S "", S "", S "X", S "X", S "X", S "", S "",
            S "", S "X", S "", S ""],
            [S "IM2", S "", S "", S "", S "", S "X", S "", S "X", S "", S "X",
            S "X", S "", S "", S "X"],
            [S "IM3", S "", S "X", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S ""],
            [S "IM4", S "", S "X", S "X", S "", S "", S "", S "X", S "X", S "X",
            S "", S "X", S "", S ""]]
            (S "Traceability Matrix Showing the Connections Between Items " :+:
            S "of Different Sections") True

s7_table2 = Table [S "", S "IM1", S "IM2", S "IM3", S "IM4", makeRef s4_2_6,
            S "R1", S "R2"]
            [[S "IM1", S "", S "X", S "", S "", S "", S "X", S "X"],
            [S "IM2", S "X", S "", S "", S "X", S "", S "X", S "X"],
            [S "IM3", S "", S "", S "", S "", S "", S "X", S "X"],
            [S "IM4", S "", S "X", S "", S "", S "", S "X", S "X"],
            [S "R1", S "", S "", S "", S "", S "", S "", S ""],
            [S "R2", S "", S "", S "", S "", S "", S "X", S ""],
            [S "R3", S "", S "", S "", S "", S "X", S "", S ""],
            [S "R4", S "X", S "X", S "", S "", S "", S "X", S "X"],
            [S "R5", S "X", S "", S "", S "", S "", S "", S ""],
            [S "R6", S "", S "X", S "", S "", S "", S "", S ""],
            [S "R7", S "", S "", S "X", S "", S "", S "", S ""],
            [S "R8", S "", S "", S "", S "X", S "", S "", S ""],
            [S "R9", S "", S "", S "X", S "X", S "", S "", S ""],
            [S "R10", S "", S "X", S "", S "", S "", S "", S ""],
            [S "R11", S "", S "X", S "", S "", S "", S "", S ""]]
            (S "Traceability Matrix Showing the Connections Between " :+:
            (requirement ^. descr) :+: S "s and " :+: (inModel ^. descr) :+:
            S "s") True

s7_table3 = Table [S "", S "A1", S "A2", S "A3", S "A4", S "A5", S "A6", S "A7",
            S "A8", S "A9", S "A10", S "A11", S "A12", S "A13", S "A14",
            S "A15", S "A16", S "A17", S "A18"]
            [[makeRef s4_2_2_T1, S "X", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
            [makeRef s4_2_2_T2, S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
            [makeRef s4_2_2_T3, S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
            [S "GD1", S "", S "X", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S "", S "", S "", S "", S "", S ""],
            [S "GD2", S "", S "", S "X", S "X", S "X", S "X", S "", S "", S "",
            S "", S "", S "", S "", S "", S "", S "", S "", S ""],
            [makeRef s4_2_4_DD1, S "", S "", S "", S "", S "", S "", S "X",
            S "X", S "X", S "", S "", S "", S "", S "", S "", S "", S "" , 
            S ""],
            [makeRef s4_2_4_DD2, S "", S "", S "X", S "X", S "", S "", S "",
            S "", S "", S "X", S "", S "", S "", S "", S "", S "", S "", S ""],
            [makeRef s4_2_4_DD3, S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
            [makeRef s4_2_4_DD4, S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S "", S "", S "", S "", S "", S "", S ""],
            [S "IM1", S "", S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "X", S "X", S "", S "X", S "X", S "X", S "", S ""],
            [S "IM2", S "", S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "X", S "X", S "", S "", S "X", S "X", S "X"],
            [S "IM3", S "", S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S "", S "X", S "", S "", S "", S ""],
            [S "IM4", S "", S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "", S "X", S "", S "", S "", S "", S "X"],
            [S "LC1", S "", S "", S "", S "X", S "", S "", S "", S "", S "",
            S "", S "", S "", S "", S "", S "", S "", S "", S ""],
            [S "LC2", S "", S "", S "", S "", S "", S "", S "", S "X", S "",
            S "", S "", S "", S "", S "", S "", S "", S "", S ""],
            [S "LC3", S "", S "", S "", S "", S "", S "", S "", S "", S "X",
            S "", S "", S "", S "", S "", S "", S "", S "", S ""],
            [S "LC4", S "", S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "X", S "", S "", S "", S "", S "", S "", S ""],
            [S "LC5", S "", S "", S "", S "", S "", S "", S "", S "", S "",
            S "", S "", S "X", S "", S "", S "", S "", S "", S ""],
            [S "LC6", S "", S "", S "", S "", S "", S "", S "", S "", S "", 
            S "", S "", S "", S "", S "", S "X", S "", S "", S ""]]
            (S "Traceability Matrix Showing the Connections Between " :+:
            (assumption ^. descr) :+: S "s and Other Items") True

s7_intro2 = [Paragraph (S "The purpose of the traceability graphs is also " :+:
            S "to provide easy references on what has to be additionally " :+:
            S "modified if a certain component is changed. The arrows in " :+:
            S "the graphs represent dependencies. The component at the tail" :+:
            S " of an arrow is depended on by the component at the head of " :+:
            S "that arrow. Therefore, if a component is changed, the " :+:
            S "components that it points to should also be changed. " :+:
            makeRef s7_fig1 :+: S " shows the dependencies of " :+:
            (sMap (map toLower) (thModel ^. descr)) :+: S "s, " :+:
            (sMap (map toLower) (genDefn ^. descr)) :+: S "s, " :+:
            (sMap (map toLower) (dataDefn ^. descr)) :+: S "s, " :+:
            (sMap (map toLower) (inModel ^. descr)) :+: S "s, " :+:
            (sMap (map toLower) (likelyChg ^. descr)) :+: S "s, and " :+:
            (sMap (map toLower) (assumption ^. descr)) :+: S "s on each " :+:
            S "other. " :+: makeRef s7_fig2 :+: S " shows the dependencies " :+:
            S "of " :+: (sMap (map toLower) (inModel ^. descr)) :+: S "s, " :+:
            (sMap (map toLower) (requirement ^. descr)) :+: S "s, and data " :+:
            S "constraints on each other."),
            Paragraph (S "NOTE: Building a tool to automatically generate " :+:
            S "the graphical representation of the matrix by scanning the " :+:
            S "labels and reference can be future work.")]

s7_fig1 = Figure (S "Traceability Matrix Showing the Connections Between " :+:
          S "Items of Different Sections") "../../../ATrace.png"

s7_fig2 = Figure (S "Traceability Matrix Showing the Connections Between " :+:
          (requirement ^. descr) :+: S "s, " :+: (inModel ^. descr) :+: 
          S "s, and Data Constraints") "../../../RTrace.png"

--References?

