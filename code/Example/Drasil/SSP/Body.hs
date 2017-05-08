module Drasil.SSP.Body where

import Control.Lens ((^.))
import Prelude hiding (id)

import Language.Drasil
import Data.Drasil.SI_Units 
import Data.Drasil.Authors

import Drasil.SSP.Defs
import Drasil.SSP.Units
import Drasil.SSP.Modules
import Drasil.SSP.Changes
import Drasil.SSP.Reqs

import           Drasil.TableOfAbbAndAcronyms
import qualified Drasil.SRS as SRS
import           Drasil.ReferenceMaterial

import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Physics
import Data.Drasil.Concepts.PhysicalProperties

this_si :: [UnitDefn]
this_si = map UU [metre, degree] ++ map UU [newton, pascal]

ssp_srs :: Document  
ssp_srs = SRS.doc ssa (name henryFrankis) [s1, s2, s3, s4, s5, s6]

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

ssp_mg :: Document
ssp_mg = mgDoc ssa (name henryFrankis) mgBod

s1, s1_1, s1_2, s1_3, s2, s3, s4, s5, s6 :: Section


s1_1_intro, s1_1_table, s1_2_intro, s1_2_table,
  s2_p1, s2_p2, s2_1, s2_2, s2_3, s2_1_p1, s2_1_p2, s2_2_p1, s2_3_p1, s3_p1,
  s3_1, s3_2, s3_1_p1, s3_2_p1, s4_p1, s4_1, s4_2, s4_1_p1, s4_1_1, s4_1_2,
  s4_1_3, s4_1_1_list, s4_1_2_p1, s4_1_2_bullets, s4_1_2_p2, s4_1_2_fig1,
  s4_1_2_fig2, s4_1_3_p1, s4_1_3_list, s4_2_p1, s4_2_1, s4_2_2, s4_2_3, s4_2_4,
  s4_2_5, s4_2_6, s4_2_1_p1, s4_2_1_list, s4_2_2_p1, s5_p1, s5_1, s5_2,
  s5_1_list, s5_1_table, s5_2_p1 :: SecCons

s4_2_2_tmods :: [SecCons]

-- SECTION 1 --
s1 = refSec [s1_1, s1_2, s1_3]

-- SECTION 1.1 --
s1_1 = Section (titleize tOfUnits) [s1_1_intro, s1_1_table]

s1_1_intro = Con $ Paragraph (S "Units of the physical properties of the" +:+
  S "soil that are of interest when examining slope stability problems" +:+
  S "are given in the following table.")

s1_1_table = Con $ Table [S "Symbol", S "Description", S "Name"] (mkTable
  [(\x -> Sy (x ^. usymb)),
   (\x -> (x ^. defn)),
   (\x -> (phrase $ x ^. term))
  ] this_si)
  (titleize tOfUnits) True

-- SECTION 1.2 --
s1_2 = Section (titleize tOfSymb) [s1_2_intro, s1_2_table]

s1_2_intro = Con $ Paragraph $
  S "A collection of the symbols that will be used in the models" +:+
  S "and equations of the program are summarized in the table" +:+
  S "below. Values with a subscript i implies that the value will" +:+
  S "be taken at and analyzed at a slice or slice interface" +:+
  S "composing the total slip" +:+. (phrase $ mass ^. term)

s1_2_table = Con $ Table (map titleize [symbol_, units_, description]) (mkTable
  [(\ch -> P (ch ^. symbol)), -- (\ch -> N (ch ^. symbol)) , 
   (\ch -> Sy $ unit_symb ch),
   (\ch -> phrase $ ch ^. term)
   ]
  sspSymbols)
  (titleize tOfSymb) False

-- SECTION 1.3 --
s1_3 = table_of_abb_and_acronyms acronyms
  
-- SECTION 2 --
s2 = SRS.intro [s2_p1, s2_p2, s2_1, s2_2, s2_3]

s2_p1 = Con $ Paragraph $ S "A slope of geological mass, composed" +:+
  S "of soil and rock, is subject to the influence of gravity on" +:+
  S "the mass. For an unstable slope this can cause instability" +:+
  S "in the form of soil/rock movement. The effects of soil/rock" +:+
  S "movement can range from inconvenient to seriously hazardous," +:+
  S "resulting in signifcant life and economic loses. Slope stability" +:+
  S "is of interest both when analyzing natural slopes," +:+
  S "and when designing an excavated slope." +:+ (at_start ssa) +:+
  S "is the assessment of the safety of a slope, identifying the" +:+
  S "surface most likely to experience slip and an index of it's" +:+
  S "relative stability known as the factor of safety." --FIXME: use a definition for "factor of safety"

s2_p2 = Con $ Paragraph $ S "The following section provides an overview" +:+
  S "of the" +:+ (introduceAbb srs) +:+
  S "for a" +:+ (phrase ssa) +:+ S "problem. The developed program" +:+
  S "will be referred to as the" +:+ (introduceAbb ssa) +:+
  S "program. This section explains the purpose of this document," +:+
  S "the scope of the system, the organization of the document and" +:+
  S "the" +:+ (phrase characteristics) +:+ S "of the intended readers."

-- SECTION 2.1 --
s2_1 = Sub $ Section (titleize purpose) [s2_1_p1, s2_1_p2]

s2_1_p1 = Con $ Paragraph $ S "The" +:+ (short ssa) +:+ S "program determines the" +:+
  S "critical slip surface, and it's respective factor of safety" +:+
  S "as a method of assessing the stability of a slope design." +:+
  S "The program is intended to be used as an educational tool for" +:+
  S "introducing slope stability issues, and will facilitate the" +:+
  S "analysis and design of a safe slope."

s2_1_p2 = Con $ Paragraph $ S "This document will be used as a" +:+
  S "starting point for subsequent development phases, including" +:+
  S "writing the design specification and the software verification" +:+
  S "and validation plan.  The design document will show how the" +:+
  (plural requirement) +:+ S "are to be realized, including decisions on the" +:+
  S "numerical algorithms and programming environment.  The" +:+
  S "verification and validation plan will show the steps that will" +:+
  S "be used to increase confidence in the software documentation and" +:+
  S "the implementation.  Although the" +:+ (short srs) +:+ S "fits in a series of" +:+
  S "documents that follow the so-called waterfall model, the actual" +:+
  S "development process is not  constrained in any way.  Even when" +:+
  S "the waterfall model is not followed, as Parnas and Clements point" +:+
  S "out, the most logical way to present the documentation is still to"+:+
  S "fake a rational design process."

-- SECTION 2.2 --
s2_2 = Sub $ Section (titleize scpOfReq) [s2_2_p1]

s2_2_p1 = Con $ Paragraph $ S "The scope of the requirements is" +:+
  S "limited to stability analysis of a 2 dimensional slope," +:+
  S "composed of homogeneous soil layers. Given appropriate" +:+
  S "inputs, the code for" +:+ (short ssa) +:+ S "will identify the most likely" +:+
  S "failure surface within the possible input range, and find" +:+
  S "the factor of safety for the slope as well as displacement" +:+
  S "of soil that will occur on the slope."

-- SECTION 2.3 --
s2_3 = Sub $ Section (titleize orgOfDoc) [s2_3_p1]

s2_3_p1 = Con $ Paragraph $ S "The" +:+ (phrase organization) +:+
  S "of this" +:+ (phrase document) +:+ S "follows the template" +:+ 
  S "for an" +:+ (short srs) +:+ S "for scientific computing" +:+
  S "software proposed by Koothoor as well as Smith and Lai." +:+ 
  S "The presentation follows the standard pattern of presenting" +:+
  S "goals" `sC` (plural theory) `sC` (plural definition) `sC`
  S "and" +:+. (plural assumption) +:+ S "For readers" +:+
  S "that would like a more bottom up approach, they can start" +:+
  S "reading the instance models in" +:+ makeRef sec_IMs +:+
  S "and trace back to find any additional information they" +:+
  S "require.  The instance models provide the set of algebraic" +:+
  S "equations that must be solved iteratively to perform a" +:+
  S "Morgenstern Price Analysis. The goal statements are refined" +:+
  S "to the theoretical models" +:+ (sParen . makeRef) sec_TMs +:+ 
  S "and instance models" +:+. (sParen . makeRef) sec_IMs

-- SECTION 3 --
s3 = Section (titleize generalSystemDescription) [s3_p1, s3_1, s3_2]

s3_p1 = Con $ Paragraph $ S "This section provides general information" +:+
  S "about the system, identifies the interfaces between the" +:+
  S "system and its environment, and describes the user characteristics" +:+
  S "and the system constraints."

-- SECTION 3.1 --
s3_1 = Sub $ Section (S "User" +:+ titleize characteristics) [s3_1_p1]

s3_1_p1 = Con $ Paragraph $ S "The end user of" +:+ (short ssa) +:+ S "should" +:+
  S "have an understanding of undergraduate Level 1 Calculus and" +:+
  S "Physics, and be familiar with soil and material properties."

-- SECTION 3.2 --
s3_2 = Sub $ Section (S "System Constraints") [s3_2_p1]

s3_2_p1 = Con $ Paragraph $ S "There are no system constraints."

-- SECTION 4 --
s4 = Section (titleize specificsystemdescription) [s4_p1, s4_1, s4_2]

s4_p1 = Con $ Paragraph $ S "This section first presents the" +:+
  S "problem description, which gives a high-level view of the" +:+
  S "problem to be solved.  This is followed by the solution" +:+
  (phrase characteristicsSpecification) `sC` S "which presents the" +:+ 
  (plural assumption) `sC` (plural theory) `sC` (plural definition) +:+
  S "and finally the instance models that model the slope."

-- SECTION 4.1 --
s4_1 = Sub $ Section (titleize problemDescription) [s4_1_p1, s4_1_1, s4_1_2, s4_1_3]

s4_1_p1 = Con $ Paragraph $ (short ssa) +:+ S "is a computer program developed" +:+
  S "to evaluate the factor of safety of a slopes slip surface and" +:+ --FIXME apostrophe on "slope's"
  S "to calculate the displacement that the slope will experience."

-- SECTION 4.1.1 --
s4_1_1 = Sub $ Section (S "Terminology") [s4_1_1_list]

s4_1_1_list = Con $ Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "Factor of Safety", 
      S "Stability metric. How likely a slip surface is to experience" +:+
      S "failure through slipping."), 
  (S "Critical Slip Surface", 
      S "Slip surface of the slope that has the lowest global factor of" +:+
      S "safety, and therefore most likely to experience failure."),
  (S "Stress", 
      S "Forces that are exerted between planes internal to a larger" +:+
      S "body subject to external loading."),
  (S "Strain", 
      S "Stress forces that result in deformation of the body/plane."),
  (S "Normal Force", 
      S "A force applied perpendicular to the plane of the material."),
  (S "Shear Force", 
      S "A force applied parallel to the plane of the material."),
  (S "Tension", 
      S "A stress the causes displacement of the body away from it's center."),
  (S "Compression", 
      S "A stress the causes displacement of the body towards it's center."),
  (S "Plane Strain", 
      S "The resultant stresses in one of the directions of a" +:+
      S "3 dimensional material can be approximated as 0. Results" +:+
      S "when the length of one dimension of the body dominates the" +:+
      S "others. Stresses in the dominate dimensions direction are" +:+
      S "the ones that can be approximated as 0.")
  ]

-- SECTION 4.1.2 --
s4_1_2 = Sub $ Section (S "Physical" +:+ (titleize systemdescription)) --FIXME: Use proper compound nounPhrase for the whole title?
  [s4_1_2_p1, s4_1_2_bullets, s4_1_2_p2, s4_1_2_fig1, s4_1_2_fig2]

s4_1_2_p1 = Con $ Paragraph $ S "Analysis of the slope is performed" +:+
  S "by looking at properties of the slope as a series" +:+
  S "of slice elements. Some properties are interslice" +:+
  S "properties, and some are slice or slice base properties." +:+
  S "The index convention for referencing which interslice" +:+
  S "or slice is being used is shown in" +:+. (makeRef fig_indexconv)

s4_1_2_bullets = Con $ Enumeration $ Bullet $ map Flat [
  (S "Interslice properties convention is noted by j. The end" +:+
    S "interslice properties are usually not of interest" `sC` 
    S "therefore use the interslice properties from 1" +:+
    P (Special LEQ) +:+ S "i" +:+ P (Special LEQ) +:+. S "n-1"),
  (S "Slice properties convention is noted by i.")
  ]
  
s4_1_2_p2 = Con $ Paragraph $ S "A" +:+ (phrase $ fbd ^. term) +:+ S "of the forces" +:+
  S "acting on the slice is displayed in " :+: (makeRef fig_forceacting) :+: S "."

s4_1_2_fig1 = Con fig_indexconv

fig_indexconv :: Contents
fig_indexconv = Figure (S "Index convention for numbering slice and" +:+
  S "interslice force variables")  "IndexConvention.png"

s4_1_2_fig2 = Con fig_forceacting

fig_forceacting :: Contents
fig_forceacting = Figure (S "Forces acting on a slice") "ForceDiagram.png"

-- SECTION 4.1.3 --
s4_1_3 = Sub $ Section (S "Goal Statements") [s4_1_3_p1, s4_1_3_list]

s4_1_3_p1 = Con $ Paragraph $ S "Given the geometry of the water" +:+
  S "table, the geometry of the layers composing the plane of a" +:+
  S "slope, and the material properties of the layers."

s4_1_3_list = Con $ Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "GS1", S "Evaluate local and global factors of safety along" +:+
            S "a given slip surface."),
  (S "GS2", S "Identify the critical slip surface for the slope" `sC` 
            S "with the lowest Factor of Safety."),
  (S "GS3", S "Determine the displacement of the slope.")
  ]

-- SECTION 4.2 --
s4_2 = Sub $ Section (S "Solution" +:+ (titleize characteristicsSpecification))
  [s4_2_p1, s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5, s4_2_6]

s4_2_p1 = Con $ Paragraph $ S "The instance models that govern" +:+
  (short ssa) +:+ S "are presented in" +:+. makeRef sec_IMs +:+
  S "The information to understand the meaning of the instance" +:+
  S "models and their derivation is also presented, so that the" +:+
  S "instance models can be verified."

-- SECTION 4.2.1 --
s4_2_1 = Sub $ Section (titleize' assumption) [s4_2_1_p1, s4_2_1_list]

s4_2_1_p1 = Con $ Paragraph $ S "This section simplifies the" +:+
  S "original problem and helps in developing the theoretical" +:+
  S "model by filling in the missing information for the" +:+. (phrase physicalSystem) +:+
  S "The numbers given in the square brackets refer to" +:+
  S "the data definition, or the instance model, in which the" +:+
  S "respective" +:+ (phrase assumption) +:+ S "is used."

s4_2_1_list = Con $ Enumeration $ Simple $ (map (\(a,b) -> (a, Flat b)) [
  (S "A1", S "The slip surface is concave with respect to" +:+
           S "the slope surface. The" +:+ P (coords ^. symbol) +:+ 
           S "coordinates of the failure surface follow a" +:+
           S "monotonic function."),
  (S "A2", S "The geometry of the slope, and the material" +:+
           S "properties of the soil layers are given as inputs."),
  (S "A3", S "The different layers of the soil are homogeneous," +:+
           S "with consistent soil properties throughout," +:+
           S "and independent of dry or saturated conditions," +:+
           S "with the exception of unit weight."),
  (S "A4", S "Soil layers are treated as if they have" +:+
           S "isotropic properties."),
  (S "A5", S "Interslice normal and shear forces have a" +:+
           S "linear relationship, proportional to a constant" +:+
           sParen (P (lambda ^. symbol)) +:+ S "and an" +:+
           S "interslice force function" +:+ sParen (P (fi ^. symbol)) +:+
           S "depending on x position."),
  (S "A6", S "Slice to base normal and shear forces have" +:+
           S "a linear relationship, dependent on the factor" +:+
           S "of safety" +:+ sParen (P (fs ^. symbol)) :+:
           S ", and the Coulomb sliding law."),
  (S "A7", S "The stress-strain curve for interslice" +:+
           S "relationships is linear with a constant slope."),
  (S "A8", S "The slope and slip surface extends far into" +:+
           S "and out of the geometry (z coordinate). This" +:+
           S "implies plane strain conditions, making 2D" +:+
           S "analysis appropriate."),
  (S "A9", S "The effective normal stress is large enough" +:+
           S "that the resistive shear to effective normal" +:+
           S "stress relationship can be approximated as a" +:+
           S "linear relationship."),
  (S "A10", S "The surface and base of a slice between" +:+
            S "interslice nodes are approximated as" +:+
            S "straight lines.")
  ])

-- SECTION 4.2.2 --
s4_2_2 = Sub sec_TMs

sec_TMs :: Section
sec_TMs = Section (titleize' thModel) (s4_2_2_p1:s4_2_2_tmods)

s4_2_2_p1 = Con $ Paragraph $ S "This section focuses on the" +:+
  S "general equations and laws that" +:+ (short ssa) +:+
  S "is based on."

s4_2_2_tmods = map (Con . Definition) [Theory fs_rc] --FIX fs_rc to use lowercase

-- SECTION 4.2.3 --
s4_2_3 = Sub $ Section (S "General" +:+ (titleize' definition)) []

-- SECTION 4.2.4 --
s4_2_4 = Sub $ Section (S "Data" +:+ (titleize' definition)) []

-- SECTION 4.2.5 --
s4_2_5 = Sub sec_IMs

sec_IMs :: Section
sec_IMs = Section (S "Instance Models") []

-- SECTION 4.2.6 --
s4_2_6 = Sub $ Section (S "Data Constraints") []

-- SECTION 5 --
s5 = Section (titleize' requirement) [s5_p1, s5_1, s5_2]

s5_p1 = Con $ Paragraph $ S "This section provides the" +:+
  S "functional" +:+ (plural requirement) `sC` 
  S "the business tasks that the software" +:+
  S "is expected to complete, and the nonfunctional" +:+ 
  (plural requirement) `sC`
  S "the qualities that the software is expected to exhibit."

-- SECTION 5.1 --
s5_1 = Sub $ Section (S "Functional" +:+ (titleize' requirement))
  [s5_1_list, s5_1_table]

s5_1_list = Con $ Enumeration $ Simple $ (map (\(a,b) -> (a, Flat b)) [
  (S "R1" , S "Read the input file, and store the" +:+
            S "data. Necessary input data summarized in" +:+.
            makeRef table_inputdata),
  (S "R2" , S "Generate potential critical slip" +:+ 
            S "surface's for the input slope."),
  (S "R3" , S "Test the slip surfaces to determine" +:+
            S "if they are physically realizable based" +:+
            S "on a set of pass or fail criteria."),
  (S "R4" , S "Prepare the slip surfaces for a method" +:+
            S "of slices or limit equilibrium analysis."),
  (S "R5" , S "Calculate the factors of safety of the" +:+
            S "slip surfaces."),
  (S "R6" , S "Rank and weight the slopes based on their" +:+
            S "factor of safety, such that a slip surface" +:+
            S "with a smaller factor of safety has a larger" +:+
            S "weighting."),
  (S "R7" , S "Generate new potential critical slip" +:+
            S "surfaces based on previously analysed" +:+
            S "slip surfaces with low factors of safety."),
  (S "R8" , S "Repeat" +:+ plural requirement +:+ S "R3 to R7 until the" +:+
            S "minimum factor of safety remains approximately" +:+
            S "the same over a predetermined number of" +:+
            S "repetitions. Identify the slip surface that" +:+
            S "generates the minimum factor of safety as" +:+
            S "the critical slip surface."),
  (S "R9" , S "Prepare the critical slip surface for" +:+
            S "method of slices or limit equilibrium analysis."),
  (S "R10", S "Calculate the factor of safety of the" +:+
            S "critical slip surface using the Morgenstern" +:+
            S "price method."),
  (S "R11", S "Display the critical slip surface and the" +:+
            S "slice element displacements graphically. Give" +:+
            S "the values of the factors of safety calculated" +:+
            S "by the Morgenstern price method.")
  ])
  
s5_1_table = Con table_inputdata

table_inputdata :: Contents
table_inputdata =  Table (map titleize [symbol_, units_, description]) 
  (mkTable
    [(\ch -> P (ch ^. symbol)),
     (\ch -> Sy $ unit_symb ch),
     (\ch -> phrase $ ch ^. term)]
    [coords, elastMod, cohesion, poisson, fricAngle, dryWeight,
      satWeight, waterWeight])
  (S "Input data") True
 
-- SECTION 5.2 --
s5_2 = Sub $ Section (S "Nonfunctional" +:+ (titleize' requirement)) [s5_2_p1]

s5_2_p1 = Con $ Paragraph $ (short ssa) +:+ S "is intended to be an" +:+
  S "educational tool, therefore accuracy and performance speed" +:+
  S "are secondary program priorities to correctness," +:+
  S "understandability, reusability, and maintainability."

-- SECTION 6 --
s6 = Section (titleize' likelyChg) []
