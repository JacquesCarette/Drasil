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

this_si :: [UnitDefn]
this_si = map UU [metre, degree] ++ map UU [newton, pascal]

ssp_srs :: Document  
ssp_srs = SRS.doc sSA (name henryFrankis) [s1, s2, s3, s4, s5, s6]

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

ssp_mg :: Document
ssp_mg = mgDoc sSA (name henryFrankis) mgBod

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
s1_1 = Section (S "Table of Units") [s1_1_intro, s1_1_table]

s1_1_intro = Con $ Paragraph (S "Units of the physical properties of the" +:+
  S "soil that are of interest when examining slope stability problems" +:+
  S "are given in the following table.")

s1_1_table = Con $ Table [S "Symbol", S "Description", S "Name"] (mkTable
  [(\x -> Sy (x ^. usymb)),
   (\x -> (x ^. defn)),
   (\x -> (phrase $ x ^. term))
  ] this_si)
  (S "Table of Units") True

-- SECTION 1.2 --
s1_2 = Section (titleize tOfSymb) [s1_2_intro, s1_2_table]

s1_2_intro = Con $ Paragraph $
  S "A collection of the symbols that will be used in the models" +:+
  S "and equations of the program are summarized in the table" +:+
  S "below. Values with a subscript i implies that the value will" +:+
  S "be taken at and analyzed at a slice or slice interface" +:+
  S "composing the total slip mass."

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
  S "resulting in signifcant life and economic loses. Slope" +:+
  S "stability is of interest both when analyzing natural slopes," +:+
  S "and when designing an excavated slope. Slope stability analysis" +:+
  S "is the assessment of the safety of a slope, identifying the" +:+
  S "surface most likely to experience slip and an index of it's" +:+
  S "relative stability known as the factor of safety."

s2_p2 = Con $ Paragraph $ S "The following section provides an" +:+
  S "overview of the Software Requirements Specification (SRS) for" +:+
  S "a slope stability analysis problem. The developed program will be referred to as the Slope Stability Analysis (SSA) program. This section explains the purpose of this document, the scope of the system, the organization of the document and the characteristics of the intended readers."

-- SECTION 2.1 --
s2_1 = Sub $ Section (S "Purpose") [s2_1_p1, s2_1_p2]

s2_1_p1 = Con $ Paragraph $ S "The SSA program determines the critical slip surface, and it's respective factor of safety as a method of assessing the stability of a slope design. The program is intended to be used as an educational tool for introducing slope stability issues, and will facilitate the analysis and design of a safe slope."

s2_1_p2 = Con $ Paragraph $ S "This document will be used as a starting point for subsequent development phases, including writing the design specification and the software verification and validation plan.  The design document will show how the requirements are to be realized, including decisions on the numerical algorithms and programming environment.  The verification and validation plan will show the steps that will be used to increase confidence in the software documentation and the implementation.  Although the SRS fits in a series of documents that follow the so-called waterfall model, the actual development process is not  constrained in any way.  Even when the waterfall model is not followed, as Parnas and Clements point out, the most logical way to present the documentation is still to fake a rational design process."

-- SECTION 2.2 --
s2_2 = Sub $ Section (S "Scope of Requirements") [s2_2_p1]

s2_2_p1 = Con $ Paragraph $ S "The scope of the requirements is limited to stability analysis of a 2 dimensional slope, composed of homogeneous soil layers. Given appropriate inputs, the code for SSA will identify the most likely failure surface within the possible input range, and find the factor of safety for the slope as well as displacement of soil that will occur on the slope."

-- SECTION 2.3 --
s2_3 = Sub $ Section (titleize orgOfDoc) [s2_3_p1]

s2_3_p1 = Con $ Paragraph $ S "The organization of this document follows the template for an SRS for scientific computing software proposed by Koothoor as well as Smith and Lai.  The presentation follows the standard pattern of presenting goals, theories, definitions, and assumptions.  For readers that would like a more bottom up approach, they can start reading the instance models in " :+: makeRef sec_IMs :+: S " and trace back to find any additional information they require.  The instance models provide the set of algebraic equations that must be solved iteratively to perform a Morgenstern Price Analysis. The goal statements are refined to the theoretical models (" :+: makeRef sec_TMs :+: S ") and instance models (" :+: makeRef sec_IMs :+: S ")."

-- SECTION 3 --
s3 = Section (S "General" +:+ (titleize systemdescription)) [s3_p1, s3_1, s3_2]

s3_p1 = Con $ Paragraph $ S "This section provides general information about the system, identifies the interfaces between the system and its environment, and describes the user characteristics and the system constraints."

-- SECTION 3.1 --
s3_1 = Sub $ Section (S "User Characteristics") [s3_1_p1]

s3_1_p1 = Con $ Paragraph $ S "The end user of SSA should have an understanding of undergraduate Level 1 Calculus and Physics, and be familiar with soil and material properties."

-- SECTION 3.2 --
s3_2 = Sub $ Section (S "System Constraints") [s3_2_p1]

s3_2_p1 = Con $ Paragraph $ S "There are no system constraints."

-- SECTION 4 --
s4 = Section (S "Specific" +:+ (titleize systemdescription)) [s4_p1, s4_1, s4_2]

s4_p1 = Con $ Paragraph $ S "This section first presents the problem description, which gives a high-level view of the problem to be solved.  This is followed by the solution characteristics specification, which presents the assumptions, theories, definitions and finally the instance models that model the slope."

-- SECTION 4.1 --
s4_1 = Sub $ Section (S "Problem Description") [s4_1_p1, s4_1_1, s4_1_2, s4_1_3]

s4_1_p1 = Con $ Paragraph $ S "SSA is a computer program developed to evaluate the factor of safety of a slopes slip surface and to calculate the displacement that the slope will experience."

-- SECTION 4.1.1 --
s4_1_1 = Sub $ Section (S "Terminology") [s4_1_1_list]

s4_1_1_list = Con $ Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "Factor of Safety", S "Stability metric. How likely a slip surface is to experience failure through slipping."), 
  (S "Critical Slip Surface", S "Slip surface of the slope that has the lowest global factor of safety, and therefore most likely to experience failure."),
  (S "Stress", S "Forces that are exerted between planes internal to a larger body subject to external loading."),
  (S "Strain", S "Stress forces that result in deformation of the body/plane."),
  (S "Normal Force", S "A force applied perpendicular to the plane of the material."),
  (S "Shear Force", S "A force applied parallel to the plane of the material."),
  (S "Tension", S "A stress the causes displacement of the body away from it's center."),
  (S "Compression", S "A stress the causes displacement of the body towards it's center."),
  (S "Plane Strain", S "The resultant stresses in one of the directions of a 3 dimensional material can be approximated as 0. Results when the length of one dimension of the body dominates the others. Stresses in the dominate dimensions direction are the ones that can be approximated as 0.")
  ]

-- SECTION 4.1.2 --
s4_1_2 = Sub $ Section (S "Physical" +:+ (titleize systemdescription))
  [s4_1_2_p1, s4_1_2_bullets, s4_1_2_p2, s4_1_2_fig1, s4_1_2_fig2]

s4_1_2_p1 = Con $ Paragraph $ S "Analysis of the slope is performed by looking at properties of the slope as a series of slice elements. Some properties are interslice properties, and some are slice or slice base properties.  The index convention for referencing which interslice or slice is being used is shown in " :+: (makeRef fig_indexconv) :+: S "."

s4_1_2_bullets = Con $ Enumeration $ Bullet $ map Flat [
  (S "Interslice properties convention is noted by j. The end interslice properties are usually not of interest, therefore use the interslice properties from 1 " :+: P (Special LEQ) :+: S " i " :+: P (Special LEQ) :+: S " n-1."),
  (S "Slice properties convention is noted by i.")
  ]
  
s4_1_2_p2 = Con $ Paragraph $ S "A free body diagram of the forces acting on the slice is displayed in " :+: (makeRef fig_forceacting) :+: S "."

s4_1_2_fig1 = Con fig_indexconv

fig_indexconv :: Contents
fig_indexconv = Figure (S "Index convention for numbering slice and interslice force variables")  "IndexConvention.png"

s4_1_2_fig2 = Con fig_forceacting

fig_forceacting :: Contents
fig_forceacting = Figure (S "Forces acting on a slice") "ForceDiagram.png"

-- SECTION 4.1.3 --
s4_1_3 = Sub $ Section (S "Goal Statements") [s4_1_3_p1, s4_1_3_list]

s4_1_3_p1 = Con $ Paragraph $ S "Given the geometry of the water table, the geometry of the layers composing the plane of a slope, and the material properties of the layers."

s4_1_3_list = Con $ Enumeration $ Simple $ [
  (S "GS1", Flat $ S "Evaluate local and global factors of safety along a given slip surface."),
  (S "GS2", Flat $ S "Identify the critical slip surface for the slope, with the lowest Factor of Safety."),
  (S "GS3", Flat $ S "Determine the displacement of the slope.")
  ]

-- SECTION 4.2 --
s4_2 = Sub $ Section (S "Solution Characteristics Specification")
  [s4_2_p1, s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5, s4_2_6]

s4_2_p1 = Con $ Paragraph $ S "The instance models that govern SSA are presented in " :+: makeRef sec_IMs :+: S ".  The information to understand the meaning of the instance models and their derivation is also presented, so that the instance models can be verified."

-- SECTION 4.2.1 --
s4_2_1 = Sub $ Section (S "Assumptions") [s4_2_1_p1, s4_2_1_list]

s4_2_1_p1 = Con $ Paragraph $ S "This section simplifies the original problem and helps in developing the theoretical model by filling in the missing information for the physical system. The numbers given in the square brackets refer to the data definition, or the instance model, in which the respective assumption is used."

s4_2_1_list = Con $ Enumeration $ Simple $ [
  (S "A1", Flat $ S "The slip surface is concave with respect to the slope surface. The " :+: P (coords ^. symbol) :+: S " coordinates of the failure surface follow a monotonic function."),
  (S "A2", Flat $ S "The geometry of the slope, and the material properties of the soil layers are given as inputs."),
  (S "A3", Flat $ S "The different layers of the soil are homogeneous, with consistent soil properties throughout, and independent of dry or saturated conditions, with the exception of unit weight."),
  (S "A4", Flat $ S "Soil layers are treated as if they have isotropic properties."),
  (S "A5", Flat $ S "Interslice normal and shear forces have a linear relationship, proportional to a constant (" :+: P (lambda ^. symbol) :+: S ") and an interslice force function (" :+: P (fi ^. symbol) :+: S ") depending on x position."),
  (S "A6", Flat $ S "Slice to base normal and shear forces have a linear relationship, dependent on the factor of safety (" :+: P (fs ^. symbol) :+: S "), and the Coulomb sliding law."),
  (S "A7", Flat $ S "The stress-strain curve for interslice relationships is linear with a constant slope."),
  (S "A8", Flat $ S "The slope and slip surface extends far into and out of the geometry (z coordinate). This implies plane strain conditions, making 2D analysis appropriate."),
  (S "A9", Flat $ S "The effective normal stress is large enough that the resistive shear to effective normal stress relationship can be approximated as a linear relationship."),
  (S "A10", Flat $ S "The surface and base of a slice between interslice nodes are approximated as straight lines.")
  ]

-- SECTION 4.2.2 --
s4_2_2 = Sub sec_TMs

sec_TMs :: Section
sec_TMs = Section (S "Theoretical Models") (s4_2_2_p1:s4_2_2_tmods)

s4_2_2_p1 = Con $ Paragraph $ S "This section focuses on the general equations and laws that SSA is based on."

s4_2_2_tmods = map (Con . Definition) (map Theory [fs_rc])

-- SECTION 4.2.3 --
s4_2_3 = Sub $ Section (S "General Definitions") []

-- SECTION 4.2.4 --
s4_2_4 = Sub $ Section (S "Data Definitions") []

-- SECTION 4.2.5 --
s4_2_5 = Sub sec_IMs

sec_IMs :: Section
sec_IMs = Section (S "Instance Models") []

-- SECTION 4.2.6 --
s4_2_6 = Sub $ Section (S "Data Constraints") []

-- SECTION 5 --
s5 = Section (S "Requirements") [s5_p1, s5_1, s5_2]

s5_p1 = Con $ Paragraph $ S "This section provides the functional requirements, the business tasks that the software is expected to complete, and the nonfunctional requirements, the qualities that the software is expected to exhibit."

-- SECTION 5.1 --
s5_1 = Sub $ Section (S "Functional Requirements")
  [s5_1_list, s5_1_table]

s5_1_list = Con $ Enumeration $ Simple $ [
  (S "R1", Flat $ S "Read the input file, and store the data. Necessary input data summarized in " :+: makeRef table_inputdata :+: S "."),
  (S "R2", Flat $ S "Generate potential critical slip surface's for the input slope."),
  (S "R3", Flat $ S "Test the slip surfaces to determine if they are physically realizable based on a set of pass or fail criteria."),
  (S "R4", Flat $ S "Prepare the slip surfaces for a method of slices or limit equilibrium analysis."),
  (S "R5", Flat $ S "Calculate the factors of safety of the slip surfaces."),
  (S "R6", Flat $ S "Rank and weight the slopes based on their factor of safety, such that a slip surface with a smaller factor of safety has a larger weighting."),
  (S "R7", Flat $ S "Generate new potential critical slip surfaces based on previously analysed slip surfaces with low factors of safety."),
  (S "R8", Flat $ S "Repeat requirements R3 to R7 until the minimum factor of safety remains approximately the same over a predetermined number of repetitions. Identify the slip surface that generates the minimum factor of safety as the critical slip surface."),
  (S "R9", Flat $ S "Prepare the critical slip surface for method of slices or limit equilibrium analysis."),
  (S "R10", Flat $ S "Calculate the factor of safety of the critical slip surface using the Morgenstern price method."),
  (S "R11", Flat $ S "Display the critical slip surface and the slice element displacements graphically. Give the values of the factors of safety calculated by the Morgenstern price method.")
  ]
  
s5_1_table = Con table_inputdata

table_inputdata :: Contents
table_inputdata =  Table (map titleize [symbol_, units_, description]) (mkTable
  [(\ch -> P (ch ^. symbol)),
   (\ch -> Sy $ unit_symb ch),
   (\ch -> phrase $ ch ^. term)
   ]
  [coords, elastMod, cohesion, poisson, fricAngle, dryWeight, satWeight, waterWeight])
  (S "Input data") True
 
-- SECTION 5.2 --
s5_2 = Sub $ Section (S "Nonfunctional Requirements") [s5_2_p1]

s5_2_p1 = Con $ Paragraph $ S "SSA is intended to be an educational tool, therefore accuracy and performance speed are secondary program priorities to correctness, understandability, reusability, and maintainability."

-- SECTION 6 --
s6 = Section (S "Likely Changes") []
