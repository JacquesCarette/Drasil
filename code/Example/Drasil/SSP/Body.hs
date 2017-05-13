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

import Drasil.ReferenceMaterial
import Drasil.DocumentLanguage

import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Physics
import Data.Drasil.Concepts.PhysicalProperties
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Math (unit_)

import Drasil.Template.MG
import Drasil.Template.DD

--type declerations for sections--
s2, s3, s4, s5, s6 :: Section

s1_2_intro :: [TSIntro]

s2_1, s2_2, s2_3, s3_1, s3_2, s4_1, s4_1_1, s4_1_2,
  s4_1_3, s4_2, s4_2_1, s4_2_2, s4_2_3, s4_2_4,
  s4_2_5, s4_2_6, s5_1, s5_2 :: Section

s2_p1, s2_p2, s2_1_p1, s2_1_p2, s2_2_p1, s2_3_p1, s3_p1,
  s3_1_p1, s3_2_p1, s4_p1, s4_1_p1, s4_1_1_list, s4_1_2_p1, 
  s4_1_2_bullets, s4_1_2_p2, s4_1_2_fig1, s4_1_2_fig2, 
  s4_1_3_p1, s4_1_3_list, s4_2_p1, s4_2_1_p1, s4_2_1_list, 
  s4_2_2_p1, s5_p1, s5_1_list, s5_1_table, s5_2_p1 :: Contents

s4_2_2_tmods :: [Contents]

--Document Settup--
this_si :: [UnitDefn]
this_si = map UU [metre, degree] ++ map UU [newton, pascal]

ssp_si :: SystemInformation
ssp_si = SI ssa srs [henryFrankis]
  this_si sspSymbols (sspSymbols) acronyms 

mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro 
  [TUnits, tsymb s1_2_intro, TAandA]
  ) : map Verbatim [s2, s3, s4, s5, s6]

ssp_srs, ssp_mg :: Document
ssp_srs = mkDoc mkSRS ssp_si
ssp_mg = mgDoc ssa (name henryFrankis) mgBod

mgBod :: [Section]
(mgBod, _) = makeDD lcs ucs reqs modules

-- SECTION 1 --
--automaticly generated in mkSRS 

-- SECTION 1.1 --
--automaticly generated in mkSRS 

-- SECTION 1.2 --
--automaticly generated in mkSRS using the intro bellow

s1_2_intro = [TSPurpose, TypogConvention [Verb $
  S "values with a subscript i implies that the value will" +:+
  S "be taken at and analyzed at a slice or slice interface" +:+
  S "composing the total slip" +:+ (phrase $ mass ^. term)]]

-- SECTION 1.3 --
--automaticly generated in mkSRS 
  
-- SECTION 2 --
s2 = section (titleize introduction) [s2_p1, s2_p2] [s2_1, s2_2, s2_3]

s2_p1 = Paragraph $ S "A slope of geological" +:+ (phrase $ mass ^. term) `sC` 
  S "composed of soil and rock, is subject to the influence of gravity on" +:+
  S "the" +:+. (phrase $ mass ^. term) +:+ S "For an unstable slope this can cause" +:+
  S "instability in the form of soil/rock movement. The effects of soil/rock" +:+
  S "movement can range from inconvenient to seriously hazardous," +:+
  S "resulting in signifcant life and economic loses. Slope stability" +:+
  S "is of interest both when analyzing natural slopes," +:+
  S "and when designing an excavated slope." +:+ (at_start ssa) +:+
  S "is the assessment of the safety of a slope, identifying the" +:+
  S "surface most likely to experience slip and an index of it's" +:+
  S "relative stability known as the" +:+. (phrase $ fs_rc ^. term)

s2_p2 = Paragraph $ S "The following" +:+ (phrase section_) +:+
  S "provides an overview of the" +:+ (introduceAbb srs) +:+
  S "for a" +:+ (phrase ssa) +:+. (phrase problem) +:+ S "The developed" +:+ (phrase $ program ^. term) +:+
  S "will be referred to as the" +:+ (introduceAbb ssa) +:+.
  (phrase $ program ^. term) +:+ S "This" +:+ (phrase section_) +:+ S "explains the purpose of this document," +:+ --FIXME: purpose, scope and organization have a similar pattern here
  S "the scope of the system, the organization of the document and" +:+
  S "the" +:+ (plural characteristic) +:+ S "of the intended readers."

-- SECTION 2.1 --
s2_1 = section (titleize purpose) [s2_1_p1, s2_1_p2] []

s2_1_p1 = Paragraph $ S "The" +:+ (short ssa) +:+ (phrase $ program ^. term) +:+  S "determines the" +:+
  (phrase crtSlpSrf) `sC` S "and it's respective" +:+ (phrase $ fs_rc ^. term) +:+
  S "as a method of assessing the stability of a slope" +:+. (phrase design) +:+
  S "The" +:+ (phrase $ program ^. term) +:+ S "is intended to be used as an educational tool for" +:+
  S "introducing slope stability issues, and will facilitate the" +:+
  S "analysis and" +:+ (phrase design) +:+ S "of a safe slope."

s2_1_p2 = Paragraph $ S "This" +:+ (phrase document) +:+ S "will be used as a" +:+
  S "starting point for subsequent development phases, including" +:+
  S "writing the" +:+ (phrase desSpec) +:+ S "and the software" +:+
  (phrase vav) +:+ S "plan. The" +:+ (phrase design) +:+ (phrase document) +:+ S "will show how the" +:+
  (plural requirement) +:+ S "are to be realized, including decisions on the" +:+
  S "numerical algorithms and programming environment. The" +:+
  (phrase vav) +:+ S "plan will show the steps that will" +:+
  S "be used to increase confidence in the software documentation and" +:+
  S "the implementation. Although the" +:+ (short srs) +:+ S "fits in a series of" +:+
  (plural document) +:+ S "that follow the so-called waterfall" +:+ (phrase model) `sC`
  S "the actual development process is not  constrained in any way. Even when" +:+
  S "the waterfall" +:+ (phrase model) +:+ S "is not followed, as Parnas and Clements" +:+
  S "point out, the most logical way to present the documentation is still to" +:+
  S "fake a rational" +:+ (phrase design) +:+ S "process."

-- SECTION 2.2 --
s2_2 = section (titleize scpOfReq) [s2_2_p1] []

s2_2_p1 = Paragraph $ S "The scope of the requirements is" +:+ --FIXME: somehow use scpOfReq with a "the"
  S "limited to stability analysis of a 2 dimensional slope," +:+
  S "composed of homogeneous soil layers. Given appropriate" +:+
  S "inputs, the code for" +:+ (short ssa) +:+ S "will identify the most likely" +:+
  S "failure surface within the possible input range, and find" +:+
  S "the" +:+ (phrase $ fs_rc ^. term) +:+ S "for the slope as well as displacement" +:+
  S "of soil that will occur on the slope."

-- SECTION 2.3 --
s2_3 = section (titleize orgOfDoc) [s2_3_p1] []

s2_3_p1 = Paragraph $ S "The" +:+ (phrase organization) +:+
  S "of this" +:+ (phrase document) +:+ S "follows the template" +:+ 
  S "for an" +:+ (short srs) +:+ S "for" +:+ (phrase sciCompS) +:+
  S "proposed by Koothoor as well as Smith and Lai." +:+ 
  S "The presentation follows the standard pattern of presenting" +:+
  S "goals" `sC` (plural theory) `sC` (plural definition) `sC`
  S "and" +:+. (plural assumption) +:+ S "For readers" +:+
  S "that would like a more bottom up approach, they can start" +:+
  S "reading the" +:+ (plural instMdl) +:+ S "in" +:+ makeRef sec_IMs +:+
  S "and trace back to find any additional" +:+ (phrase information) +:+
  S "they require. The" +:+ (plural instMdl) +:+ S "provide the set of algebraic" +:+
  S "equations that must be solved iteratively to perform a" +:+
  S "Morgenstern Price Analysis. The" +:+ (plural goalStmt) +:+ S "are refined" +:+
  S "to the" +:+ (plural thModel) +:+ (sParen . makeRef) sec_TMs +:+ 
  S "and" +:+ (plural instMdl) +:+. (sParen . makeRef) sec_IMs

-- SECTION 3 --
s3 = section (titleize generalSystemDescription) [s3_p1] [s3_1, s3_2]

s3_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "provides general" +:+ (phrase information) +:+
  S "about the" +:+ (phrase system) `sC` S "identifies the interfaces between the" +:+
  (phrase system) +:+ S "and its environment, and describes the" +:+ (plural userCharacteristic) +:+
  S "and the" +:+. (plural systemConstraint)

-- SECTION 3.1 --
s3_1 = section (titleize' userCharacteristic) [s3_1_p1] []

s3_1_p1 = Paragraph $ S "The end" +:+ (phrase user) +:+ S "of" +:+ (short ssa) +:+
  S "should have an understanding of undergraduate Level 1 Calculus and" +:+
  S "Physics, and be familiar with soil and material properties."

-- SECTION 3.2 --
s3_2 = section (titleize' systemConstraint) [s3_2_p1] []
 
s3_2_p1 = Paragraph $ S "There are no" +:+. (plural systemConstraint)

-- SECTION 4 --
s4 = section (titleize specificsystemdescription) [s4_p1] [s4_1, s4_2]

s4_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "first presents the" +:+
  (phrase problemDescription) `sC` S "which gives a high-level view of the" +:+
  (phrase problem) +:+ S "to be solved. This is followed by the" +:+ (phrase solution) +:+
  (plural characteristicSpecification) `sC` S "which presents the" +:+ 
  (plural assumption) `sC` (plural theory) `sC` (plural definition) +:+
  S "and finally the" +:+ (plural instMdl) +:+ S "that" +:+ (phrase model) +:+ S "the slope."

-- SECTION 4.1 --
s4_1 = section (titleize problemDescription) [s4_1_p1] [s4_1_1, s4_1_2, s4_1_3]

s4_1_p1 = Paragraph $ (short ssa) +:+ S "is a computer" +:+ (phrase $ program ^. term) +:+ S "developed" +:+
  S "to evaluate the" +:+ (phrase $ fs_rc ^. term) +:+ S "of a slopes" +:+ (phrase slpSrf) +:+ --FIXME apostrophe on "slope's"
  S "and to calculate the displacement that the slope will experience."

-- SECTION 4.1.1 --
s4_1_1 = section (titleize terminology) [s4_1_1_list] []

s4_1_1_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (titleize $ fs_rc ^. term, 
      S "Stability metric. How likely a" +:+ (phrase slpSrf) +:+ S "is to experience" +:+
      S "failure through slipping."), 
  (titleize crtSlpSrf, 
      (at_start slpSrf) +:+ S "of the slope that has the lowest global" +:+
      (phrase $ fs_rc ^. term) `sC` S "and therefore most likely to experience failure."),
  (titleize $ stress ^. term, 
      stress ^. defn),
  (titleize $ strain ^. term, 
      strain ^. defn),
  (S "Normal Force", 
      S "A force applied perpendicular to the plane of the material."),
  (S "Shear Force", 
      S "A force applied parallel to the plane of the material."),
  (titleize $ tension ^. term, 
      tension ^. defn),
  (titleize $ compression ^. term, 
      compression ^. defn),
  (S "Plane Strain", 
      S "The resultant stresses in one of the directions of a" +:+
      S "3 dimensional material can be approximated as 0. Results" +:+
      S "when the length of one dimension of the body dominates the" +:+
      S "others. Stresses in the dominate dimensions direction are" +:+
      S "the ones that can be approximated as 0.")
  ]

-- SECTION 4.1.2 --
s4_1_2 = section (titleize physSyst)
  [s4_1_2_p1, s4_1_2_bullets, s4_1_2_p2, s4_1_2_fig1, s4_1_2_fig2] []

s4_1_2_p1 = Paragraph $ S "Analysis of the slope is performed" +:+
  S "by looking at properties of the slope as a series" +:+
  S "of slice elements. Some properties are interslice" +:+
  S "properties, and some are slice or slice base properties." +:+
  S "The index convention for referencing which interslice" +:+
  S "or slice is being used is shown in" +:+. (makeRef fig_indexconv)

s4_1_2_bullets = Enumeration $ Bullet $ map Flat [
  (S "Interslice properties convention is noted by j. The end" +:+
    S "interslice properties are usually not of interest" `sC` 
    S "therefore use the interslice properties from 1" +:+
    P (Special LEQ) +:+ S "i" +:+ P (Special LEQ) +:+. S "n-1"),
  (S "Slice properties convention is noted by i.")
  ]
  
s4_1_2_p2 = Paragraph $ S "A" +:+ (phrase $ fbd ^. term) +:+ S "of the forces" +:+
  S "acting on the slice is displayed in" +:+. (makeRef fig_forceacting)

s4_1_2_fig1 = fig_indexconv

fig_indexconv :: Contents
fig_indexconv = Figure (S "Index convention for numbering slice and" +:+
  S "interslice force variables")  "IndexConvention.png"

s4_1_2_fig2 = fig_forceacting

fig_forceacting :: Contents
fig_forceacting = Figure (S "Forces acting on a slice") "ForceDiagram.png"

-- SECTION 4.1.3 --
s4_1_3 = section (titleize' goalStmt) [s4_1_3_p1, s4_1_3_list] []

s4_1_3_p1 = Paragraph $ S "Given the geometry of the water" +:+
  S "table, the geometry of the layers composing the plane of a" +:+
  S "slope, and the material properties of the layers."

s4_1_3_list = Enumeration $ Simple $ map (\(a,b) -> (a, Flat b)) [
  (S "GS1", S "Evaluate local and global" +:+ (plural $ fs_rc ^. term) +:+
            S "along a given" +:+. phrase slpSrf),
  (S "GS2", S "Identify the" +:+ (phrase crtSlpSrf) +:+ S "for the slope" `sC` 
            S "with the lowest" +:+. (phrase $ fs_rc ^. term)),
  (S "GS3", S "Determine the displacement of the slope.")
  ]

-- SECTION 4.2 --
s4_2 = section (titleize' solutionCharSpec)
  [s4_2_p1] [s4_2_1, s4_2_2, s4_2_3, s4_2_4, s4_2_5, s4_2_6]

s4_2_p1 = Paragraph $ S "The" +:+ (plural instMdl) +:+ S "that govern" +:+
  (short ssa) +:+ S "are presented in" +:+. makeRef sec_IMs +:+
  S "The" +:+ (phrase information) +:+ S "to understand the meaning of the instance" +:+
  (plural model) +:+ S "and their derivation is also presented, so that the" +:+
  (plural instMdl) +:+ S "can be verified."

-- SECTION 4.2.1 --
s4_2_1 = section (titleize' assumption) [s4_2_1_p1, s4_2_1_list] []

s4_2_1_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "simplifies the" +:+
  S "original" +:+ (phrase problem) +:+ S "and helps in developing the" +:+ (phrase thModel) +:+
  S "by filling in the missing" +:+ (phrase information) +:+ S "for the" +:+.
  (phrase physicalSystem) +:+ S "The numbers given in the square brackets refer to" +:+
  S "the" +:+ (phrase dataDefn) `sC` S "or the" +:+ (phrase instMdl) `sC` S "in which the" +:+
  S "respective" +:+ (phrase assumption) +:+ S "is used."

s4_2_1_list = Enumeration $ Simple $ (map (\(a,b) -> (a, Flat b)) [
  (S "A1", S "The" +:+ (phrase slpSrf) +:+ S "is concave with respect to" +:+
           S "the slope surface. The" +:+ P (coords ^. symbol) +:+ 
           S "coordinates of the failure surface follow a" +:+
           S "monotonic function."),
  (S "A2", S "The geometry of the slope, and the material" +:+
           S "properties of the soil layers are given as inputs."),
  (S "A3", S "The different layers of the soil are homogeneous," +:+
           S "with consistent soil properties throughout," +:+
           S "and independent of dry or saturated" +:+ (plural condition) `sC`
           S "with the exception of" +:+ (phrase $ unit_ ^. term) +:+ S "weight."),
  (S "A4", S "Soil layers are treated as if they have" +:+
           S "isotropic properties."),
  (S "A5", S "Interslice normal and shear forces have a" +:+
           S "linear relationship, proportional to a constant" +:+
           (sParen $ P $ lambda ^. symbol) +:+ S "and an" +:+
           S "interslice force function" +:+ (sParen $ P $ fi ^. symbol) +:+
           S "depending on x position."),
  (S "A6", S "Slice to base normal and shear forces have" +:+
           S "a linear relationship, dependent on the" +:+
           (phrase $ fs_rc ^. term) +:+ (sParen $ P $ fs ^. symbol) `sC`
           S "and the Coulomb sliding law."),
  (S "A7", S "The stress-strain curve for interslice" +:+
           S "relationships is linear with a constant slope."),
  (S "A8", S "The slope and" +:+ (phrase slpSrf) +:+ S "extends far" +:+
           S "into and out of the geometry (z coordinate)." +:+
           S "This implies plane strain" +:+ (plural condition) `sC`
           S "making 2D analysis appropriate."),
  (S "A9", S "The effective normal stress is large enough" +:+
           S "that the resistive shear to effective normal" +:+
           S "stress relationship can be approximated as a" +:+
           S "linear relationship."),
  (S "A10", S "The surface and base of a slice between" +:+
            S "interslice nodes are approximated as" +:+
            S "straight lines.")
  ])

-- SECTION 4.2.2 --
s4_2_2 = sec_TMs

sec_TMs :: Section
sec_TMs = section (titleize' thModel) (s4_2_2_p1:s4_2_2_tmods) []

s4_2_2_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "focuses on the" +:+
  S "general equations and laws that" +:+ (short ssa) +:+
  S "is based on."

s4_2_2_tmods = map Definition [Theory fs_rc] --FIX fs_rc to use lowercase

-- SECTION 4.2.3 --
s4_2_3 = section (titleize' genDefn) [] []

-- SECTION 4.2.4 --
s4_2_4 = section (titleize' dataDefn) [] []

-- SECTION 4.2.5 --
s4_2_5 = sec_IMs

sec_IMs :: Section
sec_IMs = section (titleize' instMdl) [] []

-- SECTION 4.2.6 --
s4_2_6 = section (titleize' datumConstraint) [] []

-- SECTION 5 --
s5 = section (titleize' requirement) [s5_p1] [s5_1, s5_2]

s5_p1 = Paragraph $ S "This" +:+ (phrase section_) +:+ S "provides the" +:+
  S "functional" +:+ (plural requirement) `sC` 
  S "the business tasks that the software" +:+
  S "is expected to complete, and the nonfunctional" +:+ 
  (plural requirement) `sC`
  S "the qualities that the software is expected to exhibit."

-- SECTION 5.1 --
s5_1 = section (titleize' functionalRequirement)
  [s5_1_list, s5_1_table] []

s5_1_list = Enumeration $ Simple $ (map (\(a,b) -> (a, Flat b)) [
  (S "R1" , S "Read the input file, and store the" +:+
            S "data. Necessary input data summarized in" +:+.
            (makeRef table_inputdata)),
  (S "R2" , S "Generate potential" +:+ (phrase crtSlpSrf) :+:
            S "'s for the input slope."),
  (S "R3" , S "Test the" +:+ (plural slpSrf) +:+ S "to determine" +:+
            S "if they are physically realizable based" +:+
            S "on a set of pass or fail criteria."),
  (S "R4" , S "Prepare the" +:+ (plural slpSrf) +:+ S "for a method" +:+
            S "of slices or limit equilibrium analysis."),
  (S "R5" , S "Calculate the" +:+ (plural $ fs_rc ^. term) +:+ S "of the" +:+.
            (plural slpSrf)),
  (S "R6" , S "Rank and weight the slopes based on their" +:+
            (phrase $ fs_rc ^. term) `sC` S "such that a" +:+ (phrase slpSrf) +:+
            S "with a smaller" +:+ (phrase $ fs_rc ^. term) +:+
            S "has a larger weighting."),
  (S "R7" , S "Generate new potential" +:+ (plural crtSlpSrf) +:+
            S "based on previously analysed" +:+ (plural slpSrf) +:+
            S "with low" +:+. (plural $ fs_rc ^. term)),
  (S "R8" , S "Repeat" +:+ (plural requirement) +:+ S "R3 to R7 until the" +:+
            S "minimum" +:+ (phrase $ fs_rc ^. term) +:+ S "remains approximately" +:+
            S "the same over a predetermined number of" +:+
            S "repetitions. Identify the" +:+ (phrase slpSrf) +:+
            S "that generates the minimum" +:+ (phrase $ fs_rc ^. term) +:+
            S "as the" +:+. (phrase crtSlpSrf)),
  (S "R9" , S "Prepare the" +:+ (phrase crtSlpSrf) +:+ S "for" +:+
            S "method of slices or limit equilibrium analysis."),
  (S "R10", S "Calculate the" +:+ (phrase $ fs_rc ^. term) +:+ S "of the" +:+
            (phrase crtSlpSrf) +:+ S "using the Morgenstern" +:+
            S "price method."),
  (S "R11", S "Display the" +:+ (phrase crtSlpSrf) +:+ S "and the" +:+
            S "slice element displacements graphically. Give" +:+
            S "the values of the" +:+ (plural $ fs_rc ^. term) +:+ S "calculated" +:+
            S "by the Morgenstern price method.")
  ])
  
s5_1_table = table_inputdata

table_inputdata :: Contents
table_inputdata =  Table [titleize symbol_, titleize' $ unit_ ^. term, titleize description]
  (mkTable
    [(\ch -> P $ ch ^. symbol),
     (\ch -> unwrap $ getUnit ch),
     (\ch -> phrase $ ch ^. term)]
    ((map cqs [coords, elastMod, cohesion]) ++ (map cqs [poisson]) ++ --this has to be seperate since poisson is a different type
    map cqs [fricAngle, dryWeight, satWeight, waterWeight]))
  (S "Input data") True
    where unwrap :: (Maybe UnitDefn) -> Sentence
          unwrap (Just a) = Sy (a ^. usymb)
          unwrap Nothing = EmptyS
 
-- SECTION 5.2 --
s5_2 = section (titleize' nonfunctionalRequirement) [s5_2_p1] []

s5_2_p1 = Paragraph $ (short ssa) +:+ S "is intended to be an" +:+
  S "educational tool, therefore accuracy and performance speed" +:+
  S "are secondary" +:+ (phrase $ program ^. term) +:+ S "priorities to correctness," +:+
  S "understandability, reusability, and maintainability."

-- SECTION 6 --
s6 = section (titleize' likelyChg) [] []
