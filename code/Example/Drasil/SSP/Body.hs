module Drasil.SSP.Body where

import Control.Lens ((^.))
import Prelude hiding (id, sin, cos, tan)

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Authors

import Drasil.SSP.Defs
import Drasil.SSP.Unitals
import Drasil.SSP.Modules
import Drasil.SSP.Changes
import Drasil.SSP.Reqs
import Drasil.SSP.TMods
import Drasil.SSP.GenDefs
import Drasil.SSP.DataDefs
import Drasil.SSP.IMods
import qualified Drasil.SRS as SRS

import Drasil.Sections.ReferenceMaterial
import Drasil.DocumentLanguage
import Drasil.Sections.SpecificSystemDescription
import Drasil.Sections.Requirements
import Drasil.Sections.GeneralSystDesc

import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Physics
import Data.Drasil.Concepts.PhysicalProperties
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Computation
import Data.Drasil.Concepts.Math hiding (constraint)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)
import Data.Drasil.Software.Products

import Data.Drasil.Utils
import Data.Drasil.SentenceStructures

import Drasil.Template.MG
import Drasil.Template.DD

--type declarations for sections--
s3, s4, s5, s6, s7 :: Section

s1_2_intro :: [TSIntro]

s4_1, s4_1_1, s4_1_2,
  s4_1_3, s4_2, s5_1, s5_2 :: Section

s4_1_1_list, s4_1_2_p1, s4_1_2_bullets,
  s4_1_2_p2, s4_1_2_fig1, s4_1_2_fig2,
  s4_1_3_list, s4_2_1_list,
  s4_2_5_p2, s4_2_5_p3, s5_1_list, s5_1_table,
  s7_list :: Contents

s4_2_2_tmods, s4_2_3_genDefs, s4_2_4_dataDefs, s4_2_5_IMods :: [Contents]

--Document Setup--
this_si :: [UnitDefn]
this_si = map UU [metre, degree] ++ map UU [newton, pascal]

ssp_si :: SystemInformation
ssp_si = SI ssa srs [henryFrankis]
  this_si sspSymbols (sspSymbols) acronyms sspDataDefs (map qs sspInputs) (map qs sspOutputs)
  [Parallel (head sspDataDefs) (tail sspDataDefs)] sspConstrained

mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro
  [TUnits, tsymb'' s1_2_intro TAD, TAandA]) :
  IntroSec (IntroProg startIntro kSent
    [IPurpose prpsOfDoc_p1, IScope scpIncl scpEnd
    , IChar (S "solid mechanics") (S "undergraduate level 4" +:+ phrase physics)
        EmptyS
    , IOrgSec orgSecStart inModel (SRS.inModel SRS.missingP []) orgSecEnd]) :
    --FIXME: SRS.inModel should be removed and the instance model section
    --should be looked up from "inModel" by the interpreter while generating.
  map Verbatim [s3, s4, s5, s6, s7]

ssp_srs, ssp_mg :: Document
ssp_srs = mkDoc mkSRS ssp_si
ssp_mg = mgDoc ssa (name henryFrankis) mgBod

mgBod :: [Section]
(mgBod, _) = makeDD likelyChanges unlikelyChanges reqs modules

-- SYMBOL MAP HELPERS --
sspSymMap :: SymbolMap
sspSymMap = symbolMap sspSymbols

sspSymMapT :: RelationConcept -> Contents
sspSymMapT = symbolMapFun sspSymMap Theory

sspSymMapD :: QDefinition -> Contents
sspSymMapD = symbolMapFun sspSymMap Data

-- SECTION 1 --
--automatically generated in mkSRS -

-- SECTION 1.1 --
--automatically generated in mkSRS

-- SECTION 1.2 --
--automatically generated in mkSRS using the intro below

s1_2_intro = [TSPurpose, TypogConvention [Verb $
  plural value +:+S "with a subscript i implies that the" +:+
  phrase value +:+ S "will be taken at and analyzed at a" +:+
  phrase slice +:+ S "or" +:+ phrase slice +:+
  S "interface composing the total slip" +:+ phrase mass]]

-- SECTION 1.3 --
--automatically generated in mkSRS

-- SECTION 2 --
startIntro, kSent :: Sentence
startIntro = foldlSent [S "A", phrase slope, S "of geological",
  phrase mass `sC` S "composed of", phrase soil, S "and rock, is subject", 
  S "to the influence of gravity on the" +:+. phrase mass, S "For an unstable",
  phrase slope +:+. S "this can cause instability in the form of soil/rock movement",
  S "The effects of soil/rock movement can range from inconvenient to seriously",
  S "hazardous, resulting in signifcant life and economic loses.", at_start slope,
  S "stability is of", phrase interest, S "both when analyzing natural", plural slope `sC`
  S "and when designing an excavated" +:+. phrase slope, at_start ssa, S "is",
  S "assessment" `ofThe` S "safety of a", phrase slope `sC` S "identifying the",
  phrase surface, S "most likely to", S "experience slip and an index of", 
  S "it's relative stability known as the", phrase fs_rc]
kSent = S "a" +:+ phrase ssa +:+. phrase problem +:+ S "The developed"
  +:+ phrase program +:+ S "will be referred to as the" +:+ introduceAbb ssa +:+
  phrase program

-- SECTION 2.1 --
-- Purpose of Document automatically generated in introductionF
prpsOfDoc_p1 :: Sentence
prpsOfDoc_p1 = foldlSent [S "The", short ssa, phrase program,
  S "determines the", phrase crtSlpSrf `sC` S "and it's respective",
  phrase fs_rc, S "as a", phrase method_,
  S "of assessing the stability of a", phrase slope +:+. phrase design,
  S "The", phrase program,
  S "is intended to be used as an educational tool for",
  S "introducing", phrase slope, S "stability issues, and will facilitate the",
  phrase analysis, S "and", phrase design, S "of a safe", phrase slope]

-- SECTION 2.2 --
-- Scope of Requirements automatically generated in introductionF
scpIncl, scpEnd :: Sentence
scpIncl = S "stability analysis of a 2 dimensional" +:+ phrase slope `sC`
  S "composed of homogeneous" +:+ plural soilLyr
scpEnd  = S "identify the most likely failure" +:+
  phrase surface +:+ S "within the possible" +:+ phrase input_ +:+ S "range" `sC`
  S "and find the" +:+ phrase fs_rc +:+ S "for the" +:+
  phrase slope +:+ S "as well as displacement of" +:+ phrase soil +:+
  S "that will occur on the" +:+ phrase slope

-- SECTION 2.3 --
-- Characteristics of the Intended Reader automatically generated in introductionF

-- SECTION 2.4 --
-- Organization automatically generated in introductionF
orgSecStart, orgSecEnd :: Sentence
orgSecStart = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the", phrase template, S "for an",
  short srs, S "for", phrase sciCompS,
  S "proposed by Koothoor as well as Smith and Lai"]
orgSecEnd   = S "The" +:+ plural inModel +:+ S "provide the set of" +:+
  S "algebraic" +:+ plural equation +:+ S "that must be solved iteratively to perform a" +:+
  titleize morPrice +:+ titleize analysis

-- SECTION 3 --
s3 = genSysF [] userCharIntro [] []

-- SECTION 3.1 --
-- User Characteristics automatically generated in genSysF with the userContraints intro below
userCharIntro :: Contents
userCharIntro = foldlSP [S "The", phrase endUser, S "of",
  short ssa,
  S "should have an understanding of undergraduate Level 1 Calculus and",
  titleize physics `sC` S "and be familiar with", phrase soil,
  S "and", plural mtrlPrpty]

-- SECTION 3.2 --
-- System Constraints automatically generated in genSysF

-- SECTION 4 --
s4 = specSysDesF end [s4_1, s4_2]
  where end = plural definition +:+ S "and finally the" +:+
              plural inModel +:+ S "that" +:+ phrase model +:+
              S "the" +:+ phrase slope

-- SECTION 4.1 --
s4_1 = probDescF EmptyS ssa ending [s4_1_1, s4_1_2, s4_1_3]
  where ending = S "evaluate the" +:+ phrase fs_rc +:+ S "of a" +:+
                 phrase slope :+: S "'s" +:+ --FIXME apostrophe on "slope's"
                 phrase slpSrf +:+ S "and to calculate the displacement"
                 +:+ S "that the" +:+ phrase slope +:+ S "will experience"

-- SECTION 4.1.1 --
s4_1_1 = termDefnF EmptyS [s4_1_1_list]

s4_1_1_list = Enumeration $ Simple $ --FIXME: combine this definition below? But fs_rc already has a definition
  ([(titleize $ fs_rc, Flat $ S "Stability metric. How likely a" +:+
    (phrase slpSrf) +:+. S "is to experience failure through slipping")] ++
  map (\x -> (titleize $ x, Flat $ x ^. defn))
      [crtSlpSrf, stress, strain, normForce, shearForce, tension, compression, plnStrn])
      -- most of these are in concepts (physics or solidMechanics) except for crtSlpSrf & plnStrn which is in defs.hs

-- SECTION 4.1.2 --
s4_1_2 = SRS.physSyst [s4_1_2_p1, s4_1_2_bullets, s4_1_2_p2, s4_1_2_fig1, s4_1_2_fig2] []

s4_1_2_p1 = foldlSP [at_start analysis, S "of the", phrase slope,
  S "is performed by looking at", plural property, S "of the",
  phrase slope, S "as a series of", phrase slice +:+. plural element,
  S "Some", plural property, S "are", plural itslPrpty `sC`
  S "and some are", phrase slice, S "or", phrase slice,
  S "base" +:+. plural property, S "The index convention for referencing which",
  phrase intrslce, S "or", phrase slice, S "is being used is shown in",
  makeRef fig_indexconv]

s4_1_2_bullets = enumBullet [
  (at_start' itslPrpty +:+ S "convention is noted by j. The end" +:+
    plural itslPrpty +:+ S "are usually not of" +:+ phrase interest `sC`
    S "therefore use the" +:+ plural itslPrpty +:+ S "from" +:+
    S "1" +:+ P (Special LEQ) +:+ (E . V) "i" +:+ P (Special LEQ) +:+.
    (E $ (C numbSlices) :- Int 1)),--FIXME: this whole thing should be one expr
  (at_start slice +:+ plural property +:+. S "convention is noted by i")
  ]

s4_1_2_p2 = foldlSP [S "A", phrase fbd, S "of the", plural force,
  S "acting on the", phrase slice, S "is displayed in", makeRef fig_forceacting]

s4_1_2_fig1 = fig_indexconv

fig_indexconv :: Contents
fig_indexconv = Figure (S "Index convention for numbering" +:+
  phrase slice +:+ S "and" +:+ phrase intrslce +:+
  phrase force +:+ plural variable) "IndexConvention.png"

s4_1_2_fig2 = fig_forceacting

fig_forceacting :: Contents
fig_forceacting = Figure (at_start' force +:+ S "acting on a" +:+ (phrase slice))
  "ForceDiagram.png"

-- SECTION 4.1.3 --
s4_1_3 = goalStmtF (map (\(x, y) -> x `ofThe` y) [(S "geometry",
         S "water" +:+ phrase table_),
         (S "geometry", S "layers composing the plane of a" +:+ phrase slope),
         (plural mtrlPrpty, S "layers")]) [s4_1_3_list]

s4_1_3_list = enumSimple 1 (short goalStmt) [
  (S "Evaluate local and global" +:+ plural fs_rc +:+
      S "along a given" +:+. phrase slpSrf),
  (S "Identify the" +:+ phrase crtSlpSrf +:+ S "for the" +:+ phrase slope `sC`
      S "with the lowest" +:+. phrase fs_rc),
  (S "Determine" +:+. (S "displacement" `ofThe` phrase slope))
  ]

-- SECTION 4.2 --
s4_2 = solChSpecF ssa (s4_1, s6) True ddEnding (tbRef, EmptyS, True, EmptyS)
  ([s4_2_1_list], s4_2_2_tmods, s4_2_3_genDefs, s4_2_4_dataDefs, s4_2_5_p2:s4_2_5_p3:s4_2_5_IMods, [s4_2_6Table2, s4_2_6Table3]) []
  where ddEnding = (at_start' definition) +:+ S "DD1 to DD8 are the" +:+
          phrase force +:+ plural variable +:+ S "that can be solved by direct" +:+
          S "analysis of given" +:+. plural input_ +:+ S "The" +:+
          phrase intrslce +:+ S "forces DD9 are" +:+ phrase force +:+
          plural variable +:+. S "that must be written in terms of DD1 to DD8 to solve"
        tbRef = makeRef s4_2_6Table2 +:+ S "and" +:+ makeRef s4_2_6Table3 +:+ S "show"

-- SECTION 4.2.1 --
-- Assumptions is automatically generated in solChSpecF using the list below

s4_2_1_list = enumSimple 1 (short assumption) [
  (S "The" +:+ phrase slpSrf +:+ S "is concave with respect to" +:+
    S "the" +:+. phrase slopeSrf +:+ ((getS coords +:+
    S "coordinates") `ofThe'` S "failure") +:+ phrase surface +:+.
    S "follow a monotonic function"),
  (S "geometry" `ofThe'` phrase slope `sC` S "and" +:+
    (plural mtrlPrpty `ofThe` plural soilLyr) +:+.
    S "are given as inputs"),
  (S "different layers" `ofThe'` phrase soil +:+ S "are homogeneous" `sC`
    S "with consistent" +:+ plural soilPrpty +:+ S "throughout" `sC`
    S "and independent of dry or saturated" +:+ plural condition `sC`
    S "with the exception of" +:+ phrase unit_ +:+. S "weight"),
  (at_start' soilLyr +:+ S "are treated as if they have" +:+.
    S "isotropic properties"),
  (at_start intrslce +:+ S "normal and" +:+ plural shearForce +:+ S "have a" +:+
    S "linear relationship, proportional to a constant" +:+
    sParen (getS normToShear) +:+ S "and an" +:+
    phrase intrslce +:+ phrase force +:+ S "function" +:+ sParen (getS scalFunc) +:+.
    S "depending on x position"),
  (at_start slice +:+ S "to base normal and" +:+ plural shearForce +:+ S "have" +:+
    S "a linear relationship, dependent on the" +:+
    phrase fs_rc +:+ sParen (getS fs) `sC`
    S "and the Coulomb sliding law."),
  (S "The" +:+ phrase stress :+: S "-" :+: phrase strain +:+ S "curve for" +:+ --FIXME: add hypens to drasil language
    phrase intrslce +:+ S "relationships is linear with a constant" +:+.
    phrase slope),
  (S "The" +:+ phrase slope +:+ S "and" +:+ phrase slpSrf +:+.
    S "extends far into and out of the geometry (z coordinate)" +:+
    S "This implies plane" +:+ phrase strain +:+ plural condition `sC`
    S "making 2D analysis appropriate."),
  (S "The effective normal" +:+ phrase stress +:+ S "is large enough" +:+
    S "that the resistive shear to effective normal" +:+
    phrase stress +:+ S "relationship can be approximated as a" +:+.
    S "linear relationship"),
  (S "The" +:+ phrase surface +:+ S "and base of a" +:+
    phrase slice +:+ S "between" +:+ phrase intrslce +:+.
    S "nodes are approximated as straight lines")
  ]

-- SECTION 4.2.2 --
-- TModels is automatically generated in solChSpecF using the tmods below

s4_2_2_tmods = map sspSymMapT sspTMods --FIX fs_rc to use lowercase

-- SECTION 4.2.3 --
-- General Definitions is automatically generated in solChSpecF
s4_2_3_genDefs = map sspSymMapT sspGenDefs

-- SECTION 4.2.4 --
-- Data Definitions is automatically generated in solChSpecF
s4_2_4_dataDefs = (map sspSymMapD (take 10 sspDataDefs)) ++ resShrDerivation ++
  [sspSymMapD (sspDataDefs !! 10)] ++ mobShrDerivation ++ [sspSymMapD (sspDataDefs !! 11)] ++
  stfMtrxDerivation ++ (map sspSymMapD (drop 12 sspDataDefs)) --FIXME: is there a better way of shoving these derivations in the middle of the Data Defs?

resShrDerivation :: [Contents]
resShrDerivation = [foldlSP [S "The resistive shear force of a slice is defined as Pi in GD3. The effective normal in the equation",
  S "for Pi of the soil is defined in the perpendicular force equilibrium of a slice from GD2, Using the",
  S "effective normal N0i of T4 shown in equation (1)"],
  
  EqnBlock $
  (C nrmFSubWat) := (((C slcWght) - (C intShrForce) + (C intShrForce) :+ (C surfHydroForce) :* (cos (C surfAngle)) :+ --FIXME: add indexing
  (C surfLoad) :* (cos (C impLoadAngle))) :* (cos (C baseAngle)) :+
  (Neg (C earthqkLoadFctr) :* (C slcWght) - (C intNormForce) + (C intNormForce) - (C watrForce) + (C watrForce) :+ (C surfHydroForce)
  :* sin (C surfAngle) :+ (C surfLoad) :* (sin (C impLoadAngle))) :* (sin (C baseAngle)) :- (C baseHydroForce)),
  
  foldlSP [S "The values of the interslice forces E and X in the equation are unknown, while the other values",
  S "are found from the physical force definitions of DD1 to DD9. Consider a force equilibrium without",
  S "the affect of interslice forces, to obtain a solvable value as done for N*i in equation (2)"],

  EqnBlock $
  (C nrmFNoIntsl) := (((C slcWght) :+ (C surfHydroForce) :* (cos (C surfAngle)) :+ 
  (C surfLoad) :* (cos (C impLoadAngle))) :* (cos (C baseAngle)) :+
  (Neg (C earthqkLoadFctr) :* (C slcWght) - (C watrForce) + (C watrForce) :+ (C surfHydroForce)
  :* sin (C surfAngle) :+ (C surfLoad) :* (sin (C impLoadAngle))) :* (sin (C baseAngle)) :- (C baseHydroForce)),
  
  foldlSP [S "Using N*i, a resistive shear force neglecting the influence of interslice forces can be solved for in",
  S "terms of all known values as done in equation (3)"],
  
  EqnBlock $
  C shearRNoIntsl := (C nrmFNoIntsl) * tan (C fricAngle) + (C cohesion) * (C baseWthX) * sec (C baseAngle),
  
  EqnBlock $
  C shearRNoIntsl := (((C slcWght) :+ (C surfHydroForce) :* (cos (C surfAngle)) :+ 
  (C surfLoad) :* (cos (C impLoadAngle))) :* (cos (C baseAngle)) :+
  (Neg (C earthqkLoadFctr) :* (C slcWght) :- (C watrForceDif) :+ (C surfHydroForce)
  :* sin (C surfAngle) :+ (C surfLoad) :* (sin (C impLoadAngle))) :* (sin (C baseAngle)) :- (C baseHydroForce)) :*
  tan (C fricAngle) :+ (C cohesion) :* (C baseWthX) :* sec (C baseAngle)
  
  ]

mobShrDerivation :: [Contents]
mobShrDerivation = [foldlSP [S "The mobile shear force acting on a slice is defined",
  S "as Si from the force equilibrium in GD2, also shown in equation (4)"],
  
  EqnBlock $
  (C nrmFSubWat) := (((C slcWght) - (C intShrForce) + (C intShrForce) :+ (C surfHydroForce) :* (cos (C surfAngle)) :+ --FIXME: add indexing
  (C surfLoad) :* (cos (C impLoadAngle))) :* (sin (C baseAngle)) :-
  (Neg (C earthqkLoadFctr) :* (C slcWght) - (C intNormForce) + (C intNormForce) - (C watrForce) + (C watrForce) :+ (C surfHydroForce)
  :* sin (C surfAngle) :+ (C surfLoad) :* (sin (C impLoadAngle))) :* (cos (C baseAngle))),
  
  foldlSP [S "The equation is unsolvable, containing the unknown interslice normal force E and shear force X.",
  S "Consider a force equilibrium without the affect of interslice forces, to obtain the mobile shear force",
  S "without the influence of interslice forces T, as done in equation (5)"],
  
  EqnBlock $
  C shearFNoIntsl := ((C slcWght) :+ (C surfHydroForce) :* (cos (C surfAngle)) :+ 
  (C surfLoad) :* (cos (C impLoadAngle))) :* (sin (C baseAngle)) :- 
  (Neg (C earthqkLoadFctr) :* (C slcWght) :- (C watrForceDif) :+ (C surfHydroForce)
  :* sin (C surfAngle) :+ (C surfLoad) :* (sin (C impLoadAngle))) :* (cos (C baseAngle)),
  
  foldlSP [S "The values of Ri and Ti are now defined completely in terms of the known force property values",
  S "of DD1 to DD9"]
  ]

stfMtrxDerivation :: [Contents]
stfMtrxDerivation = [foldlSP [S "Using the force-displacement relationship of GD8",
  S "to define stiffness matrix Ki, as seen in equation (6)"],

  foldlSP [S "For interslice surfaces the stiffness constants and displacements refer to an unrotated coordinate",
  S "system,", S "delta" , S "i of GD9. The interslice elements are left in their standard coordinate system, and",
  S "therefore are described by the same equation from GD8. Seen as Ks,i in DD12. Kst,i is the shear",
  S "element in the matrix, and Ksn,i is the normal element in the matrix, calculated as in DD14"],
  
  foldlSP [S "For basal surfaces the stiffness constants and displacements refer to a system rotated for the base",
  S "angle alpha (DD5). To analyze the effect of force-displacement relationships occurring on both basal",
  S "and interslice surfaces of an element i they must reference the same coordinate system. The basal",
  S "stiffness matrix must be rotated counter clockwise to align with the angle of the basal surface.",
  S "The base stiffness counter clockwise rotation is applied in equation (7) to the new matrix K*i"],
  
  foldlSP [S "The Hooke's law force displacement relationship of GD8 applied to the base also references a",
  S "displacement vector epsilon i of GD9 rotated for the base angle of the slice alpha i. The basal displacement",
  S "vector delta i is rotated clockwise to align with the interslice displacement vector delta i, applying the",
  S "definition of epsilon i in terms of delta i as seen in GD9. Using this with base stiffness matrix K*i, a basal",
  S "force displacement relationship in the same coordinate system as the interslice relationship can be",
  S "derived as done in equation (8)"],
  
  foldlSP [S "The new effective base stiffness matrix K0i ,as derived in equation (7) is defined in equation (9). This",
  S "is seen as matrix Kb,i in GD12. Kbt,i is the shear element in the matrix, and Kbn,i is the normal",
  S "element in the matrix, calculated as in DD14. The notation is simplified by the introduction of",
  S "the constants KbA,i and KbB,i, defined in equations (10) and (11) respectively"],
  
  EqnBlock $
  (C shrStiffBase) := (C shrStiffBase) * (cos (C baseAngle)) :^ (Int 2) :+ --FIXME: the first symbol should be K_(bA,i), waiting on indexing
  (C nrmStiffBase) * (sin (C baseAngle)) :^ (Int 2),
  
  EqnBlock $
  (C shrStiffBase) := ((C shrStiffBase)-(C nrmStiffBase)) * --FIXME: the first symbol should be K_(bB,i), waiting on indexing
  (sin (C baseAngle)) * (cos (C baseAngle)),
  
  foldlSP [S "A force-displacement relationship for an element i can be written in terms of displacements occurring",
  S "in the unrotated coordinate system delta i of GD9 using the matrix Ks,i, and Kb,i as seen in",
  S "DD12"]
  ]

-- SECTION 4.2.5 --
-- Instance Models is automatically generated in solChSpecF using the paragraphs below

s4_2_5_p2 = foldlSP [S "The", titleize morPrice,
  phrase method_, S "is a vertical", phrase slice `sC` S "limit equilibrium",
  phrase ssa +:+. phrase method_, at_start analysis, S "is performed by",
  S "breaking the", S "assumed failure", phrase surface, S "into a series of vertical",
  plural slice, S "of" +:+. phrase mass, S "Static equilibrium",
  S "analysis using two", phrase force, S "equilibrium, and one moment",
  phrase equation, S "as in" +:+. acroT "2", S "The", phrase problem,
  S "is statically indeterminate with only these 3", plural equation, S "and one", --FIXME: T2, T3, GD5, DD1, DD9, DD10, DD11 should be references to other things in the body
  S "constitutive", phrase equation, sParen $ S "the Mohr Coulomb shear strength of" +:+ acroT "3",
  S "so the", phrase assumption, S "of", acroGD "5", S "is used. Solving for",
  phrase force, S "equilibrium allows", plural definition, S "of all", plural force,
  S "in terms of the", plural physicalProperty, S "of", acroDD "1", S "to",
  acroDD "9" `sC` S "as done in", acroDD "10" `sC` acroDD "11"]

s4_2_5_p3 = foldlSP [plural value `ofThe'` (phrase intrslce +:+ phrase normForce),
  S "E the interslice normal/shear", phrase force, S "magnitude ratio", getS normToShear `sC` --FIXME: 'E' should be the symbol captital E, same with lambda
  S "and the", titleize fs_rc, (sParen $ getS fs) `sC` S "are unknown.",  --FIXME: get the relation concept symbol 'FS' from factor of safety in Defs.hs
  at_start' equation, S "for the unknowns are written in terms of only the",
  plural value, S "in", acroDD "1", S "to", acroDD "9" `sC` S "the", plural value, S "of", getS shearRNoIntsl `sC`
  S "and", getS shearFNoIntsl, S "in", acroDD "10", S "and", acroDD "11" `sC`
  S "and each", --FIXME: DD10, DD11 should be references to other things in the body
  S "other. The relationships between the unknowns are non linear" `sC`
  S "and therefore explicit", plural equation, S "cannot be derived and an",
  S "iterative", plural solution, S "method is required"]

s4_2_5_IMods = concat $ weave [map (\x -> [sspSymMapT x]) sspIMods, --FIXME: ? is there a better way of doing this?
  [fctSftyDerivation, nrmShrDerivation, intrSlcDerivation,
  rigDisDerivation, rigFoSDerivation]]

fctSftyDerivation, nrmShrDerivation, intrSlcDerivation,
  rigDisDerivation, rigFoSDerivation :: [Contents]

fctSftyDerivation = [foldlSP [S "Using equation (21) from section 4.2.5, rearranging, and applying the boundary condition that E0",
  S "and En are equal to 0 an equation for the factor of safety is found as equation (12), also seen in",
  S "IM1. Using equation (21) from section 4.2.5, rearranging, and applying the boundary condition that E0",
  S "and En are equal to 0 an equation for the factor of safety is found as equation (12), also seen in",
  S "IM1"],
  
  EqnBlock fcSfty_rel,
  
  foldlSP [S "The constants Psi and Phi described in equations 20 and 19 are functions of the unknowns: the",
  S "interslice normal/shear force ratio", getS normToShear, S "(IM2) and the Factor of Safety itself FS (IM1)"]
  ]

nrmShrDerivation = [foldlSP [S "The last static equation of T2 the moment equilibrium of GD6 about the midpoint of the base is",
  S "taken, with the assumption of GD5. Results in equation (13)"],
  
  EqnBlock momEql_rel, --FIXME: this is not *exactly* the equation but very similar
  --Need more simbols (z) to finish
  
  foldlSP [S "The equation in terms of", getS normToShear, S "leads to equation (14)"],
  
  foldlSP [S "Taking a summation of each slice, and considering the boundary conditions that E0 and En are",
  S "equal to zero, a general equation for the constant", getS normToShear, S "is developed in equation (15), also found in",
  S "IM2"],
  
  foldlSP [S "Equation (15) for", getS normToShear `sC` S "is a function of the unknown interslice normal force E (IM3)"]
  ]

intrSlcDerivation = [foldlSP [S "Taking the perpendicular force equilibrium of GD1 with the effective stress definition from T4",
  S "that", E (C totNrmForce := C nrmFSubWat - C baseHydroForce) `sC` S "and the assumption of GD5 the equilibrium equation can be rewritten as",
  S "equation (16)"],
  
  EqnBlock $
  C nrmFSubWat := ((C slcWght :- C normToShear :* C scalFunc :* C intNormForce :+ 
  C normToShear :* C scalFunc :* C intNormForce :+ 
  C baseHydroForce :* cos (C surfAngle) :+ C surfLoad :* 
  cos (C impLoadAngle)) :* cos (C baseAngle)
  :+ (Neg (C earthqkLoadFctr) :* C slcWght :- 
  C intNormForce :+ C intNormForce :- C watrForce :+ 
  C watrForce :+ C surfHydroForce :* sin (C surfAngle) :+ 
  C surfLoad :* cos (C impLoadAngle)) :* sin (C baseAngle)),
  
  foldlSP [S "Taking the parallel force equilibrium of GD2 with the definition of mobile shear from GD4 and",
  S "the assumption of GD5, the equilibrium equation can be rewritten as equation (17)"],
  
  foldlSP [S "Substituting the equation for N0 i from equation (16) into equation (17) and rearranging results in",
  S "equation (18)"],

  foldlSP [S "Where Ri and Ti are the resistive and mobile shear of the slice, without the influence of interslice",
  S "forces E and X, as defined in DD10 and DD11. Making use of the constants, and with full",
  S "equations found below in equations (19) and (20) respectively, then equation (18) can be simplified",
  S "to equation (21), also seen in IM3"],
  
  foldlSP [S "The constants Psi and Phi in equation (21) for Ei is a function of the unknown values, the interslice",
  S "normal/shear force ratio", getS normToShear, S "(IM2), and the Factor of Safety FS (IM1)"]
  ]

rigDisDerivation = [foldlSP [S "Using the net force-displacement equilibrium equation of a slice from DD13, with the definitions",
  S "of the stiffness matrices from DD12, and the force definitions from GD7, a broken down forcedisplacement",
  S "equilibrium equation can be derived. Equation (22) gives the broken down equation",
  S "in the x direction, and equation (23) gives the broken down equation in the y direction."],

  foldlSP [S "Using the known input assumption of A2, the force variable definitions of DD1 to DD8 on the left",
  S "side of the equations can be solved for. The only unknown in the variables to solve for the stiffness",
  S "values from DD14 is the displacements. Therefore taking the equation from each slice a set of", E $ (Int 2) * (C numbSlices),
  S "equations, with", E $ (Int 2) * (C numbSlices), S "unknown displacements in the x and y directions of each slice can be derived.",
  S "Solutions for the displacements of each slice can then be found. The use of displacement in the",
  S "definition of the stiffness values makes the equation implicit, which means an iterative solution",
  S "method, with an initial guess for the displacements in the stiffness values is required"]
  ]

rigFoSDerivation = [foldlSP [S "RFEM analysis can also be used to calculate the Factor of safety for the slope. For a slice element",
  S "i the displacements", getS dx_i, S "and", getS dy_i `sC` S "are solved from the system of equations in IM4. The definition of",
  S "epsilon i as the rotation of the displacement vector delta i is seen in GD9. This is used to find the displacements of the slice parallel to the base of the slice delta u in equation",
  S "(24) and normal to the base of the slice delta v in equation (25)"],
  
  foldlSP [S "With the definition of normal stiffness from DD14 to find the normal stiffness of the base Kbn,i,",
  S "and the now known base displacement perpendicular to the surface delta vi from equation (25), the",
  S "normal base stress can be calculated from the force-displacement relationship of T5. Stress sigma is",
  S "used in place of force F as the stiffness hasn't been normalized for the length of the base. Results",
  S "in equation (26)"],

  foldlSP [S "The resistive shear to calculate the factor of safety FS in is found from the Mohr Coulomb resistive",
  S "strength of soil in T3. Using the normal stress sigma from equation (26) as the stress the resistive",
  S "shear of the slice can be calculated from calculated in equation (27)"],
  
  foldlSP [S "Previously the value of the base shear stiffness Kbt,i as seen in equation (28) was unsolvable because",
  S "the normal stress sigma i was unknown. With the definition of sigma i from equation (26) and the definition",
  S "of displacement shear to the base delta ui from equation (25), the value of Kbt,i becomes solvable"],
  
  foldlSP [S "With shear stiffness Kbt,i calculated in equation (28) and shear displacement delta ui calculated in",
  S "equation (24) values now known the shear stress acting on the base of a slice tau can be calculated",
  S "using T5, as done in equation (29). Again stress tau is used in place of force F as the stiffness hasn't",
  S "been normalized for the length of the base"],
  
  foldlSP [S "The shear stress on the base tau acts as the mobile shear acting on the base. Using the definition",
  S "Factor of Safety equation from T1, with the definitions of resistive shear strength of a slice Si",
  S "from equation (27) and mobile shear on a slice tau from equation (29) the factor of safety for a slice",
  S "FSLoc,i can be found from as seen in equation (30), and IM5"],
  
  foldlSP [S "The global Factor of Safety is then the ratio of the summation of the resistive and mobile shears",
  S "for each slice, with a weighting for the length of the slices base. Shown in equation (31), and IM5"]
  ]

-- SECTION 4.2.6 --
-- Data Constraints is automatically generated in solChSpecF using the tables below
{-input data-}
noTypicalVal, vertConvention :: Sentence
noTypicalVal   = short notApp
vertConvention = S "Consecutive vertexes have increasing x" +:+. plural value +:+
  S "The start and end vertices of all layers go to the same x" +:+. plural value

vertVar :: Sentence -> Sentence
vertVar vertexType = getS coords +:+ S "of" +:+ vertexType +:+ S "vertices'"

verticesConst :: Sentence -> [Sentence]
verticesConst vertexType = [vertVar vertexType, vertConvention, noTypicalVal]

waterVert, slipVert, slopeVert :: [Sentence]
waterVert = verticesConst $ S "water" +:+ phrase table_
slipVert  = verticesConst $ phrase slip
slopeVert = verticesConst $ phrase slope

dataConstIn :: [[Sentence]]
dataConstIn = [waterVert, slipVert, slopeVert] ++ --List of typical values below
  zipWith makeConstraint sspInputs (map S ["15000","10","0.4","25","20","20","9.8"])

{-output data-}
slipVert2 :: [[Sentence]]
slipVert2 = [[vertVar $ phrase slip, S "Vertices's monotonic"]]

displayContr' :: (Constrained s, SymbolForm s) => s -> [Sentence]
displayContr' s = init $ makeConstraint s EmptyS

dataConstOut :: [[Sentence]]
dataConstOut = [(displayContr' . head) sspOutputs] ++ slipVert2 ++
  map displayContr' (tail sspOutputs)

{-input and output tables-}
s4_2_6Table2, s4_2_6Table3 :: Contents
s4_2_6Table2 = inDataConstTbl dataConstIn "2"
s4_2_6Table3 = outDataConstTbl dataConstOut "3"

-- SECTION 5 --
s5 = reqF [s5_1, s5_2]

-- SECTION 5.1 --
s5_1 = SRS.funcReq
  [s5_1_list, s5_1_table] []

s5_1_list = enumSimple 1 (short requirement) [
  (S "Read the" +:+ phrase input_ +:+ S "file, and store the" +:+. plural datum +:+
    S "Necessary" +:+ plural inDatum +:+ S "summarized in" +:+.
    makeRef s5_1_table),
  (S "Generate potential" +:+ phrase crtSlpSrf :+:
    S "'s for the" +:+ phrase input_ +:+. phrase slope),
  (S "Test the" +:+ plural slpSrf +:+ S "to determine" +:+
    S "if they are physically realizable based" +:+.
    S "on a set of pass or fail criteria"),
  (S "Prepare the" +:+ plural slpSrf +:+ S "for a" +:+ phrase method_ +:+
    S "of" +:+ plural slice +:+. S "or limit equilibrium analysis"),
  (S "Calculate" +:+. (plural fs_rc `ofThe` plural slpSrf)),
  (S "Rank and weight the" +:+ plural slope +:+ S "based on their" +:+
    phrase fs_rc `sC` S "such that a" +:+ phrase slpSrf +:+
    S "with a smaller" +:+ phrase fs_rc +:+.
    S "has a larger weighting"),
  (S "Generate new potential" +:+ plural crtSlpSrf +:+
    S "based on previously analysed" +:+ plural slpSrf +:+
    S "with low" +:+. plural fs_rc),
  (S "Repeat" +:+ plural requirement +:+ acroR "3" +:+ S "to" +:+
    acroR "7" +:+ S "until the" +:+
    S "minimum" +:+ phrase fs_rc +:+ S "remains approximately" +:+
    S "the same over a predetermined number of" +:+
    S "repetitions. Identify the" +:+ (phrase slpSrf) +:+
    S "that generates the minimum" +:+ phrase fs_rc +:+
    S "as the" +:+. phrase crtSlpSrf),
  (S "Prepare the" +:+ phrase crtSlpSrf +:+ S "for" +:+ phrase method_ +:+
    S "of" +:+ plural slice +:+. S "or limit equilibrium analysis"),
  (S "Calculate" +:+ (phrase fs_rc `ofThe` phrase crtSlpSrf) +:+
    S "using the" +:+ titleize morPrice +:+. phrase method_),
  (S "Display the" +:+ phrase crtSlpSrf +:+ S "and the" +:+
    phrase slice +:+ phrase element +:+.
    S "displacements graphically" +:+ S "Give" +:+
    (plural value `ofThe` plural fs_rc) +:+ S "calculated" +:+
    S "by the" +:+ titleize morPrice +:+. phrase method_)
  ]

s5_1_table = mkInputDatTb ([cqs coords] ++ --this has to be seperate since coords is a different type
  map cqs [elasticMod, cohesion, poissnsRatio, fricAngle, dryWeight, satWeight, waterWeight])

-- SECTION 5.2 --
s5_2 = nonFuncReqF [accuracy, performanceSpd]
  [correctness, understandability, reusability, maintainability] r EmptyS
  where r = (short ssa) +:+ S "is intended to be an educational tool"

-- SECTION 6 --
s6 = SRS.likeChg [] []

-- References --
s7 = SRS.reference [s7_list] []

s7_list = mkRefsList 1 [ --FIXME: names should be in italics
  S "Q.H. Qian D.Y. Zhu, C.F. Lee and G.R. Chen. A concise algorithm for computing" +:+
            S "the factor of safety using the morgensternprice method. Can. Geotech. J.," +:+.
            S "(42):272-278, 19 February 2005",
  S "D.G. Fredlund and J.Krahn. Comparison of slope stability methods of" +:+.
            phrase analysis +:+. S "Can. Geotech. J., (14):429-439, 4 April 1977",
  S "Nirmitha Koothoor. A document drive approach to certifying" +:+.
            phrase sciCompS +:+ S "Master's thesis, McMaster University," +:+.
            S "Hamilton, Ontario, Canada, 2013",
  S "David L. Parnas and P.C. Clements. A rational design process: How" +:+
            S "and why to fake it. IEEE Transactions on Software Engineering," +:+.
            S "12(2):251-257, February 1986",
  S "W. Spencer Smith and Lei Lai. A new requirements template for" +:+
            S "scientific computing. In J. Ralyt" :+: (F Acute 'e') `sC` S "P. Agerfalk, and N. Kraiem" `sC`
            S "editors, Proceedings of the First International Workshopon" +:+
            S "Situational Requirements Engineering Processes - Methods," +:+
            S "Techniques and Tools to Support Situation-Specific Requirements" +:+
            S "Engineering Processes, SREP'05, pages 107-121, Paris, France" `sC`
            S "2005. In conjunction with 13th IEEE International Requirements" +:+.
            S "Engineering Conference",
  S "Dieter Stolle and Peijun Guo. Limit equilibrum" +:+ phrase ssa +:+.
            S "using rigid finite elements. Can. Geotech. J., (45):653-662, 20 May 2008",
  S "Tony L.T Zhan Dao-Sheng Ling Yu-Chao Li, Yun-Min Chen and" +:+
            S "Peter John Cleall. An efficient approach for locating the" +:+
            phrase crtSlpSrf +:+ S "in" +:+ plural ssa +:+ S "using a" +:+
            S "real-coded genetic algorithm. Can. Geotech. J., (47):806-820," +:+.
            S "25 June 2010"]