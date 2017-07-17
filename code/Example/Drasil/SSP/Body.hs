module Drasil.SSP.Body where

import Control.Lens ((^.))
import Prelude hiding (id, sin, cos, tan)

import Language.Drasil
import Data.Drasil.SI_Units
import Data.Drasil.Authors

import Drasil.SSP.Assumptions
import Drasil.SSP.Changes
import Drasil.SSP.DataDefs
import Drasil.SSP.Defs
import Drasil.SSP.GenDefs
import Drasil.SSP.IMods
import Drasil.SSP.Modules
import Drasil.SSP.Reqs
import Drasil.SSP.Requirements
import Drasil.SSP.TMods
import Drasil.SSP.Unitals
import qualified Drasil.SRS as SRS

import Drasil.Sections.ReferenceMaterial
import Drasil.DocumentLanguage
import Drasil.Sections.SpecificSystemDescription
import Drasil.Sections.Requirements
import Drasil.Sections.GeneralSystDesc
import Drasil.Sections.AuxiliaryConstants

import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Physics
import Data.Drasil.Concepts.PhysicalProperties
import Data.Drasil.Concepts.Education
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Math hiding (constraint)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)
import Data.Drasil.Software.Products

import Data.Drasil.Utils
import Data.Drasil.SentenceStructures

import Drasil.Template.MG
import Drasil.Template.DD

--type declarations for sections--
s3, s4, s5, s6, s7, s8 :: Section

s1_2_intro :: [TSIntro]

s4_1, s4_1_1, s4_1_2,
  s4_1_3, s4_2, s5_1, s5_2 :: Section

s4_1_1_list, s4_1_2_p1, s4_1_2_bullets,
  s4_1_2_p2, s4_1_3_list, s4_2_1_list,
  s4_2_5_p2, s4_2_5_p3, s5_1_list,
  s8_list :: Contents

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
    , IChar (phrase solidMechanics) 
      (phrase undergraduate +:+ S "level 4" +:+ phrase physics)
      EmptyS
    , IOrgSec orgSecStart inModel (SRS.inModel SRS.missingP []) orgSecEnd]) :
    --FIXME: SRS.inModel should be removed and the instance model section
    --should be looked up from "inModel" by the interpreter while generating.
  map Verbatim [s3, s4, s5, s6, s7, s8]

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
  plural value +:+ S "with a subscript" +:+ getS index +:+ S "implies that the"
  +:+ phrase value +:+ S "will be taken at and analyzed at a" +:+ phrase slice
  `sOr` phrase slice +:+ S "interface composing the total slip" +:+ phrase mass]]

-- SECTION 1.3 --
--automatically generated in mkSRS

-- SECTION 2 --
startIntro, kSent :: Sentence
startIntro = foldlSent [S "A", phrase slope, S "of geological",
  phrase mass `sC` S "composed of", phrase soil, S "and rock, is subject", 
  S "to the influence of gravity on the" +:+. phrase mass, S "For an unstable",
  phrase slope +:+. S "this can cause instability in the form of soil/rock movement",
  S "The", plural effect, S "of soil/rock movement can range from inconvenient to",
  S "seriously hazardous, resulting in signifcant life and economic" +:+. plural loss,
  at_start slope, S "stability is of", phrase interest, S "both when analyzing", 
  S "natural", plural slope `sC` S "and when designing an excavated" +:+. 
  phrase slope, at_start ssa, S "is", (S "assessment" `ofThe` S "safety of a"),
  phrase slope `sC` S "identifying the", phrase surface,
  S "most likely to experience slip" `sAnd` S "an index of its relative stability", 
  S "known as the", phrase fs]
kSent = keySent ssa

keySent :: (NamedIdea a) => a -> Sentence
keySent pname = S "a" +:+ phrase pname +:+. phrase problem +:+ S "The developed"
  +:+ phrase program +:+ S "will be referred to as the" +:+ introduceAbb pname +:+
  phrase program
  
-- SECTION 2.1 --
-- Purpose of Document automatically generated in introductionF
prpsOfDoc_p1 :: Sentence
prpsOfDoc_p1 = purposeDoc ssa crtSlpSrf fs how introduces analysizes
  where how = S "assessing the stability of a" +:+ phrase slope +:+ phrase design
        introduces = phrase slope +:+ S "stability" +:+ plural issue
        analysizes = S "safe" +:+ phrase slope

purposeDoc :: (NamedIdea a, NamedIdea b, NamedIdea c) =>
              a -> b -> c -> Sentence -> Sentence -> Sentence
              -> Sentence
purposeDoc pname what calculates how introduces analysizes =
  foldlSent [S "The", short pname, phrase program,
  S "determines the", phrase what `sC` S "and its respective",
  phrase calculates, S "as a", phrase method_,
  S "of" +:+. how, S "The", phrase program,
  S "is intended to be used as an educational tool for",
  S "introducing", introduces `sC` S "and will facilitate the",
  phrase analysis `sAnd` phrase design, S "of a", analysizes]

-- SECTION 2.2 --
-- Scope of Requirements automatically generated in introductionF
scpIncl, scpEnd :: Sentence
scpIncl = S "stability analysis of a 2 dimensional" +:+ phrase slope `sC`
  S "composed of homogeneous" +:+ plural soilLyr
scpEnd  = S "identify the most likely failure" +:+
  phrase surface +:+ S "within the possible" +:+ phrase input_ +:+ 
  S "range" `sC` S "and find the" +:+ phrase fs_rc +:+ S "for the" +:+
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
  S "algebraic" +:+ plural equation +:+ S "that must be solved iteratively"
  +:+ S "to perform a" +:+ titleize morPrice +:+ titleize analysis

-- SECTION 3 --
s3 = genSysF [] userCharIntro [] []

-- SECTION 3.1 --
-- User Characteristics automatically generated in genSysF with the userContraints intro below
userCharIntro :: Contents
userCharIntro = userChar ssa [S "Calculus", titleize physics]
  [phrase soil, plural mtrlPrpty]

userChar :: (NamedIdea a) => a -> [Sentence] -> [Sentence] -> Contents
userChar pname understandings familiarities = foldlSP [
  S "The", phrase endUser, S "of", short pname,
  S "should have an understanding of undergraduate Level 1",
  foldlList understandings `sC`
  S "and be familiar with", foldlList familiarities]

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
                 phrase's slope +:+ --FIXME apostrophe on "slope's"
                 phrase slpSrf +:+ S "and to calculate the displacement"
                 +:+ S "that the" +:+ phrase slope +:+ S "will experience"

-- SECTION 4.1.1 --
s4_1_1 = termDefnF EmptyS [s4_1_1_list]

s4_1_1_list = Enumeration $ Simple $
  map (\x -> (titleize $ x, Flat $ x ^. defn))
  [fs_concept, crtSlpSrf, stress, strain, normForce,
  shearForce, tension, compression, plnStrn]
  -- most of these are in concepts (physics or solidMechanics)
  -- except for crtSlpSrf & plnStrn which is in defs.hs
  -- and fs which is in Unitals.hs

-- SECTION 4.1.2 --
s4_1_2 = SRS.physSyst
  [s4_1_2_p1, s4_1_2_bullets, s4_1_2_p2, fig_indexconv, fig_forceacting] []

s4_1_2_p1 = physSystIntro slope how intrslce slice (S "slice base") fig_indexconv
  where how = S "as a series of" +:+ phrase slice +:+. plural element

physSystIntro :: (NamedIdea a, NamedIdea b, NamedIdea c, LayoutObj d) =>
  a -> Sentence -> b -> c -> Sentence -> d -> Contents
physSystIntro what how p1 p2 p3 indexref = foldlSP [
  at_start analysis, S "of the", phrase what, S "is performed by looking at",
  plural property, S "of the", phrase what, how, S "Some", plural property,
  S "are", phrase p1, plural property `sC` S "and some are", phrase p2 `sOr`
  p3 +:+. plural property, S "The index convention for referencing which",
  phrase p1 `sOr` phrase p2, S "is being used is shown in", makeRef indexref]

s4_1_2_bullets = enumBullet [
  (at_start' itslPrpty +:+ S "convention is noted by j. The end" +:+
    plural itslPrpty +:+ S "are usually not of" +:+ phrase interest `sC`
    S "therefore use the" +:+ plural itslPrpty +:+ S "from" +:+ 
    (E $ Int 1 :<= C index :<= (C numbSlices) :- Int 1)),
  (at_start slice +:+ plural property +:+. S "convention is noted by"
  +:+ getS index)
  ]

s4_1_2_p2 = foldlSP [S "A", phrase fbd, S "of the", plural force,
  S "acting on the", phrase slice, S "is displayed in", makeRef fig_forceacting]

fig_indexconv :: Contents
fig_indexconv = Figure (S "Index convention for numbering" +:+
  phrase slice `sAnd` phrase intrslce +:+
  phrase force +:+ plural variable) "IndexConvention.png"

fig_forceacting :: Contents
fig_forceacting = Figure (at_start' force +:+ S "acting on a" +:+ (phrase slice))
  "ForceDiagram.png"

-- SECTION 4.1.3 --
s4_1_3 = goalStmtF (map (\(x, y) -> x `ofThe` y) [
  (S "geometry", S "water" +:+ phrase table_),
  (S "geometry", S "layers composing the plane of a" +:+ phrase slope),
  (plural mtrlPrpty, S "layers")
  ]) [s4_1_3_list]

s4_1_3_list = enumSimple 1 (short goalStmt) sspGoals

sspGoals :: [Sentence]
sspGoals = [locAndGlFS, lowestFS, displSlope]

locAndGlFS, lowestFS, displSlope :: Sentence
locAndGlFS = S "Evaluate local and global" +:+ plural fs_rc +:+
  S "along a given" +:+. phrase slpSrf
lowestFS   = S "Identify the" +:+ phrase crtSlpSrf +:+ S "for the" +:+
  phrase slope `sC` S "with the lowest" +:+. phrase fs_rc
displSlope = S "Determine" +:+. (S "displacement" `ofThe` phrase slope)

-- SECTION 4.2 --
s4_2 = solChSpecF ssa (s4_1, s6) ddEnding (EmptyS, dataConstraintUncertainty, EmptyS)
  ([s4_2_1_list], s4_2_2_tmods, s4_2_3_genDefs, s4_2_4_dataDefs, 
  s4_2_5_p2:s4_2_5_p3:s4_2_5_IMods, [s4_2_6Table2, s4_2_6Table3]) []
  where ddEnding = foldlSent [at_start' definition, acroDD 1, S "to", acroDD 8,
          S "are the", phrase force, plural variable, S "that can be solved by",
          S "direct analysis of given" +:+. plural input_, S "The", 
          phrase intrslce, S "forces", acroDD 9, S "are", phrase force,
          plural variable, S "that must be written in terms of", acroDD 1, 
          S "to", acroDD 8, S "to solve"]

-- SECTION 4.2.1 --
-- Assumptions is automatically generated in solChSpecF using the list below

s4_2_1_list = enumSimple 1 (short assumption) sspAssumptions

-- SECTION 4.2.2 --
-- TModels is automatically generated in solChSpecF using the tmods below

s4_2_2_tmods = map sspSymMapT sspTMods

-- SECTION 4.2.3 --
-- General Definitions is automatically generated in solChSpecF
s4_2_3_genDefs = map sspSymMapT sspGenDefs

-- SECTION 4.2.4 --
-- Data Definitions is automatically generated in solChSpecF
s4_2_4_dataDefs = (map sspSymMapD (take 10 sspDataDefs)) ++ resShrDerivation ++
  [sspSymMapD (sspDataDefs !! 10)] ++ mobShrDerivation ++ [sspSymMapD (sspDataDefs !! 11)] ++
  stfMtrxDerivation ++ (map sspSymMapD (drop 12 sspDataDefs)) 
  --FIXME: move to DataDefs

resShrDerivation :: [Contents]
resShrDerivation = [foldlSP [S "The", phrase shrResI, S "of a slice is", 
  S "defined as", getS shrResI, S "in" +:+. acroGD 3, S "The", phrase nrmFSubWat,
  S "in the", phrase equation, S "for", getS shrResI, S "of the soil is defined", 
  S "in the perpendicular force equilibrium of a slice from", acroGD 2 `sC` 
  S "using the", getTandS nrmFSubWat, S "of", acroT 4, S "shown in", eqN 1],
  
  EqnBlock $
  (C nrmFSubWat) := (((C slcWght) - (C intShrForce) + (C intShrForce) + 
  (C surfHydroForce) * (cos (C surfAngle)) + --FIXME: add indexing
  (C surfLoad) * (cos (C impLoadAngle))) * (cos (C baseAngle)) +
  (Neg (C earthqkLoadFctr) * (C slcWght) - (C intNormForce) + (C intNormForce) -
  (C watrForce) + (C watrForce) + (C surfHydroForce) * sin (C surfAngle) + 
  (C surfLoad) * (sin (C impLoadAngle))) * (sin (C baseAngle)) - (C baseHydroForce)),
  
  foldlSP [S "values" `ofThe'` S "interslice forces", getS intNormForce `sAnd`
  getS intShrForce, S "in the", phrase equation, S "are unknown, while the other",
  plural value, S "are found from the physical force", plural definition,
  S "of", acroDD 1, S "to" +:+. acroDD 9,
  S "Consider a force equilibrium without the affect of interslice forces" `sC`
  S "to obtain a solvable value as done for", getS nrmFNoIntsl, S "in", eqN 2],

  EqnBlock $
  (C nrmFNoIntsl) := (((C slcWght) + (C surfHydroForce) * (cos (C surfAngle)) +
  (C surfLoad) * (cos (C impLoadAngle))) * (cos (C baseAngle)) +
  (Neg (C earthqkLoadFctr) * (C slcWght) - (C watrForce) + (C watrForce) +
  (C surfHydroForce) * sin (C surfAngle) +
  (C surfLoad) * (sin (C impLoadAngle))) * (sin (C baseAngle)) - (C baseHydroForce)),
  
  foldlSP [S "Using", getS nrmFNoIntsl `sC` S "a", phrase shearRNoIntsl,
  shearRNoIntsl ^. defn, S "can be solved for in terms of all known",
  S "values as done in", eqN 3],
  
  EqnBlock $
  C shearRNoIntsl := (C nrmFNoIntsl) * tan (C fricAngle) +
  (C cohesion) * (C baseWthX) * sec (C baseAngle) := (((C slcWght) + (C surfHydroForce) * (cos (C surfAngle)) +
  (C surfLoad) * (cos (C impLoadAngle))) * (cos (C baseAngle)) +
  (Neg (C earthqkLoadFctr) * (C slcWght) - (C watrForceDif) + (C surfHydroForce)
  * sin (C surfAngle) + (C surfLoad) * (sin (C impLoadAngle))) * (sin (C baseAngle))
  - (C baseHydroForce)) *
  tan (C fricAngle) + (C cohesion) * (C baseWthX) * sec (C baseAngle)
  ]

mobShrDerivation :: [Contents]
mobShrDerivation = [foldlSP [S "The", phrase mobShrI, S "acting on a slice is",
  S "defined as", getS mobShrI, S "from the force equilibrium in", acroGD 2 `sC`
  S "also shown in", eqN 4],
  
  EqnBlock $
  (C nrmFSubWat) := (((C slcWght) - (C intShrForce) + (C intShrForce) +
  (C surfHydroForce) * (cos (C surfAngle)) + --FIXME: add indexing
  (C surfLoad) * (cos (C impLoadAngle))) * (sin (C baseAngle)) -
  (Neg (C earthqkLoadFctr) * (C slcWght) - (C intNormForce) + (C intNormForce)
  - (C watrForce) + (C watrForce) + (C surfHydroForce)
  * sin (C surfAngle) + (C surfLoad) * (sin (C impLoadAngle))) * (cos (C baseAngle))),
  
  foldlSP [S "The", phrase equation, S "is unsolvable, containing the unknown",
  getTandS intNormForce, S "and" +:+. getTandS intShrForce, S "Consider a force", 
  S "equilibrium", S wiif `sC` S "to obtain the", getTandS shearFNoIntsl `sC` 
  S "as done in", eqN 5], --FIXME: use wiif from shearFNoIntsl's definition but removed index
  
  EqnBlock $
  C shearFNoIntsl := ((C slcWght) :+ (C surfHydroForce) :* (cos (C surfAngle)) :+ 
  (C surfLoad) :* (cos (C impLoadAngle))) :* (sin (C baseAngle)) :- 
  (Neg (C earthqkLoadFctr) :* (C slcWght) :- (C watrForceDif) :+ (C surfHydroForce)
  :* sin (C surfAngle) :+ (C surfLoad) :* (sin (C impLoadAngle))) :* (cos (C baseAngle)),
  
  foldlSP [S "The", plural value, S "of", getS shearRNoIntsl `sAnd` getS shearFNoIntsl,
  S "are now defined completely in terms of the known force property", plural value,
  S "of", acroDD 1, S "to", acroDD 9]
  ]

stfMtrxDerivation :: [Contents]
stfMtrxDerivation = [foldlSP [S "Using the force-displacement relationship of", 
  acroGD 8, S "to define stiffness matrix", getS shrStiffIntsl `sC` S "as seen in",
  eqN 6], --FIXME: index
  
  EqnBlock $ C shrStiffIntsl := dgnl2x2 (C shrStiffIntsl) (C nrmStiffBase),
  
  foldlSP [S "For interslice surfaces the stiffness constants and displacements",
  S "refer to an unrotated coordinate system" `sC` getS genDisplace, S "of" +:+.
  acroGD 9, S "The interslice elements are left in their standard coordinate system" `sC`
  S "and therefore are described by the same", phrase equation, S "from" +:+. acroGD 8,
  S "Seen as", getS shrStiffIntsl, S "in" +:+. acroDD 12, isElMx shrStiffIntsl "shear" `sC` --FIXME: Index
  S "and", isElMx nrmStiffIntsl "normal" `sC` S "calculated as in", acroDD 14],
  
  foldlSP [S "For basal surfaces the stiffness constants and displacements refer",
  S "to a system rotated for the base angle alpha" +:+. sParen (acroDD 5),
  S "To analyze the effect of force-displacement relationships occurring on both basal",
  S "and interslice surfaces of an", phrase element, getS index, S "they must reference", 
  S "the same coordinate system. The basal stiffness matrix must be rotated", 
  S "counter clockwise to align with" +:+. (phrase angle `ofThe` S "basal surface"),
  S "The base stiffness counter clockwise rotation is applied in", eqN 7,
  S "to the new matrix", getS nrmFNoIntsl],
  
  EqnBlock $ C shrStiffIntsl := --FIXME: Index
  m2x2 (cos(C baseAngle)) (Neg $ sin(C baseAngle)) (sin(C baseAngle)) (cos(C baseAngle)) *
  C shrStiffIntsl :=
  m2x2 (C shrStiffBase * cos(C baseAngle)) (Neg $ C nrmStiffBase * sin(C baseAngle))
  (C shrStiffBase * sin(C baseAngle)) (C nrmStiffBase * cos(C baseAngle)),
  
  foldlSP [S "The Hooke's law force displacement relationship of", acroGD 8,
  S "applied to the base also references a displacement vector", getS rotatedDispl,
  S "of", acroGD 9, S "rotated for", S "base angle" `ofThe` S "slice", 
  getS baseAngle +:+. S "The basal displacement vector", getS genDisplace, 
  S "is rotated clockwise to align with the interslice displacement vector",
  getS genDisplace `sC` S "applying the", phrase definition, S "of", 
  getS rotatedDispl, S "in terms of", getS genDisplace, S "as seen in" +:+. acroGD 9,
  S "Using this with base stiffness matrix", getS shrStiffBase --FIXME: index, should be K*i"
  `sC` S "a basal force displacement relationship in the same coordinate system",
  S "as the interslice relationship can be derived as done in", eqN 8],
  
  EqnBlock $ vec2D (C genPressure) (C genPressure) := C shrStiffBase * C rotatedDispl := --FIXME: pull from other equations? index
  m2x2 (C shrStiffBase * cos(C baseAngle)) (Neg $ C nrmStiffBase * sin(C baseAngle))
  (C shrStiffBase * sin(C baseAngle)) (C nrmStiffBase * cos(C baseAngle)) *
  m2x2 (cos(C baseAngle)) (sin(C baseAngle)) (Neg $ sin(C baseAngle)) (cos(C baseAngle)) *
  vec2D (C dx_i) (C dy_i) := m2x2
  (C shrStiffBase * cos(C baseAngle) :^ Int 2 + C nrmStiffIntsl * sin(C baseAngle) :^ Int 2)
  ((C shrStiffBase - C nrmStiffBase) * sin(C baseAngle) * cos(C baseAngle))
  ((C shrStiffBase - C nrmStiffBase) * sin(C baseAngle) * cos(C baseAngle))
  (C shrStiffBase * cos(C baseAngle) :^ Int 2 + C nrmStiffIntsl * sin(C baseAngle) :^ Int 2) *
  vec2D (C dx_i) (C dy_i),
  
  foldlSP [S "The new effective base stiffness matrix", getS shrStiffBase, --FIXME: index
  S "as derived in", eqN 7, S "is defined in" +:+. eqN 9, S "This is seen as matrix",
  getS shrStiffBase, S "in" +:+. acroGD 12, isElMx shrStiffBase "shear" `sC` S "and",
  isElMx nrmStiffBase "normal" `sC` S "calculated as in" +:+. acroDD 14,
  S "The notation is simplified by", S "introduction" `ofThe` S "constants",
  getS shrStiffBase `sAnd` getS shrStiffBase `sC` S "defined in", eqN 10 `sAnd`--FIXME: index should be KbA,i and KbB,i
  eqN 11, S "respectively"],
  
  EqnBlock $ C shrStiffBase := m2x2
  (C shrStiffBase * cos(C baseAngle) :^ Int 2 + C nrmStiffIntsl * sin(C baseAngle) :^ Int 2)
  ((C shrStiffBase - C nrmStiffBase) * sin(C baseAngle) * cos(C baseAngle))
  ((C shrStiffBase - C nrmStiffBase) * sin(C baseAngle) * cos(C baseAngle))
  (C shrStiffBase * cos(C baseAngle) :^ Int 2 + C nrmStiffIntsl * sin(C baseAngle) :^ Int 2)
  := m2x2 (C shrStiffBase) (C nrmStiffBase) (C nrmStiffBase) (C shrStiffBase),
  
  EqnBlock $
  (C shrStiffBase) := (C shrStiffBase) * (cos (C baseAngle)) :^ (Int 2) :+ --FIXME: the first symbol should be K_(bA,i), waiting on indexing
  (C nrmStiffBase) * (sin (C baseAngle)) :^ (Int 2),
  
  EqnBlock $
  (C shrStiffBase) := ((C shrStiffBase)-(C nrmStiffBase)) * --FIXME: the first symbol should be K_(bB,i), waiting on indexing
  (sin (C baseAngle)) * (cos (C baseAngle)),
  
  foldlSP [S "A force-displacement relationship for an element", getS index,
  S "can be written in terms of displacements occurring in the unrotated", 
  S "coordinate system", getS genDisplace `sOf` acroGD 9, S "using the matrix",
  getS shrStiffBase `sC` --FIXME: index 
  S "and", getS shrStiffBase, S "as seen in", acroDD 12]
  ]

isElMx :: (SymbolForm a) => a -> String -> Sentence
isElMx sym kword = getS sym `isThe` S kword +:+ S "element in the matrix"

-- SECTION 4.2.5 --
-- Instance Models is automatically generated in solChSpecF using the paragraphs below

s4_2_5_p2 = foldlSP [S "The", titleize morPrice,
  phrase method_, S "is a vertical", phrase slice `sC` S "limit equilibrium",
  phrase ssa +:+. phrase method_, at_start analysis, S "is performed by",
  S "breaking the assumed failure", phrase surface, S "into a series of vertical",
  plural slice, S "of" +:+. phrase mass, S "Static equilibrium",
  S "analysis using two", phrase force, S "equilibrium, and one moment",
  phrase equation, S "as in" +:+. acroT 2, S "The", phrase problem,
  S "is statically indeterminate with only these 3", plural equation, S "and one",
  S "constitutive", phrase equation, sParen $ S "the Mohr Coulomb shear strength of" +:+ 
  acroT 3, S "so the", phrase assumption, S "of", acroGD 5, S "is used. Solving for",
  phrase force, S "equilibrium allows", plural definition, S "of all", plural force,
  S "in terms of the", plural physicalProperty, S "of", acroDD 1, S "to",
  acroDD 9 `sC` S "as done in", acroDD 10 `sC` acroDD 11]

s4_2_5_p3 = foldlSP [plural value `ofThe'` (phrase intrslce +:+ phrase normForce),
  getS intNormForce, S "the", getTandS normToShear `sC`
  S "and the", titleize fs_rc, (sParen $ getS fs) `sC` S "are unknown.",
  at_start' equation, S "for the unknowns are written in terms of only the",
  plural value, S "in", acroDD 1, S "to", acroDD 9 `sC` S "the", plural value,
  S "of", getS shearRNoIntsl `sC` S "and", getS shearFNoIntsl, S "in", acroDD 10,
  S "and", acroDD 11 `sC` S "and each",
  S "other. The relationships between the unknowns are non linear" `sC`
  S "and therefore explicit", plural equation, S "cannot be derived and an",
  S "iterative", plural solution, S "method is required"]

s4_2_5_IMods = concat $ weave [map (\x -> [sspSymMapT x]) sspIMods, --FIXME: move to IMods
  [fctSftyDerivation, nrmShrDerivation, intrSlcDerivation,
  rigDisDerivation, rigFoSDerivation]]

fctSftyDerivation, nrmShrDerivation, intrSlcDerivation,
  rigDisDerivation, rigFoSDerivation :: [Contents]

fctSftyDerivation = [foldlSP [S "Using", eqN 21, S "from", acroIM 3 `sC`
  S "rearranging, and", boundaryCon `sC` S "an", phrase equation, 
  S "for the", phrase fs_rc, S "is found as", eqN 12 `sC` 
  S "also seen in", acroIM 1],
  
  EqnBlock fcSfty_rel,
  
  fUnknowns]

boundaryCon :: Sentence
boundaryCon = foldlSent_ [S "applying the boundary condition that", --FIXME: Index
  getS intNormForce `sAnd` getS intNormForce,  S "are equal to", E $ Int 0]

fUnknowns :: Contents
fUnknowns = foldlSP [S "The constants", getS mobShrC `sAnd` getS shrResC, 
  S "described in", eqN 20 `sAnd` eqN 19, S "are functions of the unknowns: the",
  getTandS normToShear, sParen (acroIM 2) `andThe` getTandS fs, sParen (acroIM 1)]

nrmShrDerivation = [foldlSP [S "Taking the last static", phrase equation,
  S "of", acroT 2, S "with the", phrase momentEql `sOf` acroGD 6, S "about", 
  (S "midpoint" `ofThe` S "base") `sAnd` S "the", phrase assumption, S "of",
  acroGD 5, S "results in", eqN 13],
  
  EqnBlock momEql_rel, --FIXME: this is not *exactly* the equation but very similar
  --Need more simbols (z) to finish
  
  foldlSP [S "The", phrase equation, S "in terms of", getS normToShear, S "leads to", eqN 14],
  
  EqnBlock $
  C normToShear := momEql_rel / ((C baseWthX / Int 2) * --FIXME: remove Int 0 from momEql_rel
  (C intNormForce * C scalFunc + C intNormForce * C scalFunc)), 
  
  foldlSP [S "Taking a summation of each slice, and", boundaryCon `sC`
  S "a general", phrase equation, S "for the constant", getS normToShear,
  S "is developed in", eqN 15 `sC` S "also found in", acroIM 2], --NOTE: "Taking this with that and the assumption of _ to get equation #" pattern
  
  EqnBlock $
  C normToShear := summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseWthX * (C intNormForce + C intNormForce + C watrForce + C watrForce) * tan(C baseAngle) +
  C midpntHght * (C earthqkLoadFctr * C slcWght - Int 2 * C surfHydroForce * sin(C surfAngle) -
  Int 2 * C surfLoad * sin(C impLoadAngle))) / 
  summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseWthX * (C intNormForce * C scalFunc + C intNormForce * C scalFunc)),
  
  foldlSP [eqN 15, S "for", getS normToShear `sC` S "is a function of the unknown",
  getTandS intNormForce, acroIM 3]
  ]

intrSlcDerivation = [foldlSP [S "Taking the", phrase normForcEq `sOf` acroGD 1,
  S "with the", phrase effStress, phrase definition, S "from", acroT 4, --NOTE: "Taking this with that and the assumption of _ to get equation #" pattern
  S "that", E (C totNrmForce := C nrmFSubWat - C baseHydroForce) `sC`
  S "and the assumption of", acroGD 5, S "the equilibrium", phrase equation, 
  S "can be rewritten as", eqN 16],
  
  EqnBlock $
  C nrmFSubWat := ((C slcWght :- C normToShear :* C scalFunc :* C intNormForce :+ 
  C normToShear :* C scalFunc :* C intNormForce :+ 
  C baseHydroForce :* cos (C surfAngle) :+ C surfLoad :* 
  cos (C impLoadAngle)) :* cos (C baseAngle)
  :+ (Neg (C earthqkLoadFctr) :* C slcWght :- 
  C intNormForce :+ C intNormForce :- C watrForce :+ 
  C watrForce :+ C surfHydroForce :* sin (C surfAngle) :+ 
  C surfLoad :* sin (C impLoadAngle)) :* sin (C baseAngle)) - (C baseHydroForce),
  
  foldlSP [S "Taking the", phrase bsShrFEq `sOf` acroGD 2, S "with the", phrase definition,
  S "of", phrase mobShr, S "from", acroGD 4 `sAnd` S "the assumption of", acroGD 5 `sC`
  S "the equilibrium", phrase equation, S "can be rewritten as", eqN 17], --NOTE: "Taking this with that and the assumption of _ to get equation #" pattern
  
  EqnBlock $
  ((C totNrmForce) * tan (C fricAngle) + (C cohesion) * (C baseWthX) * sec (C baseAngle)) / (C fs) := --FIXME: pull the left side of this from GD4
  (C slcWght :- C normToShear :* C scalFunc :* C intNormForce :+ 
  C normToShear :* C scalFunc :* C intNormForce :+ 
  C baseHydroForce :* cos (C surfAngle) :+ C surfLoad :* 
  cos (C impLoadAngle)) :* sin (C baseAngle)
  :+ (Neg (C earthqkLoadFctr) :* C slcWght :- 
  C intNormForce :+ C intNormForce :- C watrForce :+ 
  C watrForce :+ C surfHydroForce :* sin (C surfAngle) :+ 
  C surfLoad :* sin (C impLoadAngle)) :* cos (C baseAngle),
  
  foldlSP [S "Substituting the", phrase equation, S "for", getS nrmFSubWat,
  S "from", eqN 16, S "into", eqN 17, S "and rearranging results in", eqN 18],

  EqnBlock $
  (C intNormForce) * (((C normToShear)*(C scalFunc) * cos (C baseAngle) - sin (C baseAngle)) * tan (C fricAngle) -
  ((C normToShear)*(C scalFunc) * sin (C baseAngle) - cos (C baseAngle)) * (C fs)) := 
  (C intNormForce) * (((C normToShear)*(C scalFunc) * cos (C baseAngle) - sin (C baseAngle)) * tan (C fricAngle) -
  ((C normToShear)*(C scalFunc) * sin (C baseAngle) - cos (C baseAngle)) * (C fs)) +
  (C fs) * (C shearFNoIntsl) - (C shearRNoIntsl),
  
  foldlSP [S "Where", getS shearRNoIntsl `sAnd` getS shearFNoIntsl, S "are the",
  S "resistive and mobile shear of the slice" `sC` S wiif, getS intNormForce
  `sAnd` getS intShrForce `sC` S "as defined in", acroDD 10 `sAnd` acroDD 11,
  S "Making use of the constants, and with full", plural equation, 
  S "found below in", eqN 19 `sAnd` eqN 20, S "respectively, then", eqN 18, 
  S "can be simplified to", eqN 21 `sC` S "also seen in", acroIM 3],
  
  EqnBlock $
  (C shrResC) := ((C normToShear)*(C scalFunc) * cos (C baseAngle) - sin (C baseAngle)) * tan (C fricAngle) -
  ((C normToShear)*(C scalFunc) * sin (C baseAngle) - cos (C baseAngle)) * (C fs),
  --FIXME: index everything here and add "Where i is the local slice of mass for 1 :<= i :<= n-1"
  EqnBlock $
  (C mobShrC) := ((C normToShear)*(C scalFunc) * cos (C baseAngle) - sin (C baseAngle)) * tan (C fricAngle) -
  ((C normToShear)*(C scalFunc) * sin (C baseAngle) - cos (C baseAngle)) * (C fs),
  
  EqnBlock $
  (C intNormForce) := ((C mobShrC)*(C intNormForce) + (C fs)*(C shearFNoIntsl)
  - (C shearRNoIntsl)) / (C shrResC),
  
  fUnknowns]

rigDisDerivation = [foldlSP [S "Using the net force-displacement equilibrium",
  phrase equation, S "of a slice from", acroDD 13, S "with", plural definition
  `ofThe` S "stiffness matrices", S "from", acroDD 12, S "and the force", 
  plural definition, S "from", acroGD 7 , S "a broken down force displacement", 
  S "equilibrium", phrase equation +:+. S "can be derived",
  eqN 22, S "gives the broken down", phrase equation, S "in the x direction"
  `sC` S "and", eqN 23, S "gives the broken down", phrase equation,
  S "in the y direction"],

  EqnBlock fDisEq_rel, --FIXME: Original equations need indexing
  
  foldlSP [S "Using the known input assumption of", acroA 2 `sC` S "the force",
  S "variable", plural definition, S "of", acroDD 1, S "to", acroDD 8, S "on",
  S "left side" `ofThe` plural equation, S "can be solved for. The only unknown", 
  S "in the variables to solve for the stiffness values from", acroDD 14 +:+. 
  S "is the displacements", S "Therefore taking the", phrase equation, 
  S "from each slice a set of", E $ (Int 2) * (C numbSlices), plural equation
  `sC` S "with", E $ (2) * (C numbSlices), S "unknown displacements in the", 
  S "x and y directions of each slice can be derived. Solutions for the displacements",
  S "of each slice can then be found. The use of displacement in", phrase definition `ofThe`
  S "stiffness values makes the", phrase equation, S "implicit, which means an iterative solution",
  S "method, with an initial guess for the displacements in the stiffness", plural value,
  S "is required"]
  ]

rigFoSDerivation = [foldlSP [S "RFEM analysis can also be used to calculate the",
  phrase fs, S "for the slope. For a slice element", getS index, S "the displacements",
  getS dx_i `sAnd` getS dy_i `sC` S "are solved from the system of", plural equation, 
  S "in" +:+. acroIM 4, S "The", phrase definition, S "of", getS rotatedDispl, S "as", 
  S "rotation" `ofThe` S "displacement vector", getS genDisplace, S "is seen in" +:+.
  acroGD 9, S "This is", --FIXME: index i 
  S "used to find", plural displacement `ofThe` S "slice parallel to", 
  S "base" `ofThe` S "slice", getS shrDispl `sIn` eqN 24, S "and normal to", 
  S "base" `ofThe` S "slice", getS nrmDispl, S "in", eqN 25],
  
  EqnBlock $
  C shrDispl := cos(C baseAngle) * C dx_i + sin(C baseAngle) * C dy_i,
  EqnBlock $
  C nrmDispl := Neg (sin(C baseAngle)) * C dx_i + sin(C baseAngle) * C dy_i,
  
  foldlSP [S "With the", phrase definition, S "of normal stiffness from", acroDD 14, --FIXME: grab nrmStiffBase's term name?
  S "to find", S "normal stiffness" `ofThe` S "base", getS nrmStiffBase,
  S "and the now known base displacement perpendicular to the surface",
  getS nrmDispl, S "from", eqN 25, S "the normal base stress",
  S "can be calculated from the force-displacement relationship of" +:+. acroT 5,
  S "Stress", getS normStress `sIs` S "used in place of", getTandS genForce, --FIXME: use getTandS
  S "as the stiffness hasn't been normalized for" +:+. (S "length" `ofThe` S "base"), 
  S "Results" `sIn` eqN 26], --FIXME: grammar

  EqnBlock $
  C normStress := C nrmStiffBase * C nrmDispl, --FIXME: index
  
  foldlSP [S "The resistive shear to calculate the", getTandS fs,
  S "is found from the Mohr Coulomb resistive strength of soil in", acroT 3,
  S "Using the", getTandS normStress, S "from", eqN 26, S "as the stress" `sC`
  (S "resistive shear" `ofThe` S "slice"), S "can be calculated from", eqN 27],
  
  EqnBlock $
  C mobStress := C cohesion - C normStress * tan(C fricAngle), --FIXME: index and prime
  
  foldlSP [S "Previously", phrase value `ofThe` getTandS shrStiffBase,
  S "as seen in", eqN 28, S "was unsolvable because the", getTandS normStress,
  S "was unknown. With the", phrase definition, S "of", getS normStress, S "from", eqN 26,
  S "and the", phrase definition, S "of displacement shear to the base", getS shrDispl,
  S "from", eqN 25 `sC` S "the value of", getS shrStiffBase, S "becomes solvable"],
  
  EqnBlock $
  C shrStiffBase := C intNormForce / (Int 2 * (Int 1 + C poissnsRatio)) * (Dbl 0.1 / C baseWthX) +
  (C cohesion - C normStress * tan(C fricAngle)) / (abs (C shrDispl) + V "a"),
  
  foldlSP [S "With", getTandS shrStiffBase, S "calculated in", eqN 28,
  S "and shear displacement", getS shrDispl, S "calculated in", eqN 24, --FIXME: grab term too once we have a displacement modifier
  S "values now known the", phrase shrStress, shrStress ^. defn, getS shrStress,
  S "can be calculated using", acroT 5 `sC` S "as done in" +:+. eqN 29,
  S "Again, stress", getS shrStress, S "is used in place of force", getS genForce, --FIXME: grab term
  S "as the stiffness has not been normalized for", S "length" `ofThe` S "base"],
  
  EqnBlock $
  C shrStress := C shrStiffBase * C shrDispl,
  
  foldlSP [S "The", phrase shrStress, shrStress ^. defn, getS shrStress, --FIXME: ISSUE #348
  S "acts as the mobile shear acting on the base. Using the", phrase definition,
  titleize fs, phrase equation, S "from", acroT 1 `sC` S "with the", 
  plural definition, S "of resistive shear strength of a slice", getS mobStress,
  S "from", phrase equation, S "(27) and shear stress on a slice", getS shrStress, S "from",
  eqN 29, S "the", getTandS fsloc, S "can be found from as seen in", eqN 30 `sAnd` acroIM 5],
  
  EqnBlock $
  C fsloc := C mobStress / C shrStress :=
  (C cohesion - C nrmStiffBase * C nrmDispl * tan(C fricAngle)) /
  (C shrStiffBase * C shrDispl), --FIXME: pull parts of this equation from other equations such as IM5
  
  foldlSP [S "The global", titleize fs, S "is then", S "ratio" `ofThe` S "summation",
  S "of the resistive and mobile shears for each slice, with a weighting for" +:+.
  (S "length" `ofThe` S "slice's base"), S "Shown in", eqN 31 `sAnd` acroIM 5],
  
  EqnBlock $ --FIXME: pull from other equations in derivation
  (C fs) := summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseLngth * C mobStress) /
  summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseLngth * C shrStress) :=
  summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseLngth * (C cohesion - C nrmStiffBase * C nrmDispl * tan(C fricAngle))) /
  summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseLngth * (C shrStiffBase * C shrDispl)) --FIXME: Grouping with brackets
  ]

-- SECTION 4.2.6 --
-- Data Constraints is automatically generated in solChSpecF using the tables below
{-
{-input data-}
noTypicalVal, vertConvention :: Sentence
noTypicalVal   = short notApp
vertConvention = S "Consecutive vertexes have increasing x" +:+. plural value +:+
  S "The start and end vertices of all layers go to the same x" +:+. plural value --Monotonicly increasing?

verticesConst :: Sentence -> [Sentence]
verticesConst vertexType = [vertVar vertexType, vertConvention, noTypicalVal, noTypicalVal, noTypicalVal]

waterVert, slipVert, slopeVert :: [Sentence]
waterVert = verticesConst $ S "water" +:+ phrase table_
slipVert  = verticesConst $ phrase slip
slopeVert = verticesConst $ phrase slope
-}
{-input and output tables-}
s4_2_6Table2, s4_2_6Table3 :: Contents
s4_2_6Table2 = inDataConstTbl sspInputs --FIXME: needs more inputs but cannot express them yet
s4_2_6Table3 = outDataConstTbl sspOutputs

-- SECTION 5 --
s5 = reqF [s5_1, s5_2]

-- SECTION 5.1 --
s5_1 = SRS.funcReq
  [s5_1_list, sspInputDataTable] []

s5_1_list = enumSimple 1 (short requirement) sspRequirements

-- SECTION 5.2 --
s5_2 = nonFuncReqF [accuracy, performanceSpd]
  [correctness, understandability, reusability, maintainability] r EmptyS
  where r = (short ssa) +:+ S "is intended to be an educational tool"

-- SECTION 6 --
s6 = SRS.likeChg [] []

-- SECTION 7 --
s7 = valsOfAuxConstantsF ssa []

-- References --
s8 = SRS.reference [s8_list] []

s8_list = mkRefsList 1 [ --FIXME: names should be in italics
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