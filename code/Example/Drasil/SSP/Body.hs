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
import Drasil.SSP.References
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
  s5_1_list :: Contents

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

sspChoices :: Choices
sspChoices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,         -- LogNone, LogFunc
  comments = CommentNone,    -- CommentNone, CommentFunc
  onSfwrConstraint = Warning,  -- Warning, Exception
  onPhysConstraint = Warning,  -- Warning, Exception
  inputStructure = Loose    -- Loose, AsClass
}  
  
ssp_code :: CodeSpec
ssp_code = codeSpec' ssp_si sspChoices


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
  instModIntro1:instModIntro2:s4_2_5_IMods, [s4_2_6Table2, s4_2_6Table3]) []
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
  --FIXME: derivations should be with the appropriate dataDef

-- SECTION 4.2.5 --
-- Instance Models is automatically generated in solChSpecF using the paragraphs below

s4_2_5_IMods = concat $ weave [map (\x -> [sspSymMapT x]) sspIMods, --FIXME: move to IMods
  [fctSftyDerivation, nrmShrDerivation, intrSlcDerivation,
  rigDisDerivation, rigFoSDerivation]]


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
s8 = SRS.reference [sspReferences] []