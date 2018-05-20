module Drasil.SSP.Body (ssp_srs, ssp_code, sspSymMap) where

import Language.Drasil hiding (organization)
import Data.Drasil.SI_Units
import Control.Lens ((^.))
import Prelude hiding (sin, cos, tan)

import Data.Drasil.People (henryFrankis)

import Drasil.SSP.Assumptions (sspAssumptions)
import Drasil.SSP.DataDefs (sspDataDefs, resShrDerivation,
  mobShrDerivation, stfMtrxDerivation)
import Drasil.SSP.DataDesc (sspInputMod)
import Drasil.SSP.Defs (ssa, acronyms, slice, slope, soil,
  crtSlpSrf, soilLyr, morPrice, mtrlPrpty, slpSrf,
  fs_concept, plnStrn, intrslce, itslPrpty)
import Drasil.SSP.GenDefs (sspGenDefs)
import Drasil.SSP.Goals (sspGoals)
import Drasil.SSP.IMods (instModIntro1, instModIntro2,
  sspIMods, fctSftyDerivation, nrmShrDerivation,
  intrSlcDerivation, rigDisDerivation, rigFoSDerivation)
import Drasil.SSP.References (sspCitations)
import Drasil.SSP.Requirements (sspRequirements, sspInputDataTable)
import Drasil.SSP.TMods (sspTMods)
import Drasil.SSP.Unitals (sspSymbols, sspInputs, sspOutputs,
  sspConstrained, index, fs, numbSlices)
import qualified Drasil.SRS as SRS (physSyst, funcReq, likeChg, inModel,
  missingP)

import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.DocumentLanguage (TSIntro, DocDesc, RefSec(..),
  RefTab(..), tsymb'', LFunc(..),
  IntroSub(..), TSIntro(..), TConvention(..),
  DocSection(..), mkDoc, IntroSec(..))
import Drasil.Sections.SpecificSystemDescription (inDataConstTbl,
  outDataConstTbl, dataConstraintUncertainty, goalStmtF, termDefnF,
  probDescF, solChSpecF, specSysDesF)
import Drasil.Sections.Requirements (reqF, nonFuncReqF)
import Drasil.Sections.GeneralSystDesc (genSysF)
import Drasil.Sections.AuxiliaryConstants (valsOfAuxConstantsF)

import Data.Drasil.Concepts.Documentation (srs, physics, inModel,
  value, effect, loss, interest, problem, design, issue,
  method_, analysis, input_, organization, document,
  template, analysis, endUser, definition, model, element,
  property, interest, variable, table_, goalStmt,
  assumption, requirement)
import Data.Drasil.Phrase (for)
import Data.Drasil.Concepts.Physics (stress, strain, tension,
  compression, fbd, force)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Education (solidMechanics, undergraduate)
import Data.Drasil.Concepts.Software (program, accuracy,
  performanceSpd, correctness, understandability,
  reusability, maintainability)
import Data.Drasil.Concepts.Math (surface, equation)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)
import Data.Drasil.Software.Products (sciCompS)

import Data.Drasil.Utils (getES, enumBullet, enumSimple, weave)
import Data.Drasil.SentenceStructures (sOr, acroDD,
  foldlSent, ofThe, sAnd, foldlSP, foldlList, foldlSent_)

--type declarations for sections--
s3, s4, s5, s6, s7 :: Section

s1_2_intro :: [TSIntro]

s4_1, s4_1_1, s4_1_2,
  s4_1_3, s4_2, s5_1, s5_2 :: Section

s4_1_1_list, s4_1_2_p1, s4_1_2_bullets,
  s4_1_2_p2, goals_list, s4_2_1_list,
  s5_1_list :: Contents

s4_2_2_tmods, s4_2_3_genDefs, s4_2_4_dataDefs, s4_2_5_IMods :: [Contents]

--Document Setup--
this_si :: [UnitDefn]
this_si = map unitWrapper [metre, degree] ++ map unitWrapper [newton, pascal]

ssp_si :: SystemInformation
ssp_si = SI {
  _sys = ssa, 
  _kind = srs, 
  _authors = [henryFrankis],
  _units = this_si,
  _quants = sspSymbols,
  _concepts = (sspSymbols),
  _definitions = sspDataDefs,
  _inputs = map qw sspInputs,
  _outputs = map qw sspOutputs,
  _defSequence = [Parallel (head sspDataDefs) (tail sspDataDefs)],
  _constraints = sspConstrained,
  _constants = [],
  _sysinfodb = sspSymMap,
  _refdb = sspRefDB
}

sspRefDB :: ReferenceDB
sspRefDB = rdb [] [] [] [] [] sspCitations
-- FIXME: Convert the rest to new chunk types (similar to issues #446 and #447)

mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro
  [TUnits, tsymb'' s1_2_intro TAD, TAandA]) :
  IntroSec (IntroProg startIntro kSent
    [IPurpose prpsOfDoc_p1, IScope scpIncl scpEnd
    , IChar (phrase solidMechanics) 
      (phrase undergraduate +:+ S "level 4" +:+ phrase physics)
      EmptyS
    , IOrgSec orgSecStart inModel (SRS.inModel SRS.missingP []) orgSecEnd]) :
    --FIXME: issue #235
  map Verbatim [s3, s4, s5, s6, s7] ++ (Bibliography : [])
  
ssp_srs :: Document
ssp_srs = mkDoc mkSRS (for) ssp_si
  
ssp_code :: CodeSpec
ssp_code = codeSpec ssp_si [sspInputMod]


-- SYMBOL MAP HELPERS --
sspSymMap :: ChunkDB
sspSymMap = cdb sspSymbols (map nw sspSymbols ++ map nw acronyms) ([] :: [ConceptChunk]) -- FIXME: Fill in Concepts
  this_si

-- SECTION 1 --
--automatically generated in mkSRS -

-- SECTION 1.1 --
--automatically generated in mkSRS

-- SECTION 1.2 --
--automatically generated in mkSRS using the intro below

s1_2_intro = [TSPurpose, TypogConvention [Verb $ foldlSent_
  [plural value, S "with a subscript", getES index, S "implies that the",
  phrase value, S "will be taken at and analyzed at a", phrase slice
  `sOr` phrase slice, S "interface composing the total slip", phrase mass]]]

-- SECTION 1.3 --
--automatically generated in mkSRS

-- SECTION 2 --
startIntro, kSent :: Sentence
startIntro = foldlSent [S "A", phrase slope, S "of geological",
  phrase mass `sC` S "composed of", phrase soil, S "and rock, is subject", 
  S "to the influence of gravity on the" +:+. phrase mass, S "For an unstable",
  phrase slope, S "this can cause instability in the form" +:+.
  S "of soil/rock movement", S "The", plural effect,
  S "of soil/rock movement can range from inconvenient to",
  S "seriously hazardous, resulting in signifcant life and economic" +:+.
  plural loss, at_start slope, S "stability is of", phrase interest,
  S "both when analyzing natural", plural slope `sC`
  S "and when designing an excavated" +:+.  phrase slope, at_start ssa,
  S "is", (S "assessment" `ofThe` S "safety of a" +:+ phrase slope) `sC`
  S "identifying the", phrase surface,
  S "most likely to experience slip" `sAnd`
  S "an index of its relative stability known as the", phrase fs]

kSent = keySent ssa

keySent :: (Idea a) => a -> Sentence
keySent pname = foldlSent_ [S "a", phrase pname +:+. phrase problem,
  S "The developed", phrase program, S "will be referred to as the",
  introduceAbb pname, phrase program]
  
-- SECTION 2.1 --
-- Purpose of Document automatically generated in introductionF
prpsOfDoc_p1 :: Sentence
prpsOfDoc_p1 = purposeDoc ssa crtSlpSrf fs how introduces analysizes
  where how = S "assessing the stability of a" +:+ phrase slope +:+
          phrase design
        introduces = phrase slope +:+ S "stability" +:+ plural issue
        analysizes = S "safe" +:+ phrase slope

purposeDoc :: (Idea a, NamedIdea b, NamedIdea c) =>
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
  S "range" `sC` S "and find the" +:+ phrase fs +:+ S "for the" +:+
  phrase slope +:+ S "as well as displacement of" +:+ phrase soil +:+
  S "that will occur on the" +:+ phrase slope

-- SECTION 2.3 --
-- Characteristics of the Intended Reader generated in introductionF

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
-- User Characteristics automatically generated in genSysF with the
-- userContraints intro below

userCharIntro :: Contents
userCharIntro = userChar ssa [S "Calculus", titleize physics]
  [phrase soil, plural mtrlPrpty]

userChar :: (Idea a) => a -> [Sentence] -> [Sentence] -> Contents
userChar pname understandings familiarities = foldlSP [
  S "The", phrase endUser, S "of", short pname,
  S "should have an understanding of undergraduate Level 1",
  foldlList understandings `sC`
  S "and be familiar with", foldlList familiarities]

-- SECTION 3.2 --
-- System Constraints automatically generated in genSysF

-- SECTION 4 --
s4 = specSysDesF end [s4_1, s4_2]
  where end = foldlSent_ [plural definition, S "and finally the",
          plural inModel, S "that", phrase model, S "the", phrase slope]

-- SECTION 4.1 --
s4_1 = probDescF EmptyS ssa ending [s4_1_1, s4_1_2, s4_1_3]
  where ending = foldlSent_ [S "evaluate the", phrase fs, S "of a",
          phrase's slope, phrase slpSrf, S "and to calculate the",
          S "displacement that the", phrase slope, S "will experience"]

-- SECTION 4.1.1 --
s4_1_1 = termDefnF Nothing [s4_1_1_list]

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

s4_1_2_p1 = physSystIntro slope how intrslce slice (S "slice base")
  fig_indexconv
  where how = S "as a series of" +:+ phrase slice +:+. plural element

physSystIntro :: (NamedIdea a, NamedIdea b, NamedIdea c, Referable d) =>
  a -> Sentence -> b -> c -> Sentence -> d -> Contents
physSystIntro what how p1 p2 p3 indexref = foldlSP [
  at_start analysis, S "of the", phrase what, S "is performed by looking at",
  plural property, S "of the", phrase what, how, S "Some", plural property,
  S "are", phrase p1, plural property `sC` S "and some are", phrase p2 `sOr`
  p3 +:+. plural property, S "The index convention for referencing which",
  phrase p1 `sOr` phrase p2, S "is being used is shown in", makeRef indexref]

s4_1_2_bullets = enumBullet $ map foldlSent_ [

  [at_start' itslPrpty, S "convention is noted by j. The end",
  plural itslPrpty, S "are usually not of", phrase interest `sC`
  S "therefore use the", plural itslPrpty, S "from", 
  E $ real_interval index $ Bounded (Inc,1) (Inc,sy numbSlices -1)],
  -- (E $ 1 $<= sy index $<= (sy numbSlices) - 1)],

  [at_start slice, plural property +:+. S "convention is noted by",
  getES index]]

s4_1_2_p2 = foldlSP [S "A", phrase fbd, S "of the", plural force,
  S "acting on the", phrase slice, S "is displayed in",
  makeRef fig_forceacting]

fig_indexconv :: Contents
fig_indexconv = fig (foldlSent_ [S "Index convention for numbering",
  phrase slice `sAnd` phrase intrslce,
  phrase force, plural variable]) "IndexConvention.png" "IndexConvention"

fig_forceacting :: Contents
fig_forceacting = fig (at_start' force +:+ S "acting on a" +:+
  phrase slice) "ForceDiagram.png" "ForceDiagram"

-- SECTION 4.1.3 --
s4_1_3 = goalStmtF (map (\(x, y) -> x `ofThe` y) [
  (S "geometry", S "water" +:+ phrase table_),
  (S "geometry", S "layers composing the plane of a" +:+ phrase slope),
  (plural mtrlPrpty, S "layers")
  ]) [goals_list]

goals_list = enumSimple 1 (short goalStmt) sspGoals

-- SECTION 4.2 --
s4_2 = solChSpecF ssa (s4_1, s6) ddEnding
  (EmptyS, dataConstraintUncertainty, EmptyS)
  ([s4_2_1_list], s4_2_2_tmods, s4_2_3_genDefs, s4_2_4_dataDefs, 
  instModIntro1:instModIntro2:s4_2_5_IMods, [s4_2_6Table2, s4_2_6Table3]) []

  where ddEnding = foldlSent [at_start' definition, acroDD 1, S "to", acroDD 8,
          S "are the", phrase force, plural variable, S "that can be solved",
          S "by direct analysis of given" +:+. plural input_, S "The", 
          phrase intrslce, S "forces", acroDD 9, S "are", phrase force,
          plural variable, S "that must be written in terms of", acroDD 1, 
          S "to", acroDD 8, S "to solve"]

-- SECTION 4.2.1 --
-- Assumptions is automatically generated in solChSpecF using the list below

s4_2_1_list = enumSimple 1 (short assumption) sspAssumptions

-- SECTION 4.2.2 --
-- TModels is automatically generated in solChSpecF using the tmods below

s4_2_2_tmods = map reldefn sspTMods

-- SECTION 4.2.3 --
-- General Definitions is automatically generated in solChSpecF
s4_2_3_genDefs = map reldefn sspGenDefs

-- SECTION 4.2.4 --
-- Data Definitions is automatically generated in solChSpecF
s4_2_4_dataDefs = (map datadefn (take 13 sspDataDefs)) ++ resShrDerivation ++
  [datadefn (sspDataDefs !! 13)] ++ mobShrDerivation ++
  map datadefn [sspDataDefs !! 14, sspDataDefs !! 15] ++
  stfMtrxDerivation ++ (map datadefn (drop 16 sspDataDefs))
  --FIXME: derivations should be with the appropriate DataDef

-- SECTION 4.2.5 --
-- Instance Models is automatically generated in solChSpecF
-- using the paragraphs below

s4_2_5_IMods = concat $ weave [map (\x -> [reldefn x]) sspIMods,
  [fctSftyDerivation, nrmShrDerivation, intrSlcDerivation,
  rigDisDerivation, rigFoSDerivation]]
  --FIXME: derivations should be with the appropriate IMod

-- SECTION 4.2.6 --
-- Data Constraints is automatically generated in solChSpecF
-- using the tables below

{-
{-input data-}
noTypicalVal, vertConvention :: Sentence
noTypicalVal   = short notApp
vertConvention = S "Consecutive vertexes have increasing x" +:+.
  plural value +:+ S "The start and end vertices of all layers" +:+
  S "go to the same x" +:+. plural value --Monotonicly increasing?

verticesConst :: Sentence -> [Sentence]
verticesConst vertexType = [vertVar vertexType, vertConvention,
  noTypicalVal, noTypicalVal, noTypicalVal]

waterVert, slipVert, slopeVert :: [Sentence]
waterVert = verticesConst $ S "water" +:+ phrase table_
slipVert  = verticesConst $ phrase slip
slopeVert = verticesConst $ phrase slope
-}
{-input and output tables-}

s4_2_6Table2, s4_2_6Table3 :: Contents
s4_2_6Table2 = inDataConstTbl sspInputs --FIXME: issue #295
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
-- automatically generated
