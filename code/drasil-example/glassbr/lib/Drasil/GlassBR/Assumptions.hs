module Drasil.GlassBR.Assumptions (assumpGT, assumpGC, assumpES, assumpSV,
  assumpGL, assumpBC, assumpRT, assumpLDFC, assumptionConstants,
  assumptions) where

import Language.Drasil hiding (organization)
import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.NounPhrase.Combinators as NP
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation as Doc (assumpDom, condition,
  constant, practice, reference, scenario, system, value)
import Data.Drasil.Concepts.Math (calculation, surface, shape)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty)

import Drasil.GlassBR.Concepts (beam, cantilever, edge, glaSlab, glass, glassBR, 
  lShareFac, plane, responseTy)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (constantK, constantLoadDur, 
  constantLoadSF, constantM, constantModElas, explosion, lateral, lDurFac,
  loadDur)

assumptions :: [ConceptInstance]
assumptions = [assumpGT, assumpGC, assumpES, assumpSV, assumpGL, assumpBC,
  assumpRT, assumpLDFC]

assumptionConstants :: [ConstQDef]
assumptionConstants = [constantM, constantK, constantModElas,
  constantLoadDur, constantLoadSF]

assumpGT, assumpGC, assumpES, assumpSV, assumpGL, assumpBC, assumpRT, assumpLDFC :: ConceptInstance
assumpGT           = cic "assumpGT"   glassTypeDesc                     "glassType"           Doc.assumpDom
assumpGC           = cic "assumpGC"   glassConditionDesc                "glassCondition"      Doc.assumpDom
assumpES           = cic "assumpES"   explainScenarioDesc               "explainScenario"     Doc.assumpDom
assumpSV           = cic "assumpSV"   (standardValuesDesc loadDur)      "standardValues"      Doc.assumpDom
assumpGL           = cic "assumpGL"   glassLiteDesc                     "glassLite"           Doc.assumpDom
assumpBC           = cic "assumpBC"   boundaryConditionsDesc            "boundaryConditions"  Doc.assumpDom
assumpRT           = cic "assumpRT"   responseTypeDesc                  "responseType"        Doc.assumpDom
assumpLDFC         = cic "assumpLDFC" (ldfConstantDesc lDurFac)         "ldfConstant"         Doc.assumpDom

glassTypeDesc :: Sentence
glassTypeDesc = foldlSent [S "The standard E1300-09a for",
  phrase calculation, S "applies only to", foldlList Comma Options $ map S ["monolithic",
  "laminated", "insulating"], S "glass constructions" `S.of_` S "rectangular", phrase shape, 
  S "with continuous", phrase lateral, S "support along",
  foldlList Comma Options (map S ["one", "two", "three", "four"]) +:+.
  plural edge, S "This", phrase practice +: S "assumes that",
  foldlEnumList Numb Parens SemiCol List $ map foldlSent_
  [[S "the supported glass", plural edge, S "for two, three" `S.and_`
  S "four-sided support", plural condition, S "are simply supported" `S.and_`
  S "free to slip in", phrase plane], 
  [S "glass supported on two sides acts as a simply supported", phrase beam], 
  [S "glass supported on one side acts as a", phrase cantilever]]]

glassConditionDesc :: Sentence
glassConditionDesc = foldlSent [S "Following", complexRef astm2009 (Page [1]) `sC` 
  S "this", phrase practice, S "does not apply to any form of", foldlList Comma Options $ map S ["wired",
  "patterned", "etched", "sandblasted", "drilled", "notched", "grooved glass"], S "with", 
  phrase surface `S.and_` S "edge treatments that alter the glass strength"]

explainScenarioDesc :: Sentence
explainScenarioDesc = foldlSent [S "This", phrase system, S "only considers the external", 
  phraseNP (combineNINI explosion scenario), S "for its", plural calculation]

standardValuesDesc :: UnitaryChunk -> Sentence
standardValuesDesc mainIdea = foldlSent [atStartNP' (the value), S "provided in",
  refS $ SRS.valsOfAuxCons ([]::[Contents]) ([]::[Section]), S "are assumed for the", phrase mainIdea, 
  sParen (ch mainIdea) `sC` S "and the", plural materialProprty `S.of_` 
  foldlList Comma List (map ch (take 3 assumptionConstants))]

glassLiteDesc :: Sentence
glassLiteDesc = foldlSent [atStart glass, S "under consideration is assumed to be a single", 
  S "lite; hence, the", phrase value `S.of_` short lShareFac, S "is equal to 1 for all",
  plural calculation `S.in_` short glassBR]

boundaryConditionsDesc :: Sentence
boundaryConditionsDesc = foldlSent [S "Boundary", plural condition, S "for the",
  phrase glaSlab, S "are assumed to be 4-sided support for",
  plural calculation]

responseTypeDesc :: Sentence
responseTypeDesc = foldlSent [atStartNP (the responseTy), S "considered in",
  short glassBR, S "is flexural"]

ldfConstantDesc :: QuantityDict -> Sentence
ldfConstantDesc mainConcept = foldlSent [S "With", phrase reference, S "to",
  refS assumpSV `sC` phraseNP (NP.the (value `of_`
  mainConcept)), sParen (ch mainConcept) `S.is` phraseNP (a_ constant)
  `S.in_` short glassBR]
