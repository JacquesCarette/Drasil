module Drasil.GlassBR.Assumptions (assumpGT, assumpGC, assumpES, assumpSV,
  assumpGL, assumpBC, assumpRT, assumpLDFC, assumptionConstants,
  assumptions) where

import Language.Drasil hiding (organization)
import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)

import Data.Drasil.Concepts.Documentation as Doc (assumpDom, condition,
  constant, practice, reference, scenario, system, value)
import Data.Drasil.Concepts.Math (calculation, surface, shape)
import Data.Drasil.SentenceStructures (EnumType(Numb), FoldType(..), SepType(..),
  WrapType(Parens), foldlEnumList, foldlList, foldlSent, foldlSent_, sAnd, sIn, sOf)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty)

import Drasil.GlassBR.Concepts (beam, cantilever, edge, glaSlab, glass, gLassBR, 
  lShareFac, plane, responseTy)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (constantK, constantLoadDur, 
  constant_LoadSF, constantM, constantModElas, explosion, lateral, lDurFac,
  loadDur)

assumptions :: [ConceptInstance]
assumptions = [assumpGT, assumpGC, assumpES, assumpSV, assumpGL, assumpBC,
  assumpRT, assumpLDFC]

assumptionConstants :: [QDefinition]
assumptionConstants = [constantM, constantK, constantModElas,
  constantLoadDur, constant_LoadSF]

assumpGT, assumpGC, assumpES, assumpSV, assumpGL, assumpBC, assumpRT, assumpLDFC :: ConceptInstance
assumpGT           = cic "assumpGT"   glassTypeDesc                     "glassType"           Doc.assumpDom
assumpGC           = cic "assumpGC"   glassConditionDesc                "glassCondition"      Doc.assumpDom
assumpES           = cic "assumpES"   explainScenarioDesc               "explainScenario"     Doc.assumpDom
assumpSV           = cic "assumpSV"   (standardValuesDesc loadDur)     "standardValues"      Doc.assumpDom
assumpGL           = cic "assumpGL"   glassLiteDesc                     "glassLite"           Doc.assumpDom
assumpBC           = cic "assumpBC"   boundaryConditionsDesc            "boundaryConditions"  Doc.assumpDom
assumpRT           = cic "assumpRT"   responseTypeDesc                  "responseType"        Doc.assumpDom
assumpLDFC         = cic "assumpLDFC" (ldfConstantDesc lDurFac) "ldfConstant"         Doc.assumpDom

glassTypeDesc :: Sentence
glassTypeDesc = foldlSent [S "The standard E1300-09a for",
  phrase calculation, S "applies only to", foldlList Comma Options $ map S ["monolithic",
  "laminated", "insulating"], S "glass constructions" `sOf` S "rectangular", phrase shape, 
  S "with continuous", phrase lateral, S "support along",
  foldlList Comma Options (map S ["one", "two", "three", "four"]) +:+.
  plural edge, S "This", phrase practice +: S "assumes that",
  foldlEnumList Numb Parens SemiCol List $ map foldlSent_
  [[S "the supported glass", plural edge, S "for two, three" `sAnd`
  S "four-sided support", plural condition, S "are simply supported" `sAnd`
  S "free to slip in", phrase plane], 
  [S "glass supported on two sides acts as a simply supported", phrase beam], 
  [S "glass supported on one side acts as a", phrase cantilever]]]

glassConditionDesc :: Sentence
glassConditionDesc = foldlSent [S "Following", makeCiteS astm2009, sParen (S "pg. 1") `sC` 
  S "this", phrase practice, S "does not apply to any form of", foldlList Comma Options $ map S ["wired",
  "patterned", "etched", "sandblasted", "drilled", "notched", "grooved glass"], S "with", 
  phrase surface `sAnd` S "edge treatments that alter the glass strength"]

explainScenarioDesc :: Sentence
explainScenarioDesc = foldlSent [S "This", phrase system, S "only considers the external", 
  phrase explosion, phrase scenario, S "for its", plural calculation]

standardValuesDesc :: UnitaryChunk -> Sentence
standardValuesDesc mainIdea = foldlSent [S "The", plural value, S "provided in",
  makeRef2S $ SRS.valsOfAuxCons ([]::[Contents]) ([]::[Section]), S "are assumed for the", phrase mainIdea, 
  sParen (ch mainIdea) `sC` S "and the", plural materialProprty `sOf` 
  foldlList Comma List (map ch (take 3 assumptionConstants))]

glassLiteDesc :: Sentence
glassLiteDesc = foldlSent [at_start glass, S "under consideration is assumed to be a single", 
  S "lite; hence, the", phrase value `sOf` short lShareFac, S "is equal to 1 for all",
  plural calculation `sIn` short gLassBR]

boundaryConditionsDesc :: Sentence
boundaryConditionsDesc = foldlSent [S "Boundary", plural condition, S "for the",
  phrase glaSlab, S "are assumed to be 4-sided support for",
  plural calculation]

responseTypeDesc :: Sentence
responseTypeDesc = foldlSent [S "The", phrase responseTy, S "considered in",
  short gLassBR, S "is flexural"]

ldfConstantDesc :: QuantityDict -> Sentence
ldfConstantDesc mainConcept = foldlSent [S "With", phrase reference, S "to",
  makeRef2S assumpSV `sC` S "the", phrase value `sOf`
  phrase mainConcept, sParen (ch mainConcept), S "is a", phrase constant,
  S "in", short gLassBR]
