module Drasil.GlassBR.Assumptions (glassType, glassCondition, explainScenario, standardValues, 
  glassLite, boundaryConditions, responseType, ldfConstant, assumptionConstants,
  assumptions) where

import Language.Drasil hiding (organization)
import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)

import Data.Drasil.Concepts.Documentation as Doc (condition, constant,
  practice, reference, scenario, system, value)
import Data.Drasil.Concepts.Math (calculation, surface, shape)
import Data.Drasil.SentenceStructures (EnumType(Numb), FoldType(..), SepType(..),
  WrapType(Parens), foldlEnumList, foldlList, foldlSent, foldlSent_, sAnd, sIn, sOf)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty)

import Drasil.GlassBR.Concepts (beam, cantilever, edge, glaSlab, glass, gLassBR, 
  lShareFac, plane, responseTy)
import Drasil.GlassBR.Labels (glassTypeL, glassConditionL, glassLiteL)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (constant_K, constant_LoadDF, constant_LoadDur, 
  constant_LoadSF, constant_M, constant_ModElas, explosion, lateral, load_dur)

assumptions :: [AssumpChunk] -- For testing
assumptions = [glassType, glassCondition, explainScenario, standardValues, glassLite, boundaryConditions, 
  responseType, ldfConstant]

assumptionConstants :: [QDefinition]
assumptionConstants = [constant_M, constant_K, constant_ModElas,
  constant_LoadDur, constant_LoadSF]

glassType, glassCondition, explainScenario, standardValues, glassLite, boundaryConditions, 
  responseType, ldfConstant :: AssumpChunk

-- FIXME: Remove the AssumpChunks once ConceptInstance and SCSProg's
-- Assumptions has been migrated to using assumpDom

glassType          = assump glassTypeL glassTypeDesc
-- assumpGT           = cic "assumpGT"   glassTypeDesc                     "glassType"           Doc.assumpDom  -- FIXME: Use label once ConceptInstance migrates to them
glassCondition     = assump glassConditionL glassConditionDesc
-- assumpGC           = cic "assumpGC"   glassConditionDesc                "glassCondition"      Doc.assumpDom  -- FIXME: Use label once ConceptInstance migrates to them
explainScenario    = assump (mkLabelRAAssump' "explainScenario"   ) explainScenarioDesc
-- assumpES           = cic "assumpES"   explainScenarioDesc               "explainScenario"     Doc.assumpDom
standardValues     = assump (mkLabelRAAssump' "standardValues"    ) (standardValuesDesc load_dur)
-- assumpSV           = cic "assumpSV"   (standardValuesDesc load_dur)     "standardValues"      Doc.assumpDom
glassLite          = assump glassLiteL glassLiteDesc
-- assumpGL           = cic "assumpGL"   glassLiteDesc                     "glassLite"           Doc.assumpDom  -- FIXME: Use label once ConceptInstance migrates to them
boundaryConditions = assump (mkLabelRAAssump' "boundaryConditions") boundaryConditionsDesc
-- assumpBC           = cic "assumpBC"   boundaryConditionsDesc            "boundaryConditions"  Doc.assumpDom
responseType       = assump (mkLabelRAAssump' "responseType"      ) responseTypeDesc
-- assumpRT           = cic "assumpRT"   responseTypeDesc                  "responseType"        Doc.assumpDom
ldfConstant        = assump (mkLabelRAAssump' "ldfConstant"       ) (ldfConstantDesc constant_LoadDF)
-- assumpLDFC         = cic "assumpLDFC" (ldfConstantDesc constant_LoadDF) "ldfConstant"         Doc.assumpDom

glassTypeDesc :: Sentence
glassTypeDesc = foldlSent [S "The standard E1300-09a for",
  phrase calculation, S "applies only to", foldlList Comma Options $ map S ["monolithic",
  "laminated", "insulating"], S "glass constructions" `sOf` S "rectangular", phrase shape, 
  S "with continuous", phrase lateral, S "support along",
  (foldlList Comma Options $ map S ["one", "two", "three", "four"]) +:+. plural edge, S "This",
  phrase practice +: S "assumes that", (foldlEnumList Numb Parens SemiCol List $ map foldlSent_
  [[S "the supported glass", plural edge, S "for two, three" `sAnd` S "four-sided support",
  plural condition, S "are simply supported" `sAnd` S "free to slip in", phrase plane], 
  [S "glass supported on two sides acts as a simply supported", phrase beam], 
  [S "glass supported on one side acts as a", phrase cantilever]])]

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

ldfConstantDesc :: QDefinition -> Sentence
ldfConstantDesc mainConcept = foldlSent [S "With", phrase reference, S "to",
  makeRef2S standardValues `sC` S "the", phrase value `sOf`
  phrase mainConcept, sParen (ch mainConcept), S "is a", phrase constant,
  S "in", short gLassBR]
