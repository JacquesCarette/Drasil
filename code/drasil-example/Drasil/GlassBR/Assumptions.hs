module Drasil.GlassBR.Assumptions (glassType, glassCondition, explainScenario, standardValues, 
  glassLite, boundaryConditions, responseType, ldfConstant, assumptionConstants, assumptions, 
  gbRefDB) where

import Language.Drasil hiding (organization)
import qualified Drasil.DocLang.SRS as SRS (valsOfAuxConsLabel)

import Data.Drasil.Concepts.Documentation as Doc (condition, constant, practice, 
  reference, scenario, system, value)
import Data.Drasil.Concepts.Math (calculation, surface, shape)
import Data.Drasil.SentenceStructures (EnumType(Numb), FoldType(..), SepType(..),
  WrapType(Parens), foldlEnumList, foldlList, foldlSent, foldlSent_, sAnd, sIn, sOf)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty)

import Drasil.GlassBR.Concepts (beam, cantilever, edge, glaSlab, glass, gLassBR, 
  lShareFac, plane, responseTy)
import Drasil.GlassBR.DataDefs (dimLL, loadDF, nonFL, tolStrDisFac)
import Drasil.GlassBR.Labels (probOfBreakL, calOfCapacityL, glassTypeL, glassConditionL, glassLiteL)
import Drasil.GlassBR.References (astm2009, gbCitations)
import Drasil.GlassBR.Unitals (constant_K, constant_LoadDF, constant_LoadDur, 
  constant_LoadSF, constant_M, constant_ModElas, explosion, lateral, lite, load_dur)

gbRefDB :: ReferenceDB
gbRefDB = rdb [] [] assumptions [] [] gbCitations []

assumptions :: [AssumpChunk] -- For testing
assumptions = [glassType, glassCondition, explainScenario, standardValues, glassLite, boundaryConditions, 
  responseType, ldfConstant]

assumptionConstants :: [QDefinition]
assumptionConstants = [constant_M, constant_K, constant_ModElas,
  constant_LoadDur, constant_LoadSF]

glassType, glassCondition, explainScenario, standardValues, glassLite, boundaryConditions, 
  responseType, ldfConstant :: AssumpChunk
glassType          = assump "glassTypeA"          glassTypeDesc                    glassTypeL
glassCondition     = assump "glassConditionA"     glassConditionDesc               glassConditionL
explainScenario    = assump "explainScenarioA"    explainScenarioDesc              (mkLabelRAAssump' "explainScenario"   ) 
standardValues     = assump "standardValuesA"    (standardValuesDesc load_dur)     (mkLabelRAAssump' "standardValues"    )
glassLite          = assump "glassLiteA"          glassLiteDesc                    glassLiteL
boundaryConditions = assump "boundaryConditionsA" boundaryConditionsDesc           (mkLabelRAAssump' "boundaryConditions")
responseType       = assump "responseTypeA"       responseTypeDesc                 (mkLabelRAAssump' "responseType"      )
ldfConstant        = assump "ldfConstantA"       (ldfConstantDesc constant_LoadDF) (mkLabelRAAssump' "ldfConstant"       )

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
glassConditionDesc = foldlSent [S "Following", makeRef astm2009, sParen (S "pg. 1") `sC` 
  S "this", phrase practice, S "does not apply to any form of", foldlList Comma Options $ map S ["wired",
  "patterned", "etched", "sandblasted", "drilled", "notched", "grooved glass"], S "with", 
  phrase surface `sAnd` S "edge treatments that alter the glass strength"]

explainScenarioDesc :: Sentence
explainScenarioDesc = foldlSent [S "This", phrase system, S "only considers the external", 
  phrase explosion, phrase scenario, S "for its", plural calculation]

standardValuesDesc :: UnitaryChunk -> Sentence
standardValuesDesc mainIdea = foldlSent [S "The", plural value, S "provided in",
  makeRef SRS.valsOfAuxConsLabel, S "are assumed for the", phrase mainIdea, 
  sParen (ch mainIdea) `sC` S "and the", plural materialProprty `sOf` 
  foldlList Comma List (map ch (take 3 assumptionConstants))] +:+ 
  (foldlList Comma List $ [makeRef probOfBreakL] ++ map makeRef [loadDF, 
  nonFL, dimLL, tolStrDisFac])

glassLiteDesc :: Sentence
glassLiteDesc = foldlSent [at_start glass, S "under consideration is assumed to be a single", 
  phrase lite `semiCol` S "hence, the", phrase value `sOf` short lShareFac, S "is equal to 1 for all",
  plural calculation `sIn` short gLassBR] +:+
  (foldlList Comma List $ [makeRef calOfCapacityL] ++ [makeRef dimLL])

boundaryConditionsDesc :: Sentence
boundaryConditionsDesc = foldlSent [S "Boundary", plural condition, S "for the",
  phrase glaSlab, S "are assumed to be 4-sided support for",
  plural calculation] +:+ makeRef probOfBreakL

responseTypeDesc :: Sentence
responseTypeDesc = foldlSent [S "The", phrase responseTy, S "considered in",
  short gLassBR, S "is flexural"] +:+ makeRef probOfBreakL

ldfConstantDesc :: QDefinition -> Sentence
ldfConstantDesc mainConcept = foldlSent [S "With", phrase reference, S "to",
  makeRef standardValues `sC` S "the", phrase value `sOf`
  phrase mainConcept, sParen (ch mainConcept), S "is a", phrase constant,
  S "in", short gLassBR] +:+ makeRef loadDF
