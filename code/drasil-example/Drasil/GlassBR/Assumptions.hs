module Drasil.GlassBR.Assumptions where

import Language.Drasil hiding (organization)
import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons, missingP)

import Drasil.DocLang (cite, refA)

import Data.Drasil.Concepts.Documentation as Doc (condition, constant, practice, reference, scenario, 
  system, value)
import Data.Drasil.Concepts.Math (calculation, surface, shape)
import Data.Drasil.SentenceStructures (sAnd, foldlSent, foldlOptions, foldlList, sOf, sIn)
import Data.Drasil.Concepts.PhysicalProperties (materialProprty)

import Drasil.GlassBR.Unitals ( lite, explosion, lateral, load_dur, explosion,
  constant_LoadDur, constant_ModElas, constant_M, constant_K, constant_LoadDF, constant_LoadSF)
import Drasil.GlassBR.Concepts (lShareFac, gLassBR,
  glaSlab, glass, responseTy, cantilever, beam, plane, edge)
import Drasil.GlassBR.References (gbCitations, astm2009)
 
gbRefDB :: ReferenceDB
gbRefDB = rdb [] [] newAssumptions [] [] gbCitations []

assumptionConstants :: [QDefinition]
assumptionConstants = [constant_M, constant_K, constant_ModElas,
  constant_LoadDur, constant_LoadSF]

newAssumptions :: [AssumpChunk] -- For testing
newAssumptions = [newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8]

newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8 :: AssumpChunk
newA1 = assump "glassTyA" a1Desc "glassTy"
newA2 = assump "glassConditionA" a2Desc "glassCondition" 
newA3 = assump "explsnScenarioA"a3Desc "explainScenario" 
newA4 = assump "standardValuesA" (a4Desc load_dur) "StandardValues" 
newA5 = assump "glassLiteA" a5Desc "glassLite"
newA6 = assump "bndryConditionsA" a6Desc "boundaryConditions" 
newA7 = assump "responseTyA" a7Desc "responseType" 
newA8 = assump "ldfConstantA" (a8Desc constant_LoadDF) "ldfConstant"

assumptionDescs :: [Sentence]
assumptionDescs = [a1Desc, a2Desc, a3Desc, a4Desc load_dur, a5Desc, a6Desc, a7Desc, a8Desc constant_LoadDF]

a1Desc :: Sentence
a1Desc = foldlSent [S "The standard E1300-09a for",
  phrase calculation, S "applies only to", foldlOptions $ map S ["monolithic",
  "laminated", "insulating"], S "glass constructions" `sOf` S "rectangular",
  phrase shape, S "with continuous", phrase lateral +:+. S "support along",
  foldlOptions $ map S ["one", "two", "three", "four"], plural edge, S "This",
  phrase practice, S "assumes that", sParenNum 1, S "the supported glass",
  plural edge, S "for two, three" `sAnd` S "four-sided support",
  plural condition, S "are simply supported" `sAnd` S "free to slip in",
  phrase plane `semiCol` (sParenNum 2), S "glass supported on two sides acts",
  S "as a simply supported", phrase beam `sAnd` (sParenNum 3), S "glass",
  S "supported on one side acts as a", phrase cantilever]

a2Desc :: Sentence
a2Desc = foldlSent [S "Following", cite gbRefDB astm2009 +:+ sParen
  (S "pg. 1") `sC` S "this", phrase practice,
  S "does not apply to any form of", foldlOptions $ map S ["wired",
  "patterned", "etched", "sandblasted", "drilled", "notched", "grooved glass"],
  S "with", phrase surface `sAnd`
  S "edge treatments that alter the glass strength"]

a3Desc :: Sentence
a3Desc = foldlSent [S "This", phrase system,
  S "only considers the external", phrase explosion, phrase scenario,
  S "for its", plural calculation]

a4Desc :: UnitaryChunk -> Sentence
a4Desc mainIdea = foldlSent [S "The", plural value, S "provided in",
  makeRef (SRS.valsOfAuxCons SRS.missingP []), S "are assumed for the",
  phrase mainIdea, sParen (ch mainIdea) `sC` S "and the",
  plural materialProprty `sOf` foldlList (map ch
  (take 3 assumptionConstants))] +:+ S "[IM1, DD3, DD5, DD7, DD9]"

a5Desc :: Sentence
a5Desc = foldlSent [at_start glass, S "under consideration",
  S "is assumed to be a single", phrase lite `semiCol` S "hence, the",
  phrase value `sOf` short lShareFac, S "is equal to 1 for all",
  plural calculation `sIn` short gLassBR] +:+ S "[IM2, DD7]"

a6Desc :: Sentence
a6Desc = foldlSent [S "Boundary", plural condition, S "for the",
  phrase glaSlab, S "are assumed to be 4-sided support for",
  plural calculation] +:+ S "[IM1]"

a7Desc :: Sentence
a7Desc = foldlSent [S "The", phrase responseTy, S "considered in",
  short gLassBR, S "is flexural"] +:+ S "[IM1]"

a8Desc :: QDefinition -> Sentence
a8Desc mainConcept = foldlSent [S "With", phrase reference, S "to",
  (refA gbRefDB newA4) `sC` S "the", phrase value `sOf`
  phrase mainConcept, sParen (ch mainConcept), S "is a", phrase constant,
  S "in", short gLassBR] +:+ S "[DD3]"
