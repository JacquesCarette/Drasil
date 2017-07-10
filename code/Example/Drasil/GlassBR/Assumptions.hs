module Drasil.GlassBR.Assumptions where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (calculation, equation,
  surface)
import Prelude hiding (id)
import Data.Drasil.Utils
import Data.Drasil.SentenceStructures (foldlSent, sAnd,
  foldlList)

import Drasil.GlassBR.Unitals
import Drasil.GlassBR.Concepts
import Drasil.GlassBR.DataDefs

import Drasil.DocumentLanguage

assumption4_constants :: [QDefinition]
assumption4_constants = [constant_M, constant_K, constant_ModElas,
  constant_LoadDur]

s6_2_1_list :: [Contents]
s6_2_1_list = acroNumGen assumptions 1

assumptions :: [Contents]
assumptions = [assumption1, assumption2, assumption3, assumption4, assumption5,
  assumption6, assumption7, assumption8]

assumption1, assumption2, assumption3, assumption4, assumption5, assumption6,
  assumption7, assumption8 :: Contents
assumption1 = mkAssump "assumption1"   a1Desc              --glassTyAssumps
assumption2 = mkAssump "assumption2"   a2Desc              --glassCondition
assumption3 = mkAssump "assumption3"   a3Desc              --explsnScenario
assumption4 = mkAssump "assumption4"   (a4Desc (load_dur)) --standardValues
assumption5 = mkAssump "assumption5"   a5Desc              --glassLiteAssmp
assumption6 = mkAssump "assumption6"   a6Desc              --bndryConditions
assumption7 = mkAssump "assumption7"   a7Desc              --responseTyAssump
assumption8 = mkAssump "assumption8"   (a8Desc (loadDF))   --ldfConstant

a1Desc :: Sentence
a1Desc = foldlSent [S "The standard E1300-09a for",
  phrase calculation, S "applies only to monolithic, laminated, or insulating", 
  S "glass constructions of rectangular shape with continuous", 
  phrase lateral +:+. S "support along one, two, three, or four edges", 
  S "This", phrase practice, S "assumes that (1) the supported glass edges", 
  S "for two, three" `sAnd` S "four-sided support", plural condition,
  S "are simply supported and free to slip in plane; (2) glass supported on",
  S "two sides acts as a simply supported beam and (3) glass supported on", 
  S "one side acts as a", phrase cantilever]

a2Desc :: Sentence
a2Desc = foldlSent [S "Following", (sSqBr (S "4 (pg. 1)")) `sC`
  S "this", phrase practice,
  S "does not apply to any form of wired, patterned" `sC`
  S "etched, sandblasted, drilled, notched, or grooved glass with", 
  phrase surface `sAnd` S "edge treatments that alter the glass strength"]

a3Desc :: Sentence
a3Desc = foldlSent [S "This", phrase system,
  S "only considers the external", phrase explosion, phrase scenario,
  S "for its", plural calculation]

a4Desc :: UnitaryChunk -> Sentence
a4Desc mainIdea = foldlSent [S "The", plural value, S "provided in", 
  S "makeRef s10", S "are assumed for the", phrase mainIdea, --FIXME:Reference?
  sParen (getS mainIdea) `sC` S "and the", plural materialProprty,
  S "of", foldlList (map getS (take 3 assumption4_constants))]

a5Desc :: Sentence
a5Desc = foldlSent [S "Glass under consideration", 
  S "is assumed to be a single" +:+. phrase lite, S "Hence the", 
  phrase value, S "of", short lShareFac, S "is equal to 1 for all", 
  plural calculation, S "in", short gLassBR]

a6Desc :: Sentence
a6Desc = foldlSent [S "Boundary", plural condition, S "for the", 
  phrase glaSlab, S "is assumed to be 4-sided support for",
  plural calculation]

a7Desc :: Sentence
a7Desc = foldlSent [S "The response type considered in", short gLassBR,
  S "is flexural"]

a8Desc :: QDefinition -> Sentence
a8Desc mainConcept = foldlSent [S "With", phrase reference, S "to",
  acroA 4, S "the", phrase value, S "of", phrase mainConcept, 
  sParen (getS mainConcept), S "is a constant in" +:+. short gLassBR,
  S "It is calculated by the" +: phrase equation +:+. 
  E (C mainConcept := equat mainConcept), S "Using this" `sC`
  E ((C mainConcept) := (Dbl 0.27))]