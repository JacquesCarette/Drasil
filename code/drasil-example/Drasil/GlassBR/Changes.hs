module Drasil.GlassBR.Changes (likelyChanges_SRS, unlikelyChanges_SRS) where

--A list of likely and unlikely changes for GlassBR

import Language.Drasil
import Drasil.DocLang (mkLklyChnk, mkUnLklyChnk, refA)

import Drasil.GlassBR.Assumptions (assumptionConstants, gbRefDB, newA2, newA3, newA4, newA5, newA6, newA7, newA8)
import Drasil.GlassBR.Concepts (blastRisk, glaSlab, glass)
import Drasil.GlassBR.Unitals (explosion, lite)

import Data.Drasil.Concepts.Documentation (condition, goal, input_, software, 
  system, value, variable)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.PhysicalProperties (flexure)
import Data.Drasil.SentenceStructures (foldlList, foldlSent)

{--LIKELY CHANGES--}

likelyChanges_SRS :: [Contents]
likelyChanges_SRS = [likelychg1, likelychg2, likelychg3,
  likelychg4, likelychg5]

likelychg1, likelychg2, likelychg3, likelychg4,
  likelychg5 :: Contents

likelychg1 = mkLklyChnk "likelychg1" (lc1Desc (blastRisk)) "Calculate-Internal-Blask-Risk"
likelychg2 = mkLklyChnk "likelychg2" (lc2Desc) "Variable-Values-of-m,k,E"
likelychg3 = mkLklyChnk "likelychg3" (lc3Desc) "Accomodate-More-than-Single-Lite"
likelychg4 = mkLklyChnk "likelychg4" (lc4Desc) "Accomodate-More-Boundary-Conditions"
likelychg5 = mkLklyChnk "likelychg5" (lc5Desc) "Consider-More-than-Flexure-Glass"

lc1Desc :: NamedChunk -> Sentence
lc2Desc, lc3Desc, lc4Desc, lc5Desc :: Sentence

lc1Desc mainConcept = foldlSent [(refA gbRefDB newA3) `sDash` S "The",
  phrase system, S "currently only calculates for external" +:+.
  phrase mainConcept, S "In the future,", plural calculation,
  S "can be added for the internal", phrase mainConcept]

lc2Desc = foldlSent [(refA gbRefDB newA4) `sC` ((refA gbRefDB newA8) `sDash`
  S "Currently, the"), plural value, S "for",
  foldlList (map ch (take 3 assumptionConstants)),
  S "are assumed to be the same for all" +:+. phrase glass,
  S "In the future, these", plural value, S "can be changed to",
  phrase variable, plural input_]

lc3Desc = foldlSent [(refA gbRefDB newA5) `sDash` S "The", phrase software,
  S "may be changed to accommodate more than a single", phrase lite]

lc4Desc = foldlSent [(refA gbRefDB newA6) `sDash` S "The", phrase software,
  S "may be changed to accommodate more boundary", plural condition,
  S "than 4-sided support"]

lc5Desc = foldlSent [(refA gbRefDB newA7) `sDash` S "The", phrase software,
  S "may be changed to consider more than just", phrase flexure,
  S "of the glass"]

{--UNLIKELY CHANGES--}

unlikelyChanges_SRS :: [Contents]
unlikelyChanges_SRS = [unlikelychg1, unlikelychg2]

unlikelychg1, unlikelychg2 :: Contents

unlikelychg1 = mkUnLklyChnk "unlikelychg1" (uc1Desc) "Predict-Withstanding-of-Certain-Degree"
unlikelychg2 = mkUnLklyChnk "unlikelychg2" (uc2Desc) "Accommodate-Altered-Glass"

uc1Desc, uc2Desc :: Sentence

uc1Desc = foldlSent [S "The", phrase goal, S "of the", phrase system,
  S "is to predict whether the", phrase glaSlab, S "under consideration can",
  S "withstand an", phrase explosion, S "of a certain degree"]

uc2Desc = foldlSent [(refA gbRefDB newA2), S "requires that the", phrase glass +:+.
  S "is not altered in any way", S "Therefore, this cannot be used on altered",
  phrase glass]
