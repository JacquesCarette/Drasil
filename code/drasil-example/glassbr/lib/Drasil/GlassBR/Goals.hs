module Drasil.GlassBR.Goals (goals, willBreakGS) where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (goalStmtDom, userInput)

import Drasil.GlassBR.Concepts (glaSlab)
import Drasil.GlassBR.Unitals (explosion)
import Data.Drasil.Concepts.Education (degree_)

goals :: [ConceptInstance]
goals = [willBreakGS]

willBreakGS :: ConceptInstance
willBreakGS = cic "willBreakGS" (foldlSent [S "Analyze" `S.and_`
  S "predict whether the", phrase glaSlab, S "under consideration will be able",
  S "to withstand the", phrase explosion `S.of_` S "a certain", phrase degree_,
  S "which is calculated based on", phrase userInput])
  "Predict-Glass-Withstands-Explosion" goalStmtDom
