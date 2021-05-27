module Drasil.GlassBR.Goals (goals, willBreakGS) where

import Language.Drasil
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Documentation (goalStmtDom, userInput)
import Data.Drasil.Concepts.Thermodynamics (degree_')

import Drasil.GlassBR.Concepts (glaSlab)
import Drasil.GlassBR.Unitals (explosion)

goals :: [ConceptInstance]
goals = [willBreakGS]

willBreakGS :: ConceptInstance
willBreakGS = cic "willBreakGS" (foldlSent [S "Analyze" `S.and_`
  S "predict whether the", phrase glaSlab, S "under consideration will be able",
  S "to withstand the", phrase explosion `S.of_` S "a certain", phrase degree_',
  S "which is calculated based on", phrase userInput])
  "Predict-Glass-Withstands-Explosion" goalStmtDom