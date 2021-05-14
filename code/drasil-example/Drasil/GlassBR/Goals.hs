module Drasil.GlassBR.Goals (goals, willBreakGS) where

import Language.Drasil
import Utils.Drasil
import Utils.Drasil.Sentence

import Data.Drasil.Concepts.Documentation (goalStmtDom, userInput)
import Data.Drasil.Concepts.Thermodynamics (degree_')

import Drasil.GlassBR.Concepts (glaSlab)
import Drasil.GlassBR.Unitals (explosion)

goals :: [ConceptInstance]
goals = [willBreakGS]

willBreakGS :: ConceptInstance
willBreakGS = cic "willBreakGS" (foldlSent [S "Analyze" `sAnd`
  S "predict whether the", phrase glaSlab, S "under consideration will be able",
  S "to withstand the", phrase explosion `sOf` S "a certain", phrase degree_',
  S "which is calculated based on", phrase userInput])
  "Predict-Glass-Withstands-Explosion" goalStmtDom