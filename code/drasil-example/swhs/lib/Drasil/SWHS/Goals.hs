module Drasil.SWHS.Goals (goals, waterTempGS, pcmTempGS, waterEnergyGS,
  pcmEnergyGS) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)
import Data.Drasil.Concepts.Physics (time)

import Drasil.SWHS.Unitals (tempW, tempPCM, watE, pcmE)

goals :: [ConceptInstance]
goals = [waterTempGS, pcmTempGS, waterEnergyGS, pcmEnergyGS]

waterTempGS :: ConceptInstance
waterTempGS = cic "waterTempGS" (goalState tempW) "Predict-Water-Temperature"
  goalStmtDom

pcmTempGS :: ConceptInstance
pcmTempGS = cic "pcmTempGS" (goalState tempPCM) "Predict-PCM-Temperature"
  goalStmtDom

waterEnergyGS :: ConceptInstance
waterEnergyGS = cic "waterEnergyGS" (goalState watE) "Predict-Water-Energy"
  goalStmtDom

pcmEnergyGS :: ConceptInstance
pcmEnergyGS = cic "pcmEnergyGS" (goalState pcmE) "Predict-PCM-Energy"
  goalStmtDom

goalState :: NamedIdea varTerm => varTerm -> Sentence
goalState varTerm = foldlSent [S "Predict the", phrase varTerm,
  S "over", phrase time]