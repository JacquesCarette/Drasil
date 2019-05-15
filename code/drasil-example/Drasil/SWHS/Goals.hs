module Drasil.SWHS.Goals (swhsGoals, waterTempGS, pcmTempGS, waterEnergyGS, 
  pcmEnergyGS) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)
import Data.Drasil.Concepts.Physics (time)
import Data.Drasil.SentenceStructures (foldlSent)

import Drasil.SWHS.Unitals (temp_W, temp_PCM, w_E, pcm_E)

swhsGoals :: [ConceptInstance]
swhsGoals = [waterTempGS, pcmTempGS, waterEnergyGS, pcmEnergyGS]

waterTempGS :: ConceptInstance
waterTempGS = cic "waterTempGS" (goalState temp_W) "Predict-Water-Temperature"
  goalStmtDom

pcmTempGS :: ConceptInstance
pcmTempGS = cic "pcmTempGS" (goalState temp_PCM) "Predict-PCM-Temperature" 
  goalStmtDom

waterEnergyGS :: ConceptInstance
waterEnergyGS = cic "waterEnergyGS" (goalState w_E) "Predict-Water-Energy"
  goalStmtDom

pcmEnergyGS :: ConceptInstance
pcmEnergyGS = cic "pcmEnergyGS" (goalState pcm_E) "Predict-PCM-Energy" 
  goalStmtDom

goalState :: NamedIdea varTerm => varTerm -> Sentence
goalState varTerm = foldlSent [S "Predict the", phrase varTerm,
  S "over", phrase time]