module Drasil.SWHSNoPCM.Goals (goals, waterTempGS, waterEnergyGS) where

import Language.Drasil.Document (ConceptInstance)

import Drasil.SWHS.Goals (waterTempGS, waterEnergyGS)

goals :: [ConceptInstance]
goals = [waterTempGS, waterEnergyGS]
