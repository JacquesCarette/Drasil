module Drasil.NoPCM.Goals (goals, waterTempGS, waterEnergyGS, goalRefs) where

import Language.Drasil

import Drasil.SWHS.Goals (waterTempGS, waterEnergyGS)

goals :: [ConceptInstance]
goals = [waterTempGS, waterEnergyGS]

-- References --
goalRefs :: [Reference]
goalRefs = map rw goals