module Drasil.NoPCM.Goals (nopcmGoals, waterTempGS, waterEnergyGS) where

import Language.Drasil

import Drasil.SWHS.Goals (waterTempGS, waterEnergyGS)

nopcmGoals :: [ConceptInstance]
nopcmGoals = [waterTempGS, waterEnergyGS]