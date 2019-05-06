module Drasil.NoPCM.Goals (nopcmGoals) where

import Language.Drasil

import Drasil.SWHS.Goals (waterTempGS, waterEnergyGS)

nopcmGoals :: [ConceptInstance]
nopcmGoals = [waterTempGS, waterEnergyGS]