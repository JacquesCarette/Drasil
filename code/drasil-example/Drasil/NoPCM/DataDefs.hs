module Drasil.NoPCM.DataDefs where --exports all of it

import Language.Drasil
import Theory.Drasil (DataDefinition)

import Drasil.SWHS.DataDefs (ddHtFluxC, ddHtFluxCQD, balanceDecayRate, balanceDecayRateQD)

qDefs :: [QDefinition]
qDefs = [ddHtFluxCQD, balanceDecayRateQD]

dataDefs :: [DataDefinition] 
dataDefs = [ddHtFluxC, balanceDecayRate]
