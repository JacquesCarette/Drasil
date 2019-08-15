module Drasil.NoPCM.DataDefs where --exports all of it

import Language.Drasil
import Theory.Drasil (DataDefinition)

import Drasil.SWHS.DataDefs (balanceDecayRate, balanceDecayRateQD)

qDefs :: [QDefinition]
qDefs = [balanceDecayRateQD]

dataDefs :: [DataDefinition] 
dataDefs = [balanceDecayRate]
