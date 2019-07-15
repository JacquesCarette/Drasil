module Drasil.NoPCM.DataDefs where --exports all of it

import Language.Drasil
import Theory.Drasil (DataDefinition)

import Drasil.SWHS.DataDefs (ddHtFluxC, ddHtFluxCQD)

qDefs :: [QDefinition]
qDefs = [ddHtFluxCQD]

dataDefs :: [DataDefinition] 
dataDefs = [ddHtFluxC]