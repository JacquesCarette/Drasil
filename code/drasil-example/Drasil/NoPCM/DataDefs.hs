module Drasil.NoPCM.DataDefs where --exports all of it

import Language.Drasil
import Theory.Drasil (DataDefinition)

import Drasil.SWHS.DataDefs (dd1HtFluxC, dd1HtFluxCQD)

qDefs :: [QDefinition]
qDefs = [dd1HtFluxCQD]

dataDefs :: [DataDefinition] 
dataDefs = [dd1HtFluxC]