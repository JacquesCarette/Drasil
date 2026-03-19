module Drasil.BinaryStar.DataDefs (dataDefs, sepDistDD) where

import Language.Drasil
import Theory.Drasil (DataDefinition, ddENoRefs)

import Drasil.BinaryStar.Expressions (sepDistExpr)
import Drasil.BinaryStar.Unitals (sepDist)

dataDefs :: [DataDefinition]
dataDefs = [sepDistDD]

---------------------------------------------------------
-- DD: Separation distance
-- r₁₂ = sqrt((x₁ - x₂)² + (y₁ - y₂)²)
---------------------------------------------------------
sepDistDD :: DataDefinition
sepDistDD = ddENoRefs sepDistQD Nothing "sepDistDD"
  [S "The separation distance between the two stars"]

sepDistQD :: SimpleQDef
sepDistQD = mkQuantDef sepDist sepDistExpr
