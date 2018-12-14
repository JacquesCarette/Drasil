module Drasil.GlassBR.Labels where

import Language.Drasil

-- Assumptions
glassTypeL, glassConditionL, glassLiteL :: Reference

glassTypeL      = makeAssumpRef "glassType"
glassConditionL = makeAssumpRef "glassCondition"
glassLiteL      = makeAssumpRef "glassLite"

-- Instance Models
probOfBreakL, calOfCapacityL, calOfDemandL :: Label

probOfBreakL   = mkLabelSame "probOfBreak"   (Def Instance)
calOfCapacityL = mkLabelSame "calofCapacity" (Def Instance)
calOfDemandL   = mkLabelSame "calOfDemand"   (Def Instance)
