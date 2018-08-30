module Drasil.GlassBR.Labels where

import Language.Drasil

-- Assumptions
glassTypeL, glassConditionL, glassLiteL :: Label

glassTypeL      = mkLabelRAAssump' "glassType"
glassConditionL = mkLabelRAAssump' "glassCondition"
glassLiteL      = mkLabelRAAssump' "glassLite"

-- Instance Models
probOfBreakL, calOfCapacityL, calOfDemandL :: Label

probOfBreakL   = mkLabelSame "probOfBreak"   (Def Instance)
calOfCapacityL = mkLabelSame "calofCapacity" (Def Instance)
calOfDemandL   = mkLabelSame "calOfDemand"   (Def Instance)