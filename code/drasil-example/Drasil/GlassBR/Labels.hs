module Drasil.GlassBR.Labels where

import Language.Drasil

-- Instance Models
probOfBrL, calOfCapL, calOfDemandL :: Label

probOfBrL    = mkLabelSame "probOfBr"    (Def Instance)
calOfCapL    = mkLabelSame "calOfCap"    (Def Instance)
calOfDemandL = mkLabelSame "calOfDemand" (Def Instance)

-- Assumptions
glassTypeL, glassConditionL, glassLiteL :: Label

glassTypeL      = mkLabelRAAssump' "glassType"
glassConditionL = mkLabelRAAssump' "glassCondition"
glassLiteL      = mkLabelRAAssump' "glassLite"