module Drasil.GlassBR.Labels where

import Language.Drasil

probOfBrL, calOfCapL, calOfDemandL, glassLiteL :: Label

-- Instance Models
probOfBrL    = mkLabelSame "probOfBr"    (Def Instance)
calOfCapL    = mkLabelSame "calOfCap"    (Def Instance)
calOfDemandL = mkLabelSame "calOfDemand" (Def Instance)

-- Assumption
glassLiteL = mkLabelRAAssump' "glassLite"