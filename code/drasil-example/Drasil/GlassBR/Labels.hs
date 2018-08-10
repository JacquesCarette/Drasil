module Drasil.GlassBR.Labels where

import Language.Drasil

probOfBrL, calOfCapL, calOfDemandL :: Label
probOfBrL    = mkLabelSame "probOfBr" (Def Instance)
calOfCapL    = mkLabelSame "calOfCap" (Def Instance)
calOfDemandL = mkLabelSame "calOfDemand" (Def Instance)