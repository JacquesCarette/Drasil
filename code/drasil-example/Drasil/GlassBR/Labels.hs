module Drasil.GlassBR.Labels where

import Language.Drasil

-- Assumptions
glassTypeL, glassConditionL, glassLiteL :: Reference

glassTypeL      = makeAssumpRef "glassType"
glassConditionL = makeAssumpRef "glassCondition"
glassLiteL      = makeAssumpRef "glassLite"
