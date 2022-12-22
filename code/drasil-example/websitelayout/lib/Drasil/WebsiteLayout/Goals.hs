module Drasil.WebsiteLayout.Goals (goals, improveMaintainabilityGS) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (goalStmtDom)

-----------
-- Goals --
-----------

goals :: [ConceptInstance]
goals = [improveMaintainabilityGS]

improveMaintainabilityGS :: ConceptInstance
improveMaintainabilityGS = cic "improveCMaintainability" (S "Improve the maintainability of the website")
  "Improve-Maintainability" goalStmtDom