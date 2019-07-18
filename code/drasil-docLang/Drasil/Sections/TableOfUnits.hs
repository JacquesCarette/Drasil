-- Standard code to make a table of units
-- First true example of a (small!) recipe.
module Drasil.Sections.TableOfUnits (tableOfUnits, unitTableRef) where

import Control.Lens ((^.))
import Language.Drasil
import Data.Drasil.Concepts.Documentation (symbol_, description)

-- | Table of units section builder. Takes a list of units and an introduction
tableOfUnits :: IsUnit s => [s] -> Contents -> Section
tableOfUnits u intro = Section (S "Table of Units") [Con intro, Con $ LlC (unitTable u)]
  (makeSecRef "ToU" "ToU")

-- | Creates the actual table of units from a list of units
unitTable :: IsUnit s => [s] -> LabelledContent
unitTable u = llcc unitTableRef $ Table
  [atStart symbol_, atStart description, S "SI Name"]
  (mkTable [Sy . usymb, (^. defn), phrase] u)
  (S "Table of Units") False

unitTableRef :: Reference
unitTableRef = makeTabRef "ToU"
