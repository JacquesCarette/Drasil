-- Standard code to make a table of units
-- First true example of a (small!) recipe.
module Drasil.Sections.TableOfUnits (tOfUnitSIName, unitTableRef) where

import Control.Lens ((^.))
import Language.Drasil
import Data.Drasil.Concepts.Documentation (symbol_, description)

-- | Creates the actual table of units from a list of units
tOfUnitSIName :: IsUnit s => [s] -> LabelledContent
tOfUnitSIName u = llcc unitTableRef $ Table
  [atStart symbol_, atStart description, S "SI Name"]
  (mkTable [Sy . usymb, (^. defn), phrase] u)
  (S "Table of Units") False

unitTableRef :: Reference
unitTableRef = makeTabRef "ToU"
