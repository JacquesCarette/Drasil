-- Standard code to make a table of units
-- First true example of a (small!) recipe.
module Drasil.Sections.TableOfUnits (tOfUnitDesc, tOfUnitSIName, unitTableRef) where

import Control.Lens ((^.))
import Language.Drasil
import Data.Drasil.Concepts.Documentation (symbol_, description)

-- | Creates the table of units with an "SI Name" column
tOfUnitSIName :: IsUnit s => [s] -> LabelledContent
tOfUnitSIName = tOfUnitHelper [atStart symbol_, atStart description, S "SI Name"]
                  [Sy . usymb, (^. defn), phrase]

-- | Creates the table of units with SI name in the "Description" column
tOfUnitDesc :: IsUnit s => [s] -> LabelledContent
tOfUnitDesc = tOfUnitHelper [atStart symbol_, atStart description]
                 [Sy . usymb, \x -> (x ^. defn) +:+ sParen (phrase x)]

-- | Helper for making Table of Units
tOfUnitHelper :: [Sentence] -> [s -> Sentence] -> [s] -> LabelledContent
tOfUnitHelper headers fs u = llcc unitTableRef $ Table headers
  (mkTable fs u) (S "Table of Units") False

unitTableRef :: Reference
unitTableRef = makeTabRef "ToU"
