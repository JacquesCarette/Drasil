-- Standard code to make a table of units
-- First true example of a (small!) recipe.
module Drasil.Sections.TableOfUnits(tableOfUnits, unit_table) where

import Control.Lens ((^.))
import Prelude hiding (id)
import Language.Drasil
import Data.Drasil.Concepts.Documentation (symbol_, description)

-- | Table of units section builder. Takes a list of units and an introduction
tableOfUnits :: IsUnit s => [s] -> Contents -> Section
tableOfUnits u intro = Section (S "Table of Units") [Con intro, Con $ LlC (unit_table u)]
  (makeSecRef "ToU" "ToU")

-- | Creates the actual table of units from a list of units
unit_table :: IsUnit s => [s] -> LabelledContent
unit_table u = llcc (makeTabRef "ToU") $ Table
  (map (at_start) [symbol_, description])  (mkTable
  [Sy . usymb,
   (\x -> (x ^. defn) +:+ sParen (phrase x))
  ] u)
  (S "Table of Units") False
