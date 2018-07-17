-- Standard code to make a table of units
-- First true example of a (small!) recipe.
module Drasil.Sections.TableOfUnits(table_of_units, unit_table) where

import Control.Lens ((^.))
import Prelude hiding (id)
import Language.Drasil
import Data.Drasil.Concepts.Documentation (symbol_, description)

-- | Table of units section builder. Takes a list of units and an introduction
table_of_units :: IsUnit s => [s] -> LabelledContent -> Section
table_of_units u intro = Section (S "Table of Units") [LCon intro, LCon (unit_table u)] 
  (mkLabelRA'' "Section:ToU") --"TblOfUnits"

-- | Creates the actual table of units from a list of units
unit_table :: IsUnit s => [s] -> LabelledContent
unit_table u = llcc "ToULC" (mkLabelRA'' "ToU") $ Table
  (map (at_start) [symbol_, description])  (mkTable
  [(\x -> Sy (x ^. usymb)),
   (\x -> (x ^. defn) +:+ sParen (phrase x))
  ] u)
  (S "Table of Units") False "ToU"