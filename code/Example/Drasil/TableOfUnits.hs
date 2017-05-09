-- Standard code to make a table of units
-- First true example of a (small!) recipe.
module Drasil.TableOfUnits(table_of_units, unit_table) where

import Control.Lens ((^.))
import Data.Char (toLower)
import Prelude hiding (id)
import Language.Drasil
import Data.Drasil.Concepts.Documentation

table_of_units :: Unit s => [s] -> Contents -> Section
table_of_units u intro = Section (S "Table of Units") [Con intro, Con (unit_table u)]

unit_table :: Unit s => [s] -> Contents
unit_table u = Table
  (map (at_start) [symbol_, description])  (mkTable
  [(\x -> Sy (x ^. usymb)),
   (\x -> (x ^. defn) +:+ S "(" :+: sMap (map toLower) (phrase $ x ^. term) :+: S ")")
  ] u)
  (S "Table of Units") False

