-- Standard code to make a table of units
-- First true example of a (small!) recipe.
module Drasil.TableOfUnits(table_of_units, unit_table) where

import Control.Lens ((^.))
import Data.Char (toLower)
import Prelude hiding (id)
import Language.Drasil
import Data.Drasil.Concepts.Documentation

table_of_units :: Unit s => [s] -> Section
table_of_units u = Section (S "Table of Units") [Con s1_intro, Con (unit_table u)]

s1_intro :: Contents
s1_intro = Paragraph 
  (S "The unit system used throughout is SI (Syst" :+: (F Grave 'e') :+: 
   S "me International d'Unit" :+: (F Acute 'e') :+: 
   S "s). In addition to the basic units, several derived units are" :+: 
   S " also used. For each unit, the table lists the symbol," :+: 
   S " a description and the SI name.")

unit_table :: Unit s => [s] -> Contents
unit_table u = Table
  (map (at_start) [symbol_, description])  (mkTable
  [(\x -> Sy (x ^. usymb)),
   (\x -> (x ^. defn) +:+ S "(" :+: sMap (map toLower) (phrase $ x ^. term) :+: S ")")
  ] u)
  (S "Table of Units") False

