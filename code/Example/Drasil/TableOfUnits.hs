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
  (S "Throughout this document SI (Syst" :+: (F Grave 'e') :+: 
   S "me International d'Unit" :+: (F Acute 'e') :+: 
   S "s) is employed as the unit system." :+:
   S " In addition to the basic units, several derived units are" :+: 
   S " employed as described below. For each unit, the symbol is" :+: 
   S " given followed by a description of the unit with the SI" :+: 
   S " name in parentheses.")

unit_table :: Unit s => [s] -> Contents
unit_table u = Table
  (map (^.term) [symbol_, description])  (mkTable
  [(\x -> Sy (x ^. unit)),
   (\x -> (x ^. defn) :+: S " (" :+: sMap (map toLower) (x ^. term) :+: S ")")
  ] u)
  (S "Table of Units") False

