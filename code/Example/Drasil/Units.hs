-- Standard code to make a table of units
-- First true example of a (small!) recipe.
module Example.Drasil.Units(table_of_units) where

import Control.Lens ((^.))
import Data.Char (toLower)

import Language.Drasil

table_of_units :: Unit s => [s] -> LayoutObj
table_of_units u = Section 0 (S "Table of Units") [s1_intro, s1_table u]

s1_intro :: LayoutObj
s1_intro = Paragraph 
  (S "Throughout this document SI (Syst" :+: (F Grave 'e') :+: 
   S "me International d'Unit" :+: (F Acute 'e') :+: 
   S "s) is employed as the unit system." :+:
   S " In addition to the basic units, several derived units are" :+: 
   S " employed as described below. For each unit, the symbol is" :+: 
   S " given followed by a description of the unit with the SI" :+: 
   S " name in parentheses.")

s1_table :: Unit s => [s] -> LayoutObj
s1_table u = Table [S "Symbol", S "Description"] (mkTable
  [(\x -> Sy (x ^. unit)),
   (\x -> (x ^. descr) :+: S (" (" ++ map toLower (x ^. name) ++ ")"))
  ] u)
  (S "Table of Units") False

