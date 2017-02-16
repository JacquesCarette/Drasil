-- Standard code to make a table of symbols.
module Drasil.TableOfAbbAndAcronyms
  ( table_of_abb_and_acronyms ) where

import Control.Lens ((^.))

import Language.Drasil
import Data.Drasil.Concepts.Documentation

table_of_abb_and_acronyms :: (NamedIdea s) => [s] -> Section
table_of_abb_and_acronyms ls = Section (S "Abbreviations and Acronyms") 
  [Con (table ls)]
  
--FIXME? Should it be called Symbol or something like Abbreviation/Acronym?
--FIXME? Should it be "Description" or "Term" or something else?
table :: (NamedIdea s) => [s] -> Contents
table ls = Table (map (^.term) [symbol_,description]) (mkTable
  [(\ch -> getAcc ch) , 
   (\ch -> ch ^. term)]
  ls)
  (S "Abbreviations and Acronyms") False
