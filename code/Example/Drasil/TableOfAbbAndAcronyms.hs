-- | Standard code to make a table of symbols.
module Drasil.TableOfAbbAndAcronyms
  ( table_of_abb_and_acronyms ) where

import Control.Lens ((^.))

import Language.Drasil
import Data.Drasil.Concepts.Documentation

-- | Creates a standard table of abbreviations and acronyms section from a
-- given list of abbreviated chunks
table_of_abb_and_acronyms :: (NamedIdea s) => [s] -> Section
table_of_abb_and_acronyms ls = Section (S "Abbreviations and Acronyms") 
  [Con (table ls)]
  
--FIXME? Should it be called Symbol or something like Abbreviation/Acronym?
--FIXME? Should it be "Description" or "Term" or something else?
-- | The actual table creation function.
table :: (NamedIdea s) => [s] -> Contents
table ls = Table (map (at_start) [symbol_, description]) (mkTable
  [(\ch -> getAcc ch) , 
   (\ch -> titleize $ ch ^. term)]
  ls)
  (S "Abbreviations and Acronyms") False
