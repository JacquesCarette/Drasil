-- | Standard code to make a table of symbols.
module Drasil.Sections.TableOfAbbAndAcronyms
  ( table_of_abb_and_acronyms ) where

import Data.Maybe

import Language.Drasil
import Data.Drasil.Concepts.Documentation

-- | Creates a standard table of abbreviations and acronyms section from a
-- given list of abbreviated chunks
table_of_abb_and_acronyms :: (Ord s, Idea s) => [s] -> Section
table_of_abb_and_acronyms ls = Section (S "Abbreviations and Acronyms") 
  [Con (table ls)] "TAbbAcc"

--FIXME? Should it be called Symbol or something like Abbreviation/Acronym?
--FIXME? Should it be "Description" or "Term" or something else?
-- | The actual table creation function.
table :: (Idea s) => [s] -> Contents
table ls = let chunks = filter (isJust . getA) ls in
  Table (map (at_start) [symbol_, description]) (mkTable
  [(\ch -> maybe (error "should never happen") S (getA ch)) , 
   (\ch -> titleize ch)]
  chunks)
  (S "Abbreviations and Acronyms") False "TAbbAcc"
