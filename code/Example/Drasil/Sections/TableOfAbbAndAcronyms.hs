-- | Standard code to make a table of symbols.
module Drasil.Sections.TableOfAbbAndAcronyms
  ( table_of_abb_and_acronyms ) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation

import Data.List (sortBy)
import Data.Function (on)

-- | Creates a standard table of abbreviations and acronyms section from a
-- given list of abbreviated chunks
table_of_abb_and_acronyms :: (Idea s) => [s] -> Section
table_of_abb_and_acronyms ls = Section (S "Abbreviations and Acronyms") 
  [Con (table ls)] "TAbbAcc" "TblOfAA"

select :: (Idea s) => [s] -> [(String, s)]
select [] = []
select (x:xs) = case getA x of
  Nothing -> select xs
  Just y  -> (y, x) : select xs

--FIXME? Should it be called Symbol or something like Abbreviation/Acronym?
--FIXME? Should it be "Description" or "Term" or something else?
-- | The actual table creation function.
table :: (Idea s) => [s] -> Contents
table ls = let chunks = sortBy (compare `on` fst) $ select ls in
  Table (map (at_start) [symbol_, description]) (mkTable
  [(\(a,_) -> S a),
   (\(_,b) -> titleize b)]
  chunks)
  (S "Abbreviations and Acronyms") False "TAbbAcc"
