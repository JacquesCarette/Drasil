-- | Standard code to make a table of symbols.
module Drasil.Sections.TableOfAbbAndAcronyms
  (tableOfAbbAndAcronyms) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (abbreviation, fullForm)

import Data.List (sortBy)
import Data.Function (on)
-- | Creates a standard table of abbreviations and acronyms section from a
-- given list of abbreviated chunks
tableOfAbbAndAcronyms :: (Idea s) => [s] -> Section
tableOfAbbAndAcronyms ls = Section (S "Abbreviations and Acronyms")
  [Con (LlC $ table ls)] (makeSecRef "TAbbAcc" "Abbreviations and Acronyms")


select :: (Idea s) => [s] -> [(String, s)]
select [] = []
select (x:xs) = case getA x of
  Nothing -> select xs
  Just y  -> (y, x) : select xs

-- | The actual table creation function.
table :: (Idea s) => [s] -> LabelledContent
table ls = let chunks = sortBy (compare `on` fst) $ select ls in
  llcc (makeTabRef "TAbbAcc") $ Table
  (map (titleize) [abbreviation, fullForm]) (mkTable
  [(\(a,_) -> S a),
   (\(_,b) -> titleize b)]
  chunks)
  (S "Abbreviations and Acronyms") False
