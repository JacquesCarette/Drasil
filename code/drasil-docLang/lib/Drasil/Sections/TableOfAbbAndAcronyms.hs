-- | Standard code to make a table of abbreviations and acronyms.
module Drasil.Sections.TableOfAbbAndAcronyms
  (tableAbbAccGen, tableAbbAccRef) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (abbreviation, fullForm, abbAcc)

import Control.Lens ((^.))
import Data.List (sortBy)
import Data.Function (on)
import Drasil.Sections.ReferenceMaterial (emptySectSentPlu)
import Utils.Drasil (mkTable)

-- | Helper function that gets the acronym out of an 'Idea'.
select :: (Idea s) => [s] -> [(String, s)]
select [] = []
select (x:xs) = case getA x of
  Nothing -> select xs
  Just y  -> (y, x) : select xs

-- | The actual table creation function.
tableAbbAccGen :: (Idea s) => [s] -> LabelledContent
tableAbbAccGen [] = llcc tableAbbAccRef $ Paragraph $ emptySectSentPlu [abbAcc]
tableAbbAccGen ls = let chunks = sortBy (compare `on` fst) $ select ls in
  llcc tableAbbAccRef $ Table
  (map titleize [abbreviation, fullForm]) (mkTable
  [\(a,_) -> S a,
   \(_,b) -> titleize b]
  chunks)
  (titleize' abbAcc) True

-- | Table of abbreviations and acronyms reference.
tableAbbAccRef :: Reference
tableAbbAccRef = makeTabRef' $ abbAcc ^. uid


