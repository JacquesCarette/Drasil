-- | Standard code to make a table of abbreviations and acronyms.
module Drasil.Sections.TableOfAbbAndAcronyms
  (tableAbbAccGen, tableAbbAccRef) where

import Language.Drasil
import Database.Drasil(TermAbbr, longForm, shortForm)
import Data.Drasil.Concepts.Documentation (abbreviation, fullForm, abbAcc)

import Control.Lens ((^.))
import Data.List (sortBy)
import Data.Function (on)
import Drasil.Sections.ReferenceMaterial (emptySectSentPlu)
import Utils.Drasil (mkTable)

-- | Helper function that gets the acronym out of an 'Idea'.
select :: [TermAbbr] -> [(String, TermAbbr)]
select [] = []
select (x:xs) = case shortForm x of
  Nothing -> select xs
  Just y  -> (y, x) : select xs

-- | The actual table creation function.
tableAbbAccGen :: [TermAbbr] -> LabelledContent
tableAbbAccGen [] = llcc tableAbbAccRef $ Paragraph $ emptySectSentPlu [abbAcc]
tableAbbAccGen ls = let chunks = sortBy (compare `on` fst) $ select ls in
  llcc tableAbbAccRef $ Table
  (map titleize [abbreviation, fullForm]) (mkTable
    [\(a,_) -> maybe (S "") S (shortForm a),
     \(_,b) -> S (show (longForm b))]
  chunks)
  (titleize' abbAcc) True

-- | Table of abbreviations and acronyms reference.
tableAbbAccRef :: Reference
tableAbbAccRef = makeTabRef' $ abbAcc ^. uid