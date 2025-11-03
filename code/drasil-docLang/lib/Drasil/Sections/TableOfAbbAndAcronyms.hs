-- | Standard code to make a table of abbreviations and acronyms.
module Drasil.Sections.TableOfAbbAndAcronyms
  (tableAbbAccGen, tableAbbAccRef) where

import Control.Lens ((^.))
import Data.List (sortBy)
import Data.Function (on)

import Language.Drasil
import Language.Drasil.Development (toSent)
import Drasil.Database.SearchTools (TermAbbr (shortForm), longForm)
import Data.Drasil.Concepts.Documentation (abbreviation, fullForm, abbAcc)
import Drasil.Sections.ReferenceMaterial (emptySectSentPlu)
import Utils.Drasil (mkTable)

-- | Create a table of abbreviations from the given 'TermAbbr's. If the list is
-- empty, it will return a paragraph saying there are no abbreviations or
-- acronyms. It is assumed that the provided 'TermAbbr's are unique and all have
-- a short form.
tableAbbAccGen :: [TermAbbr] -> LabelledContent
tableAbbAccGen [] = llcc tableAbbAccRef $ Paragraph $ emptySectSentPlu [abbAcc]
tableAbbAccGen ls = let chunks = sortBy (compare `on` shortForm) ls in
  llcc tableAbbAccRef $ Table
  (map titleize [abbreviation, fullForm]) (mkTable
    [maybe EmptyS S . shortForm,
     toSent . titleizeNP . longForm]
  chunks)
  (titleize' abbAcc) True

-- | Table of abbreviations and acronyms reference.
tableAbbAccRef :: Reference
tableAbbAccRef = makeTabRef' $ abbAcc ^. uid
