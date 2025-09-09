-- | Standard code to make a table of abbreviations and acronyms.
module Drasil.Sections.TableOfAbbAndAcronyms
  (tableAbbAccGen, tableAbbAccRef) where

import Control.Lens ((^.))
import Data.Function (on)
import Data.List (sortBy)

import Data.Drasil.Concepts.Documentation (abbreviation, fullForm, abbAcc)
import Drasil.Database (HasUID(..))
import Drasil.Database.SearchTools(TermAbbr, longForm, shortForm, select)
import Drasil.Sections.ReferenceMaterial (emptySectSentPlu)
import Language.Drasil
import Utils.Drasil (mkTable)

-- | The actual table creation function.
tableAbbAccGen :: [TermAbbr] -> LabelledContent
tableAbbAccGen [] = llcc tableAbbAccRef $ Paragraph $ emptySectSentPlu [abbAcc]
tableAbbAccGen ls = let chunks = sortBy (compare `on` fst) $ select ls in
  llcc tableAbbAccRef $ Table
  (map titleize [abbreviation, fullForm]) (mkTable
    [\(a,_) -> S a,
     \(_,b) -> titleizeNP (longForm b)]
  chunks)
  (titleize' abbAcc) True

-- | Table of abbreviations and acronyms reference.
tableAbbAccRef :: Reference
tableAbbAccRef = makeTabRef' $ abbAcc ^. uid
