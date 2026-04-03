-- | Standard code to make a table of abbreviations and acronyms.
module Drasil.Sections.TableOfAbbAndAcronyms
  (tableAbbAccGen, tableAbbAccRef) where

-- Generic Haskell
import Control.Lens ((^.))
import Data.Function (on)
import Data.List (sortBy)

-- Generic Drasil
import Language.Drasil
import Language.Drasil.Development (toSent)
import Drasil.Database (HasUID(..))
import Drasil.Database.SearchTools (TermAbbr (shortForm), longForm)
import Utils.Drasil (mkTable)

-- Vocabulary
import Drasil.Metadata.Documentation (abbreviation, fullForm, abbAcc)

-- other docLang
import Drasil.Sections.ReferenceMaterial (emptySectSentPlu)

-- | Create a table of abbreviations from the given 'TermAbbr's. If the list is
-- empty, it will return a paragraph saying there are no abbreviations or
-- acronyms. It is assumed that the provided 'TermAbbr's are unique and all have
-- a short form.
tableAbbAccGen :: [TermAbbr] -> LabelledContent
tableAbbAccGen [] = mkRawLC (Paragraph $ emptySectSentPlu [abbAcc]) tableAbbAccRef
tableAbbAccGen ls = let chunks = sortBy (compare `on` shortForm) ls in
  mkRawLC (Table
  (map titleize [abbreviation, fullForm]) (mkTable
    [maybe EmptyS S . shortForm,
     toSent . titleizeNP . longForm]
  chunks)
  (titleize' abbAcc) True) tableAbbAccRef

-- | Table of abbreviations and acronyms reference.
tableAbbAccRef :: Reference
tableAbbAccRef = makeTabRef' $ abbAcc ^. uid
