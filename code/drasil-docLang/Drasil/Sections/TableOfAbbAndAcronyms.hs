-- | Standard code to make a table of symbols.
module Drasil.Sections.TableOfAbbAndAcronyms
  (tableOfAbbAndAcronyms, tableAbbAccRef, tableAbbAccLabel) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (abbreviation, fullForm, abbAcc)

import Control.Lens ((^.))
import Data.List (sortBy)
import Data.Function (on)
-- | Creates a standard table of abbreviations and acronyms section from a
-- given list of abbreviated chunks.
tableOfAbbAndAcronyms :: (Idea s) => [s] -> Section
tableOfAbbAndAcronyms ls = Section (titleize abbAcc)
  [Con (LlC $ table ls)] tableAbbAccLabel

-- Will be changed when shortname is changed.
--FIXME: Temporary until shortname can take a sentence. 
-- Same problem as in Drasil.DocLang.SRS, see issue #2489 for details.
toString :: Sentence -> String
toString (S x) = x
toString ((:+:) s1 s2) = toString s1 ++ toString s2
toString _ = error "Term is not a string"

-- | Helper function that gets the acronym out of an 'Idea'.
select :: (Idea s) => [s] -> [(String, s)]
select [] = []
select (x:xs) = case getA x of
  Nothing -> select xs
  Just y  -> (y, x) : select xs

-- | The actual table creation function.
table :: (Idea s) => [s] -> LabelledContent
table ls = let chunks = sortBy (compare `on` fst) $ select ls in
  llcc tableAbbAccRef $ Table
  (map titleize [abbreviation, fullForm]) (mkTable
  [\(a,_) -> S a,
   \(_,b) -> titleize b]
  chunks)
  (titleize abbAcc) True

-- | Table of abbreviations and acronyms UID and shortname.
tableAbbAccRef :: Reference
tableAbbAccRef = makeTabRef (abbAcc ^. uid)

-- should this be moved to DocLang/SRS?
-- | Reference for Table of Abbreviations and Acronyms.
tableAbbAccLabel :: Reference
tableAbbAccLabel = makeSecRef (abbAcc ^. uid) (toString (titleize abbAcc))

