-- Standard code to make a table of symbols.
module Drasil.TableOfSymbols 
  ( table
  , defnExcept
  , termExcept) where

import Control.Lens ((^.))

import Data.Maybe (isJust)

import Language.Drasil
import qualified Data.Drasil.Concepts.Math as CM
import Data.Drasil.Concepts.Documentation
 
--Removed SymbolForm Constraint and filtered non-symbol'd chunks 
-- | table of symbols creation function
table :: (Quantity s) => [s] -> (s -> Sentence) -> Contents
table ls f = Table [at_start symbol_, at_start description, at_start' (CM.unit_ ^. term)] (mkTable
  [(\ch -> (\(Just t) -> P (t ^. symbol)) (getSymb ch)),
  (\ch -> f ch), 
  unit'2Contents]
  sls)
  (titleize tOfSymb) False
  where sls = filter (isJust . getSymb) ls --Don't use catMaybes

-- | Gets the definitions of chunks for the table of symbols, except for the given
-- chunks where terms should be used instead.
defnExcept :: (Eq s, Concept s) => [s] -> (s -> Sentence)
defnExcept xs x = if (x `elem` xs) then (phrase $ x ^. term) else (x ^. defn)

-- | Gets the terms of chunks for the table of symbols, except for the given
-- chunks where definitions should be used instead.  
termExcept :: (Concept s, Eq s) => [s] -> (s -> Sentence)
termExcept xs x = if (x `elem` xs) then (x ^. defn) else (phrase $ x ^. term)
