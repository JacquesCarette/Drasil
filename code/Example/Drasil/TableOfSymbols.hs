-- Standard code to make a table of symbols.
module Drasil.TableOfSymbols 
  ( table_of_symbols
  , table
  , defnExcept
  , termExcept) where

import Control.Lens ((^.))

import Data.Maybe (isJust)
import Data.List (sort)

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import qualified Data.Drasil.Concepts.Math as CM (unit_)

table_of_symbols :: (Ord s,Quantity s) => 
  [s] -> (s -> Sentence) -> Section
table_of_symbols ls f = Section (titleize tOfSymb)
  [Con intro, Con (table (sort ls) f)]

intro :: Contents
intro = Paragraph $ 
  S "The table that follows summarizes the symbols used in this " :+:
  S "document along with their units.  The choice of symbols was " :+:
  S "made with the goal of being consistent with the nuclear " :+:
  S "physics literature and that used in the FP manual.  The SI " :+:
  S "units are listed in brackets following the definition of " :+:
  S "the symbol."
  
--Removed SymbolForm Constraint and filtered non-symbol'd chunks 
table :: (Quantity s) => [s] -> (s -> Sentence) -> Contents
table ls f = Table [at_start symbol_, at_start description, at_start' (CM.unit_ ^. term)] (mkTable
  [(\ch -> (\(Just t) -> P (t ^. symbol)) (getSymb ch)),
  (\ch -> f ch), 
  unit'2Contents]
  sls)
  (titleize tOfSymb) False
  where sls = filter (isJust . getSymb) ls --Don't use catMaybes
  
defnExcept :: (Eq s, Concept s) => [s] -> (s -> Sentence)
defnExcept xs x = if (x `elem` xs) then (phrase $ x ^. term) else (x ^. defn)
  
termExcept :: (Concept s, Eq s) => [s] -> (s -> Sentence)
termExcept xs x = if (x `elem` xs) then (x ^. defn) else (phrase $ x ^. term)
