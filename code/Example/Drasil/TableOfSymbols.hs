-- Standard code to make a table of symbols.
module Drasil.TableOfSymbols 
  ( table_of_symbols
  , table
  , defnExcept
  , termExcept) where

import Control.Lens ((^.))

import Language.Drasil
import Data.Drasil.Concepts.Documentation

table_of_symbols :: (Quantity s, SymbolForm s) => 
  [s] -> (s -> Sentence) -> Section
table_of_symbols ls f = Section (tOfSymb ^. term)
  [Con intro, Con (table ls f)]

intro :: Contents
intro = Paragraph $ 
  S "The table that follows summarizes the symbols used in this " :+:
  S "document along with their units.  The choice of symbols was " :+:
  S "made with the goal of being consistent with the nuclear " :+:
  S "physics literature and that used in the FP manual.  The SI " :+:
  S "units are listed in brackets following the definition of " :+:
  S "the symbol."
  
table :: (Quantity s, SymbolForm s) => [s] -> (s -> Sentence) -> Contents
table ls f = Table (map (^.term) [symbol_, description, units_]) (mkTable
  [(\ch -> P (ch ^. symbol)) , 
   (\ch -> f ch), 
   unit'2Contents]
  ls)
  (tOfSymb ^. term) False
  
defnExcept :: (Eq s, Concept s) => [s] -> (s -> Sentence)
defnExcept xs x = if (x `elem` xs) then (x ^. term) else (x ^. defn)
  
termExcept :: (Concept s, Eq s) => [s] -> (s -> Sentence)
termExcept xs x = if (x `elem` xs) then (x ^. defn) else (x ^. term)
