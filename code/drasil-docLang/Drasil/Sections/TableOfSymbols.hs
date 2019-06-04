-- Standard code to make a table of symbols.
module Drasil.Sections.TableOfSymbols (table) where

import Language.Drasil

import Drasil.DocumentLanguage.Units (toSentence)
import Data.Drasil.Concepts.Documentation (symbol_, description, tOfSymb)
import Data.Drasil.Concepts.Math (unit_)
 
--Removed SymbolForm Constraint and filtered non-symbol'd chunks 
-- | table of symbols creation function
table :: (Quantity s, MayHaveUnit s) => Stage -> [s] -> (s -> Sentence) -> LabelledContent
table st ls f = llcc (makeTabRef "ToS") $
  Table [at_start symbol_, at_start description, at_start' unit_]
  (mkTable [P . (`symbol` st), f, toSentence]
  $ filter (`hasStageSymbol`st) ls)
  (titleize tOfSymb) False
