-- Standard code to make a table of symbols.
module Drasil.Sections.TableOfSymbols 
  (table) where

import Language.Drasil
import qualified Data.Drasil.Concepts.Math as CM
import Data.Drasil.Concepts.Documentation (symbol_, description, tOfSymb)
 
--Removed SymbolForm Constraint and filtered non-symbol'd chunks 
-- | table of symbols creation function
table :: (Quantity s) => Stage -> [s] -> (s -> Sentence) -> Contents
table st ls f = Table 
  [at_start symbol_, at_start description, at_start' CM.unit_]
  (mkTable [P . (flip symbol st), f, unitToSentence]
  (filter (\q -> hasStageSymbol q st) ls))
  (titleize tOfSymb) False "ToS"

-- ^. defn
