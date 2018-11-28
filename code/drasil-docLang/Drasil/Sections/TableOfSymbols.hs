-- Standard code to make a table of symbols.
module Drasil.Sections.TableOfSymbols 
  (table) where

import Language.Drasil
import Language.Drasil.Development (MayHaveUnit)

import qualified Data.Drasil.Concepts.Math as CM
import Drasil.DocumentLanguage.Units (toSentence)
import Data.Drasil.Concepts.Documentation (symbol_, description, tOfSymb)
 
--Removed SymbolForm Constraint and filtered non-symbol'd chunks 
-- | table of symbols creation function
table :: (Quantity s, MayHaveUnit s) => Stage -> [s] -> (s -> Sentence) -> LabelledContent
table st ls f = llcc (mkLabelSame "ToS" Tab) 
  $ Table "fixme"
  [at_start symbol_, at_start description, at_start' CM.unit_]
  (mkTable [P . (flip symbol st), f, toSentence]
  (filter (\q -> hasStageSymbol q st) ls))  
  (titleize tOfSymb) False
