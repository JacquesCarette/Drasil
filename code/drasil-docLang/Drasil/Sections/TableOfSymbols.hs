-- Standard code to make a table of symbols.
module Drasil.Sections.TableOfSymbols (table, symbTableRef) where

import Language.Drasil

import Data.List (nub, (\\))
import Control.Lens (view)
import Data.Bifunctor (first)
import Text.PrettyPrint.HughesPJ (render)

import Drasil.DocumentLanguage.Units (toSentence)
import Data.Drasil.Concepts.Documentation (symbol_, description, tOfSymb)
import Data.Drasil.Concepts.Math (unit_)
import Language.Drasil.Printers (symbolDoc)

--Removed SymbolForm Constraint, filtered non-symbol'd chunks and checks for duplicate symbol error
-- | table of symbols creation function
table :: (Quantity s, MayHaveUnit s) => Stage -> [s] -> (s -> Sentence) -> LabelledContent
table st ls f
    |noDuplicate = llcc symbTableRef $
      Table [atStart symbol_, atStart description, atStart' unit_]
      (mkTable [P . (`symbol` st), f, toSentence] filteredChunks)
      (titleize tOfSymb) True
    | otherwise = error $ "Same symbols for different quantities found "  ++ show symUidPairDuplicates
    where 
        filteredChunks = filter (`hasStageSymbol`st) ls
        symbolsCol     = map (`symbol` st) filteredChunks
        uidCol         = map (view uid)    filteredChunks
        symUidPair     = zip symbolsCol uidCol
        symDuplicates  = symbolsCol \\ nub symbolsCol
        noDuplicate    = null symDuplicates
        symUidPairDuplicates = [(first (render . symbolDoc) x, snd x) | x <- symUidPair, 
          fst x `elem` symDuplicates]

symbTableRef :: Reference
symbTableRef = makeTabRef "ToS"