-- Standard code to make a table of symbols.
module Drasil.Sections.TableOfSymbols (table, symbTableRef) where

import Language.Drasil

import Data.List (nub, (\\))
import Control.Lens (view)
import Text.PrettyPrint.HughesPJ (text, render, vcat, (<+>))

import Drasil.DocumentLanguage.Units (toSentence)
import Data.Drasil.Concepts.Documentation (symbol_, description, tOfSymb)
import Data.Drasil.Concepts.Math (unit_)
import Language.Drasil.Printers (symbolDoc)

--Removed SymbolForm Constraint
-- | Table of Symbols creation function. Takes in a 'Stage', 'Symbol's, and something that turns
-- the symbols into a 'Sentence'. Filters non-symbol chunks and checks for duplicate symbol error.
table :: (Quantity s, MayHaveUnit s) => Stage -> [s] -> (s -> Sentence) -> LabelledContent
table st ls f
    |noDuplicate = llcc symbTableRef $
      Table [atStart symbol_, atStart description, atStart' unit_]
      (mkTable [P . (`symbol` st), f, toSentence] filteredChunks)
      (titleize tOfSymb) True
    | otherwise = error errorMessage 
    where 
        filteredChunks = filter (`hasStageSymbol`st) ls
        symbolsCol     = map (`symbol` st) filteredChunks
        uidCol         = map (view uid)    filteredChunks
        symUidPair     = zip symbolsCol uidCol
        symDuplicates  = nub (symbolsCol \\ nub symbolsCol)
        noDuplicate    = null symDuplicates
        -- If there are duplicates then the following will extract the UID's of duplicates symbols
        extractPairs symb = filter (\x -> fst x == symb) symUidPair
        extractUid  = map snd
        extractUidFromPairs = text . show . extractUid . extractPairs
        errSymUidDuplicates = vcat $ map (\symb -> 
          extractUidFromPairs symb<+>text "all have symbol"<+>symbolDoc symb) symDuplicates
        errorMessage = "Same symbols for different quantities found: " ++ render errSymUidDuplicates

-- | Makes a reference to the Table of Symbols.
symbTableRef :: Reference
symbTableRef = makeTabRef "ToS"