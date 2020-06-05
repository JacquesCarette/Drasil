-- Standard code to make a table of symbols.
module Drasil.Sections.TableOfSymbols (table, symbTableRef) where

import Language.Drasil
import Data.List (nub, (\\))
import Drasil.DocumentLanguage.Units (toSentence)
import Data.Drasil.Concepts.Documentation (symbol_, description, tOfSymb)
import Data.Drasil.Concepts.Math (unit_)
import Control.Lens (view)
import Language.Drasil.Printers (symbolDoc)
import Text.PrettyPrint.HughesPJ (render)

--Removed SymbolForm Constraint and filtered non-symbol'd chunks 
-- | table of symbols creation function
table :: (Quantity s, MayHaveUnit s) => Stage -> [s] -> (s -> Sentence) -> LabelledContent
table st ls f
    |noDuplicate = llcc symbTableRef $
      Table [atStart symbol_, atStart description, atStart' unit_]
      (mkTable [P . (`symbol` st), f, toSentence]
      $ filteredChunks)
      (titleize tOfSymb) True
    | otherwise = error $ "Same symbols for different quantities found "  ++ show symUidPairDuplicates
    where 
        filteredChunks = filter (`hasStageSymbol`st) ls
        symbolsCol     = map (`symbol` st) filteredChunks
        uidCol         = map (view uid)    filteredChunks
        symUidPair     = zip symbolsCol uidCol
        symDuplicates  = symbolsCol \\ nub symbolsCol
        noDuplicate    = null symDuplicates
        symUidPairDuplicates = [(render . symbolDoc $ fst x, snd x) | x <- symUidPair, 
          fst x `elem` symDuplicates]

symbTableRef :: Reference
symbTableRef = makeTabRef "ToS"