-- | Standard code to make a table of symbols.
module Drasil.Sections.TableOfSymbols (table, symbTableRef, tsymb, tsymb', tsymb'', tsIntro) where

import Language.Drasil hiding (Manual, Verb) -- Manual - Citation name conflict. FIXME: Move to different namespace
                                               -- Vector - Name conflict (defined in file)

import Data.List (nub, (\\))
import Control.Lens (view)
import Text.PrettyPrint.HughesPJ (text, render, vcat, (<+>))

import Drasil.Sections.ReferenceMaterial(emptySectSentPlu)

import Drasil.DocumentLanguage.Units (toSentence)
import Data.Drasil.Concepts.Documentation (symbol_, description, tOfSymb)
import Data.Drasil.Concepts.Math (unit_)
import Language.Drasil.Printers (symbolDoc)
import Drasil.DocumentLanguage.Core (Literature(..), TConvention(..), TSIntro(..), LFunc(..), RefTab(..))

--Removed SymbolForm Constraint
-- | Table of Symbols creation function. Takes in a 'Stage', 'Symbol's, and something that turns
-- the symbols into a 'Sentence'. Filters non-symbol chunks and checks for duplicate symbol error.
table :: (Quantity s, MayHaveUnit s) => Stage -> [s] -> (s -> Sentence) -> LabelledContent
table _ [] _ = llcc symbTableRef $ Paragraph EmptyS
table st ls f
    |noDuplicate = llcc symbTableRef $
      Table [atStart symbol_, atStart description, atStart' unit_]
      (mkTable [P . (`symbol` st), f, toSentence] filteredChunks)
      (titleize' tOfSymb) True
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

----- Table of symbols section helper functions -----

-- | Table of symbols constructor.
tsymb, tsymb' :: [TSIntro] -> RefTab
-- | Default is term and given introduction.
tsymb = TSymb
-- | Similar to 'tsymb', but has a default Defn for the LFunc type. Still has a given introduction.
tsymb' = TSymb' Defn

-- | Table of symbols constructor. Takes a custom function and introduction.
tsymb'' :: [TSIntro] -> LFunc -> RefTab
tsymb'' intro lfunc = TSymb' lfunc intro

-- | Table of symbols introduction builder. Used by 'mkRefSec'.
tsIntro :: [TSIntro] -> Contents
tsIntro [] = mkParagraph $ emptySectSentPlu [symbol_]
tsIntro x = mkParagraph $ foldr ((+:+) . tsI) EmptyS x

-- | Table of symbols intro writer. Translates a 'TSIntro' to a list in a 'Sentence'.
tsI :: TSIntro -> Sentence
tsI (TypogConvention ts) = typogConvention ts
tsI SymbOrder = S "The symbols are listed in alphabetical order."
tsI (SymbConvention ls) = symbConvention ls
tsI TSPurpose = S "The symbols used in this document are summarized in the" +:+
  namedRef symbTableRef (titleize' tOfSymb) +:+. S "along with their units"
tsI VectorUnits = S "For vector quantities, the units shown are for each component of the vector."

-- | Typographic convention writer. Translates a list of typographic conventions ('TConvention's)
-- to a 'Sentence'.
typogConvention :: [TConvention] -> Sentence
typogConvention [] = error "No arguments given for typographic conventions"
typogConvention ts = S "Throughout the document," +:+. foldlList Comma List (map tcon ts)
  where tcon (Vector emph) = S ("symbols in " ++ show emph ++
                                " will represent vectors, and scalars otherwise")
        tcon (Verb s) = s

-- | Symbolic convention writer.
symbConvention :: [Literature] -> Sentence
symbConvention [] = error "Attempting to reference no literature for SymbConvention"
symbConvention scs = S "The choice of symbols was made to be consistent with the" +:+.
                      makeSentence (map scon scs)
  where makeSentence [x,y] = x +:+ S "and with" +:+ y
        makeSentence xs    = foldlList Comma List xs
        scon (Lit x)       = phrase x +:+ S "literature"
        scon (Doc x)       = S "existing documentation for" +:+ phrase x
        scon (Doc' x)      = S "existing documentation for" +:+ plural x
        scon (Manual x)    = S "that used in the" +:+ phrase x +:+ S "manual"