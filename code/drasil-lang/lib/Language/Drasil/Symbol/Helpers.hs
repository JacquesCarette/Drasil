-- | Routines to help with Symbols and Stages.
module Language.Drasil.Symbol.Helpers(eqSymb, codeSymb, hasStageSymbol,
  autoStage, hat, prime, staged, sub, subStr, sup, unicodeConv, upperLeft,
  vec, label, variable, sortBySymbol, sortBySymbolTuple, compareBySymbol) where

import Data.Char (isLatin1, toLower)
import Data.Char.Properties.Names (getCharacterName)
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)

import Language.Drasil.Symbol (HasSymbol(symbol), Symbol(..), Decoration(..), compsy)
import Language.Drasil.Stages (Stage(Equational,Implementation))

-- | Helper for creating smart constructors for 'Symbol's.
neSymb :: (String -> Symbol) -> String -> String -> Symbol
neSymb _  s [] = error $ s ++ " names must be non-empty"
neSymb sy _ s  = sy s

-- | Label smart constructor, requires non-empty labels
label :: String -> Symbol
label = neSymb Label "label"

-- | Variable smart constructor, requires non-empty variables
variable :: String -> Symbol
variable = neSymb Variable "variable"

-- | Helper function for getting a symbol in the Equational Stage.
eqSymb :: HasSymbol q => q -> Symbol
eqSymb c = symbol c Equational

-- | Helper function for getting a symbol in the Implementation Stage.
codeSymb :: HasSymbol q => q -> Symbol
codeSymb c = symbol c Implementation

-- | Finds if a 'Stage' symbol is real or Empty. True if real.
hasStageSymbol :: HasSymbol q => q -> Stage -> Bool
hasStageSymbol q st = symbol q st /= Empty

-- | Helper for creating a symbol with a superscript on the left side of the symbol.
-- Arguments: Base symbol, then superscripted symbol.
upperLeft :: Symbol -> Symbol -> Symbol
upperLeft b ul = Corners [ul] [] [] [] b

-- | Helper for creating a symbol with a subscript to the right.
-- Arguments: Base symbol, then subscripted symbol.
sub :: Symbol -> Symbol -> Symbol
sub b lr = Corners [] [] [] [lr] b

-- | Helper for a common case of subscript, with a string
-- Arguments: Base symbol, then subscript 'String'.
subStr :: Symbol -> String -> Symbol
subStr sym substr = sub sym $ Label substr

-- | Helper for creating a symbol with a superscript to the right.
-- Arguments: Base symbol, then superscripted symbol.
sup :: Symbol -> Symbol -> Symbol
sup b ur = Corners [] [] [ur] [] b

-- | Helper for creating a symbol with a hat ("^") atop it.
hat :: Symbol -> Symbol
hat = Atop Hat

-- | Helper for creating a Vector symbol.
vec :: Symbol -> Symbol
vec = Atop Vector

-- | Helper for creating a Vector symbol.
prime :: Symbol -> Symbol
prime = Atop Prime

-- | Helper for creating a symbol that depends on the stage.
staged :: Symbol -> Symbol -> Stage -> Symbol
staged eqS _ Equational = eqS
staged _ impS Implementation = impS

-- | Helper for creating a symbol with Unicode in it.
autoStage :: Symbol -> (Stage -> Symbol)
autoStage s = staged s (unicodeConv s)

-- | Helper for autoStage that applies unicodeString to all 'Symbol's with 'String's.
unicodeConv :: Symbol -> Symbol
unicodeConv (Variable st) = Variable $ unicodeString st
unicodeConv (Label    st) = Label    $ unicodeString st
unicodeConv (Atop    d s) = Atop d   $ unicodeConv s
unicodeConv (Corners a b c d s) =
  Corners (map unicodeConv a) (map unicodeConv b) (map unicodeConv c) (map unicodeConv d) (unicodeConv s)
unicodeConv (Concat ss) = Concat $ map unicodeConv ss
unicodeConv x = x

-- | Helper for 'unicodeConv' that converts each Unicode character to text equivalent.
-- If a character is Latin, it it just returned.
-- If a character is Unicode and Greek, just the name of the symbol is returned (eg. theta).
-- Otherwise, an error is thrown.
unicodeString :: String -> String
unicodeString = concatMap (\x -> if isLatin1 x then [x] else getName $ nameList x)
  where
    nameList = splitOn " " . map toLower . getCharacterName
    getName ("greek":_:_:name) = unwords name
    getName _ = error "unicodeString not fully implemented"

-----------------------
-- Useful for sorting

-- | Compare the equational 'Symbol' of two things.
compareBySymbol :: HasSymbol a => a -> a -> Ordering
compareBySymbol a b = compsy (eqSymb a) (eqSymb b)

-- | Sorts a list of 'HasSymbols' by 'Symbol'.
sortBySymbol :: HasSymbol a => [a] -> [a]
sortBySymbol = sortBy compareBySymbol

-- | Sorts a tuple list of 'HasSymbols' by first Symbol in the tuple.
sortBySymbolTuple :: HasSymbol a => [(a, b)] -> [(a, b)]
sortBySymbolTuple = sortBy (compareBySymbol `on` fst)

