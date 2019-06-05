module Utils.Drasil.Misc (sortBySymbol, sortBySymbolTuple) where

import Language.Drasil

import Data.Function (on)
import Data.List (sortBy)

-- Sorts a list of HasSymbols by Symbol
sortBySymbol :: (HasSymbol a) => [a] -> [a]
sortBySymbol = sortBy compareBySymbol

compareBySymbol :: (HasSymbol a) => a -> a -> Ordering
compareBySymbol a b = compsy (symbol a Implementation) (symbol b Implementation)

sortBySymbolTuple :: (HasSymbol a) => [(a, b)] -> [(a, b)]
sortBySymbolTuple = sortBy (compareBySymbol `on` fst)
