module Language.Drasil.Utils (sortBySymbol, sortBySymbolTuple) where

import Data.Function (on)
import Data.List (sortBy)

import Language.Drasil.Classes.Core (HasSymbol(symbol))
import Language.Drasil.Symbol (compsy)
import Language.Drasil.Stages (Stage(Implementation))

-- Sorts a list of HasSymbols by Symbol
sortBySymbol :: (HasSymbol a) => [a] -> [a]
sortBySymbol = sortBy compareBySymbol

compareBySymbol :: (HasSymbol a) => a -> a -> Ordering
compareBySymbol a b = compsy (symbol a Implementation) (symbol b Implementation)

sortBySymbolTuple :: (HasSymbol a) => [(a, b)] -> [(a, b)]
sortBySymbolTuple = sortBy (compareBySymbol `on` fst)
