module Language.Drasil.Utils(sortBySymbol) where

import Data.List (sortBy)
import Language.Drasil.Classes.Core (HasSymbol(symbol))
import Language.Drasil.Symbol (compsy)
import Language.Drasil.Stages (Stage(Implementation))

-- Sorts a list of HasSymbols by Symbol
sortBySymbol :: (HasSymbol a) => [a] -> [a]
sortBySymbol = sortBy compareBySymbol
  where
    compareBySymbol :: (HasSymbol a) => a -> a -> Ordering
    compareBySymbol a b = compsy (symbol a Implementation) (symbol b Implementation)

