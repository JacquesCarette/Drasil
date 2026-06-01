module Drasil.Code.Classes (
  -- * Classes
  IsArgumentName,
  Callable
) where

import Language.Drasil.Symbol (HasSymbol)

-- | Some chunks can be called like functions.
class HasSymbol c => Callable c

-- | Members must have a named argument.
class HasSymbol c => IsArgumentName c where
