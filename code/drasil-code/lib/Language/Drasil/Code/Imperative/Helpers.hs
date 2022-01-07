module Language.Drasil.Code.Imperative.Helpers (
  liftS, lookupC
) where

import Language.Drasil (UID, QuantityDict)
import Database.Drasil (symbResolve)
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.CodeSpec (CodeSpec(..))

import Control.Monad.State (State)

-- | Puts a state-dependent value into a singleton list.
liftS :: State a b -> State a [b]
liftS = fmap (: [])

-- | Gets the 'QuantityDict' corresponding to a 'UID'.
lookupC :: DrasilState -> UID -> QuantityDict
lookupC g = symbResolve (sysinfodb $ codeSpec g)
