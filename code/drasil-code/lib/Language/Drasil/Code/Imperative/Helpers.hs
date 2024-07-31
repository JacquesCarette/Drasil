module Language.Drasil.Code.Imperative.Helpers (
  liftS, lookupC, convScope
) where

import Language.Drasil (UID, QuantityDict)
import Database.Drasil (symbResolve)
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..),
  ScopeType(..))
import Language.Drasil.CodeSpec (CodeSpec(..))
import Drasil.GOOL (SharedProg, ScopeSym(..))

import Control.Monad.State (State)

-- | Puts a state-dependent value into a singleton list.
liftS :: State a b -> State a [b]
liftS = fmap (: [])

-- | Gets the 'QuantityDict' corresponding to a 'UID'.
lookupC :: DrasilState -> UID -> QuantityDict
lookupC g = symbResolve (sysinfodb $ codeSpec g)

-- | Converts a 'ScopeType' to a 'Scope'
convScope :: (SharedProg r) => ScopeType -> r (Scope r)
convScope Local  = local
convScope Global = global
convScope MainFn = mainFn