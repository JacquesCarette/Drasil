module Language.Drasil.Code.Imperative.Helpers (
  liftS, lookupC, convScope
) where

import Language.Drasil (UID, DefinedQuantityDict)
import Database.Drasil (symbResolve)
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..),
  ScopeType(..))
import Language.Drasil.CodeSpec (HasOldCodeSpec(..))
import Drasil.GOOL (SharedProg, ScopeSym(..))

import Control.Monad.State (State)
import Control.Lens ((^.))

-- | Puts a state-dependent value into a singleton list.
liftS :: State a b -> State a [b]
liftS = fmap (: [])

-- | Gets the 'QuantityDict' corresponding to a 'UID'.
lookupC :: DrasilState -> UID -> DefinedQuantityDict
lookupC g = symbResolve (codeSpec g ^. systemdbO)

-- | Converts a 'ScopeType' to a 'Scope'
convScope :: (SharedProg r) => ScopeType -> r (Scope r)
convScope Local  = local
convScope Global = global
convScope MainFn = mainFn