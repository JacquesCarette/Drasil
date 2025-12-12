module Language.Drasil.Code.Imperative.Helpers (
  liftS, convScope
) where

import Language.Drasil.Code.Imperative.DrasilState (ScopeType(..))
import Drasil.GOOL (SharedProg, ScopeSym(..))

import Control.Monad.State (State)

-- | Puts a state-dependent value into a singleton list.
liftS :: State a b -> State a [b]
liftS = fmap (: [])

-- | Converts a 'ScopeType' to a 'Scope'
convScope :: (SharedProg r) => ScopeType -> r (Scope r)
convScope Local  = local
convScope Global = global
convScope MainFn = mainFn
