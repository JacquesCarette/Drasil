module Language.Drasil.Code.Imperative.Helpers (
  liftS, getUpperBound, lookupC
) where

import Language.Drasil
import Language.Drasil.Development (OrdBinOp(Lt))
import Database.Drasil (symbResolve)
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.CodeSpec (CodeSpec(..))

import Control.Monad.State (State)

-- Puts a state-dependent value into a singleton list
liftS :: State a b -> State a [b]
liftS = fmap (: [])

-- For an expression using a less than operator, gets the upper bound
getUpperBound :: Expr -> Expr
getUpperBound (OrdBinaryOp Lt _ b) = b
getUpperBound _ = error "Attempt to get upper bound of invalid expression"

-- Gets the QuantityDict corresponding to a UID
lookupC :: DrasilState -> UID -> QuantityDict
lookupC g = symbResolve (sysinfodb $ codeSpec g)
