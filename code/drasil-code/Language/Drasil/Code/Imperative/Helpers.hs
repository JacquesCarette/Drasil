module Language.Drasil.Code.Imperative.Helpers (
  liftS, getUpperBound, lookupC
) where

import Language.Drasil
import Database.Drasil (symbResolve)
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.CodeSpec (CodeSpec(..))

import Control.Monad.State (State)

liftS :: State a b -> State a [b]
liftS = fmap (: [])

getUpperBound :: Expr -> Expr
getUpperBound (BinaryOp Lt _ b) = b
getUpperBound _ = error "Attempt to get upper bound of invalid expression"

lookupC :: DrasilState -> UID -> QuantityDict
lookupC g = symbResolve (sysinfodb $ codeSpec g)
