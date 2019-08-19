module Language.Drasil.Code.Imperative.Helpers (
  liftS, getUpperBound, lookupC
) where

import Language.Drasil
import Database.Drasil (symbResolve)
import Language.Drasil.Code.Imperative.State (State(..))
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..))

import Control.Monad.Reader (Reader)

liftS :: Reader a b -> Reader a [b]
liftS = fmap (: [])

getUpperBound :: Expr -> Expr
getUpperBound (BinaryOp Lt _ b) = b
getUpperBound _ = error "Attempt to get upper bound of invalid expression"

lookupC :: State -> UID -> QuantityDict
lookupC g = symbResolve (sysinfodb $ csi $ codeSpec g)
