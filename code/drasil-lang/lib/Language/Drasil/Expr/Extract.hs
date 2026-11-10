-- | Extract UIDs from an expression so that they can be looked up in the chunk database and rendered.
module Language.Drasil.Expr.Extract (
  eDep, eNames, eNames', eNamesRI
) where

import Data.Containers.ListUtils (nubOrd)

import Drasil.Database (UID)

import Language.Drasil.Expr.Lang (Expr, eNames, eNames', eNamesRI)

-- | Get dependencies from an equation.
eDep :: Expr -> [UID]
eDep = nubOrd . eNames
