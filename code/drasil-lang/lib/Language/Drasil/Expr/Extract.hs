-- | Extract UIDs (so they can be looked up in the chunk database and rendered) and matrix rows from an expression.
module Language.Drasil.Expr.Extract (
  eDep, eNames, eNames', eNamesRI, matrixRows
) where

import Data.Containers.ListUtils (nubOrd)

import Drasil.Database (UID)

import Language.Drasil.Expr.Lang (Expr, ExprF(Matrix), eNames, eNames', eNamesRI)

-- | Get dependencies from an equation.
eDep :: Expr -> [UID]
eDep = nubOrd . eNames

-- | The rows of a 'Matrix' expression, if it is one.
matrixRows :: Expr -> Maybe [[Expr]]
matrixRows (Matrix rows) = Just rows
matrixRows _             = Nothing
