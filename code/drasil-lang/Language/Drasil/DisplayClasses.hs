module Language.Drasil.DisplayClasses where

import Language.Drasil.DisplayExpr (DisplayExpr(AlgebraicExpr))
import Language.Drasil.Expr (Expr)

-- | Data that can convert into a Displayable Expr
class Display c where
  toDispExpr :: c -> DisplayExpr

instance Display Expr where
  -- | Basic wrapping in AlgebraicExpr
  toDispExpr = AlgebraicExpr

instance Display DisplayExpr where
  -- | No change, it's already a DisplayExpr
  toDispExpr = id
