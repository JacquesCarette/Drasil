module Language.Drasil.DisplayClasses where

import Language.Drasil.Expr (Expr)
import Language.Drasil.ModelExpr (ModelExpr)
import Language.Drasil.ModelExpr.Development (expr)

-- TODO: Rename file.
-- TODO: Rename toDispExpr

-- | Data that can convert into a Displayable 'Expr'.
class Display c where
  toDispExpr :: c -> ModelExpr

-- | Basic wrapping in 'AlgebraicExpr'.
instance Display Expr where
  toDispExpr = expr

-- | No change, it's already a 'ModelExpr'.
instance Display ModelExpr where
  toDispExpr = id
