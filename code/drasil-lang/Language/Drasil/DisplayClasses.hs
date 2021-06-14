module Language.Drasil.DisplayClasses where

import Language.Drasil.DisplayExpr (DisplayExpr(AlgebraicExpr))
import Language.Drasil.Expr (Expr)

class Display c where
  toDispExpr :: c -> DisplayExpr

instance Display Expr where
  toDispExpr = AlgebraicExpr

instance Display DisplayExpr where
  toDispExpr = id
