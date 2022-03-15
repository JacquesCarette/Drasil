{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- | Re-export CodeExpr constructors.
module Language.Drasil.CodeExpr (
  CodeExpr,
  CodeExprC(..), ExprC(..), LiteralC(..),
  expr
) where

import Prelude hiding (exp, sin, cos, tan, sqrt, log)

import Language.Drasil (ExprC(..), LiteralC(..))
import Language.Drasil.Code.Expr (CodeExpr)
import Language.Drasil.Code.Expr.Convert (expr) -- TODO: Remove.

import Language.Drasil.Code.Expr.Class (CodeExprC(..))
