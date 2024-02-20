{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- | Re-export CodeExpr constructors.
module Language.Drasil.CodeExpr (
  CodeExpr,
  CodeExprC(..), ExprC(..), LiteralC(..),
  expr
) where

import Prelude hiding (exp, sin, cos, tan, sqrt, log)

import Language.Drasil.Expr.Class (ExprC(..))
import Language.Drasil.Literal.Class (LiteralC(..))
import Language.Drasil.CodeExpr.Lang (CodeExpr)
import Language.Drasil.CodeExpr.Convert (expr) -- TODO: Remove.

import Language.Drasil.CodeExpr.Class (CodeExprC(..))