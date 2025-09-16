{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- | Re-export CodeExpr constructors.
module Drasil.Code.CodeExpr (
  CodeExpr,
  CodeExprC(..), ExprC(..), LiteralC(..),
  expr
) where

import Prelude hiding (exp, sin, cos, tan, sqrt, log)

import Language.Drasil.Expr.Class (ExprC(..))
import Language.Drasil.Literal.Class (LiteralC(..))

import Drasil.Code.CodeExpr.Class (CodeExprC(..))
import Drasil.Code.CodeExpr.Convert (expr) -- TODO: Remove.
import Drasil.Code.CodeExpr.Lang (CodeExpr)
