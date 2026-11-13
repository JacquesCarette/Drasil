-- | Smart constructors for the document layout language. Intended for
-- /construction/ exclusively. If you are building renderers, please see
-- 'Drasil.Document.Layout.Development' instead.
module Drasil.Document.Layout (
  -- * Expressions
  parens, addExprFilter,
  -- * Projects
  mkProject
) where

import Drasil.Document.Layout.Expr (parens, addExprFilter)
import Drasil.Document.Layout.Document (mkProject)
