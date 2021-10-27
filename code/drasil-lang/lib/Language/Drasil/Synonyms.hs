{-# LANGUAGE RankNTypes #-}
-- | Parameterized types, at times, deserve their own names.
module Language.Drasil.Synonyms (
    SimpleQDef, ModelQDef,
    PExpr
) where

import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Expr.Class (ExprC)
import Language.Drasil.Expr.Lang (Expr)
import Language.Drasil.Literal.Class (LiteralC)
import Language.Drasil.ModelExpr.Lang (ModelExpr)

-- | Commonly used type for QDefinitions containing Exprs.
type SimpleQDef = QDefinition Expr
-- | Commonly used type for QDefinitions containing ModelExprs.
type ModelQDef  = QDefinition ModelExpr

-- | Commonly used type for polymorphic Exprs.
type PExpr = forall r . (ExprC r, LiteralC r) => r
