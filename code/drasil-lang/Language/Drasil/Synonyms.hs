-- | Parameterized types, at times, deserve their own names.
module Language.Drasil.Synonyms (
    SimpleQDef, ModelQDef
) where

import Language.Drasil.Expr.Lang (Expr)
import Language.Drasil.ModelExpr.Lang (ModelExpr)
import Language.Drasil.Chunk.Eq (QDefinition)

type SimpleQDef = QDefinition Expr
type ModelQDef  = QDefinition ModelExpr
