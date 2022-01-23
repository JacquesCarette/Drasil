-- | Defines classes for use with Drasil's expression language.
module Language.Drasil.ExprClasses where

import Language.Drasil.Expr.Lang (Expr)
import Language.Drasil.ModelExpr.Lang (ModelExpr(Lit))
import Language.Drasil.ModelExpr.Convert (expr)
import Language.Drasil.Literal.Lang (Literal)

-- | Data that can be expressed using 'ModelExpr'.
class Express c where
  express :: c -> ModelExpr

instance Express Literal where
  express = Lit

-- | Rewriting 'Expr's using the 'ModelExpr' language.
instance Express Expr where
  express = expr

-- | No change, it's already a 'ModelExpr'.
instance Express ModelExpr where
  express = id
