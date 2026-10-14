-- | Defines classes for use with Drasil's expression language.
module Language.Drasil.ExprClasses (Express(..)) where

import qualified Data.List.NonEmpty as NE

import Language.Drasil.Expr.Lang (Expr)
import Language.Drasil.ModelExpr.Lang (ModelExpr(Lit))
import Language.Drasil.ModelExpr.Convert (expr)
import Language.Drasil.Literal.Lang (Literal)
import Language.Drasil.Expr.Class (($&&))

-- | Express something axiomatically.
class Express c where
  -- | Express something as a single fact.
  express :: c -> ModelExpr
  -- | Express something as a series of facts.
  mexpress :: c -> NE.NonEmpty ModelExpr

  express = foldr1 ($&&) . mexpress
  mexpress = NE.singleton . express
  {-# MINIMAL express | mexpress #-}

instance Express Literal where
  express = Lit

-- | Rewriting 'Expr's using the 'ModelExpr' language.
instance Express Expr where
  express = expr

-- | No change, it's already a 'ModelExpr'.
instance Express ModelExpr where
  express = id

instance Express t => Express [t] where
  mexpress = (express <$>) . NE.fromList

instance Express t => Express (NE.NonEmpty t) where
  mexpress = (express <$>)
