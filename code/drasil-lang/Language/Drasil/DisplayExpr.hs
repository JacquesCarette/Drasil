{-# LANGUAGE GADTs #-}
-- | Display related expression language types.
module Language.Drasil.DisplayExpr (DisplayExpr(..),
  DisplayBinOp(..), DisplayAssocBinOp(..)) where

import Language.Drasil.Expr (Expr)
import Language.Drasil.Space (Space)

-- | Binary Display Operations.
data DisplayBinOp = Defines | IsIn
  deriving (Eq, Show)

-- TODO: 'DAnd' shouldn't exist because it's redundant. When we convert the DisplayExpr into
--       another encoding, we will likely get rid of it.

-- | Associative Binary Display Operations.
data DisplayAssocBinOp = DAnd | Equivalence
  deriving (Eq, Show)

-- | A variant of the expression language that allows for extensions of Expr
--   that wouldn't normally type check, or that we only care for displaying.
data DisplayExpr where
    AlgebraicExpr :: Expr -> DisplayExpr
    SpaceExpr     :: Space -> DisplayExpr
    BinOp         :: DisplayBinOp -> DisplayExpr -> DisplayExpr -> DisplayExpr
    AssocBinOp    :: DisplayAssocBinOp -> [DisplayExpr] -> DisplayExpr
