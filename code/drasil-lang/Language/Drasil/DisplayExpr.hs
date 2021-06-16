{-# LANGUAGE GADTs #-}

module Language.Drasil.DisplayExpr (DisplayExpr(..),
  DisplayBinOp(..), DisplayAssocBinOp(..)) where

import Language.Drasil.Expr (Expr)
import Language.Drasil.Space (Space)

-- | Binary Display Operations
data DisplayBinOp = Defines | IsIn
  deriving (Eq, Show)

-- | Associative Binary Display Operations
data DisplayAssocBinOp = And | Equivalence
  deriving (Eq, Show)

-- | A variant of the expression language that allows for extensions of Expr
--   that wouldn't normally type check, or that we only care for displaying.
data DisplayExpr where
    AlgebraicExpr :: Expr -> DisplayExpr
    SpaceExpr     :: Space -> DisplayExpr
    BinOp         :: DisplayBinOp -> DisplayExpr -> DisplayExpr -> DisplayExpr
    AssocBinOp    :: DisplayAssocBinOp -> [DisplayExpr] -> DisplayExpr
