{-# LANGUAGE GADTs #-}

module Language.Drasil.DisplayExpr (DisplayExpr(..),
  DisplayBinOp(..), DisplayAssocBinOp(..)) where

import Language.Drasil.Expr (Expr)
import Language.Drasil.Space (Space)

data DisplayBinOp = Defines | IsIn
  deriving (Eq, Show)

-- | Associative Binary Display Operations
--
--   Note: 'Equal' makes sense in the Display language, but not in the
--         general computation language.
data DisplayAssocBinOp = And | Equal
  deriving (Eq, Show)

data DisplayExpr where
    AlgebraicExpr :: Expr -> DisplayExpr
    SpaceExpr     :: Space -> DisplayExpr
    BinOp         :: DisplayBinOp -> DisplayExpr -> DisplayExpr -> DisplayExpr
    AssocBinOp    :: DisplayAssocBinOp -> [DisplayExpr] -> DisplayExpr
