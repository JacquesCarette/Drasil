{-# LANGUAGE GADTs #-}

module Language.Drasil.DisplayExpr (DisplayExpr(..),
  DisplayBinOp(..), DisplayAssocBinOp(..)) where

import Language.Drasil.Expr (Expr)
import Language.Drasil.Space (Space)

data DisplayBinOp = Defines | IsIn
data DisplayAssocBinOp = And

data DisplayExpr where
    AlgebraicExpr :: Expr -> DisplayExpr
    SpaceExpr     :: Space -> DisplayExpr
    BinOp         :: DisplayBinOp -> DisplayExpr -> DisplayExpr -> DisplayExpr
    AssocBinOp    :: DisplayAssocBinOp -> [DisplayExpr] -> DisplayExpr
