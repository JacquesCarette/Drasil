{-# LANGUAGE GADTs #-}

module Language.Drasil.Expr.Display where

import Language.Drasil.Expr (Expr)

data DisplayExpr where
    -- TODO: We may need to have a field for UIDs instead of AlgebraicExpr
    AlgebraicExpr :: Expr -> DisplayExpr
    Defines       :: DisplayExpr -> DisplayExpr -> DisplayExpr
    MultiExpr     :: Foldable t => t DisplayExpr -> DisplayExpr

