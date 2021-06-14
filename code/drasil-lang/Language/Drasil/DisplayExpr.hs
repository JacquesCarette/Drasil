{-# LANGUAGE GADTs #-}

module Language.Drasil.DisplayExpr (DisplayExpr(..)) where

import qualified Data.List.NonEmpty as NE

import Language.Drasil.Expr (Expr)

data DisplayExpr where
    AlgebraicExpr :: Expr -> DisplayExpr
    Defines       :: DisplayExpr -> DisplayExpr -> DisplayExpr
    MultiExpr     :: NE.NonEmpty DisplayExpr -> DisplayExpr
