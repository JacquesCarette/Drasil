{-# LANGUAGE GADTs #-}

module Language.Drasil.Expr.Display (DisplayExpr(..), Display(..),
  defines, multiExpr, multiExprNE) where

import qualified Data.List.NonEmpty as NE

import Language.Drasil.Expr (Expr)

data DisplayExpr where
    AlgebraicExpr :: Expr -> DisplayExpr
    Defines       :: DisplayExpr -> DisplayExpr -> DisplayExpr
    MultiExpr     :: NE.NonEmpty DisplayExpr -> DisplayExpr


class Display c where
  toDispExpr :: c -> DisplayExpr

instance Display Expr where
  toDispExpr = AlgebraicExpr


defines :: (Display a, Display b) => a -> b -> DisplayExpr
defines a b = Defines (toDispExpr a) (toDispExpr b)

multiExpr :: Display a => [a] -> DisplayExpr
multiExpr = MultiExpr . NE.fromList . map toDispExpr

multiExprNE :: Display a => NE.NonEmpty a -> DisplayExpr
multiExprNE = MultiExpr . NE.map toDispExpr
