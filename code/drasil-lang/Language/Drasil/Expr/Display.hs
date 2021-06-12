{-# LANGUAGE GADTs #-}

module Language.Drasil.Expr.Display where

import Language.Drasil.Expr (Expr)

data DisplayExpr where
    -- TODO: We may need to have a field for UIDs instead of AlgebraicExpr
    AlgebraicExpr :: Expr -> DisplayExpr
    Defines       :: DisplayExpr -> DisplayExpr -> DisplayExpr
    MultiExpr     :: [DisplayExpr] -> DisplayExpr      -- TODO: NonEmpty?


class Display c where
  toDispExpr :: c -> DisplayExpr

instance Display Expr where
  toDispExpr = AlgebraicExpr


defines :: (Display a, Display b) => a -> b -> DisplayExpr
defines a b = Defines (toDispExpr a) (toDispExpr b)

multiExpr :: (Display a) => [a] -> DisplayExpr
multiExpr = MultiExpr . map toDispExpr
