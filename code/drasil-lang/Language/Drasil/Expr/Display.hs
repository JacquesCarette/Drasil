{-# LANGUAGE GADTs #-}

module Language.Drasil.Expr.Display (DisplayExpr(..), Display(..), defines, multiExpr) where

import qualified Data.List.NonEmpty as NE

import Language.Drasil.Expr (Expr)

data DisplayExpr where
    -- TODO: We may need to have a field for UIDs instead of AlgebraicExpr
    AlgebraicExpr :: Expr -> DisplayExpr
    Defines       :: DisplayExpr -> DisplayExpr -> DisplayExpr
    MultiExpr     :: NE.NonEmpty DisplayExpr -> DisplayExpr      -- TODO: NonEmpty?


class Display c where
  toDispExpr :: c -> DisplayExpr

instance Display Expr where
  toDispExpr = AlgebraicExpr


defines :: (Display a, Display b) => a -> b -> DisplayExpr
defines a b = Defines (toDispExpr a) (toDispExpr b)

multiExpr :: Display a => [a] -> DisplayExpr
multiExpr = MultiExpr . NE.fromList . map toDispExpr
