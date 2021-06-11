{-# LANGUAGE GADTs #-}

module Language.Drasil.Expr.Display where

import Language.Drasil.Expr (Expr)

data DisplayExpr where
    -- TODO: We may need to have a field for UIDs instead of AlgebraicExpr
    AlgebraicExpr :: Expr -> DisplayExpr
    Defines       :: DisplayExpr -> DisplayExpr -> DisplayExpr
    MultiExpr     :: Foldable t => t DisplayExpr -> DisplayExpr


-- TODO: The below is a very very bad hack.
instance Eq DisplayExpr where
    (AlgebraicExpr a) == (AlgebraicExpr b) = a == b
    (Defines (AlgebraicExpr a) _) == (Defines (AlgebraicExpr b) _) = a == b
    _ == _ = False
