module Language.Drasil.Expr.Display (defines, multiExpr, multiExprNE) where

import qualified Data.List.NonEmpty as NE

import Language.Drasil.DisplayClasses (Display(..))
import Language.Drasil.DisplayExpr (DisplayExpr(MultiExpr, Defines))

defines :: (Display a, Display b) => a -> b -> DisplayExpr
defines a b = Defines (toDispExpr a) (toDispExpr b)

multiExpr :: Display a => [a] -> DisplayExpr
multiExpr = MultiExpr . NE.fromList . map toDispExpr

multiExprNE :: Display a => NE.NonEmpty a -> DisplayExpr
multiExprNE = MultiExpr . NE.map toDispExpr
