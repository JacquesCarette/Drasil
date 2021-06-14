module Language.Drasil.Expr.Display (defines, spaceDE, isIn, andDEs) where

import Language.Drasil.DisplayClasses (Display(..))
import Language.Drasil.DisplayExpr (DisplayAssocBinOp(..), 
    DisplayBinOp(..), DisplayExpr(..))
import Language.Drasil.Space (Space)

defines :: (Display a, Display b) => a -> b -> DisplayExpr
defines a b = BinOp Defines (toDispExpr a) (toDispExpr b)

spaceDE :: Space -> DisplayExpr
spaceDE = SpaceExpr

isIn :: Display a => a -> DisplayExpr -> DisplayExpr
isIn a s@(SpaceExpr _) = BinOp IsIn (toDispExpr a) s
isIn _ _               = error "isIn target must be a Space"

andDEs :: Display a => [a] -> DisplayExpr
andDEs [] = error "Need at least 1 expression to use andDEs"
andDEs [x] = toDispExpr x
andDEs des = AssocBinOp And $ map toDispExpr des
