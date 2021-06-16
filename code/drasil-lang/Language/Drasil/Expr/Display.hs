{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Drasil.Expr.Display (defines, spaceDE, isIn, andDEs, equalDEs) where

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

assocCreate :: Display d => DisplayAssocBinOp -> [d] -> DisplayExpr
assocCreate b [] = error $ "Need at least 1 expression to create associative " ++ show b
assocCreate _ [x] = toDispExpr x
assocCreate b des = AssocBinOp b $ assocSanitize b $ map toDispExpr des

assocSanitize :: DisplayAssocBinOp -> [DisplayExpr] -> [DisplayExpr]
assocSanitize _ [] = []
assocSanitize b (it@(AssocBinOp c des):r)
    | b == c    = assocSanitize b des ++ assocSanitize b r
    | otherwise = it : assocSanitize b r
assocSanitize b (de:des) = de : assocSanitize b des

andDEs :: Display d => [d] -> DisplayExpr
andDEs = assocCreate And

equalDEs :: Display a => [a] -> DisplayExpr
equalDEs = assocCreate Equal
