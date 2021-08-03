{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Drasil.Expr.Display (defines, spaceDE, isIn, andDEs, equivDEs) where

import Language.Drasil.DisplayClasses (Display(..))
import Language.Drasil.DisplayExpr (DisplayAssocBinOp(..),
    DisplayBinOp(..), DisplayExpr(..))
import Language.Drasil.Space (Space)

-- | One expression is "defined" by another.
defines :: (Display a, Display b) => a -> b -> DisplayExpr
defines a b = BinOp Defines (toDispExpr a) (toDispExpr b)

-- | Bring a Space into the DisplayExpr.
spaceDE :: Space -> DisplayExpr
spaceDE = SpaceExpr

isIn :: Display a => a -> DisplayExpr -> DisplayExpr
isIn a s@(SpaceExpr _) = BinOp IsIn (toDispExpr a) s
isIn _ _               = error "isIn target must be a Space"

-- | Helper for creating new smart constructors for Associative Binary
--   operations that require at least 1 expression.
assocCreate :: Display d => DisplayAssocBinOp -> [d] -> DisplayExpr
assocCreate b [] = error $ "Need at least 1 expression to create associative " ++ show b
assocCreate _ [x] = toDispExpr x
assocCreate b des = AssocBinOp b $ assocSanitize b $ map toDispExpr des

-- | Helper for associative operations, removes embedded variants of the same kind
assocSanitize :: DisplayAssocBinOp -> [DisplayExpr] -> [DisplayExpr]
assocSanitize _ [] = []
assocSanitize b (it@(AssocBinOp c des):r)
  | b == c    = assocSanitize b des ++ assocSanitize b r
  | otherwise = it : assocSanitize b r
assocSanitize b (de:des) = de : assocSanitize b des

-- | Binary associative "And".
andDEs :: Display d => [d] -> DisplayExpr
andDEs = assocCreate DAnd

-- | Binary associative "Equivalence".
equivDEs :: Display a => [a] -> DisplayExpr
equivDEs des
  | length des >= 2 = assocCreate Equivalence des
  | otherwise       = error $ "Need at least 2 expressions to create " ++ show Equivalence
