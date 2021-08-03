{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Drasil.ModelExpr.Display (defines, space, isIn, andDEs, equivDEs) where

import Language.Drasil.ExprClasses (Express(..))
import Language.Drasil.ModelExpr
import Language.Drasil.Space (Space)

-- | One expression is "defined" by another.
defines :: (Express a, Express b) => a -> b -> ModelExpr
defines a b = StatBinaryOp Defines (express a) (express b)

-- | Bring a Space into the ModelExpr.
space :: Space -> ModelExpr
space = Spc

isIn :: Express a => a -> ModelExpr -> ModelExpr
isIn a s@(Spc _) = SpaceBinaryOp IsIn (express a) s
isIn _ _         = error "isIn target must be a Space"

-- | Helper for creating new smart constructors for Associative Binary
--   operations that require at least 1 expression.
assocCreate :: Express d => AssocBoolOper -> [d] -> ModelExpr
assocCreate abo [] = error $ "Need at least 1 expression to create " ++ show abo
assocCreate _ [x]  = express x
assocCreate b des  = AssocB b $ assocSanitize b $ map express des

-- | Helper for associative operations, removes embedded variants of the same kind
assocSanitize :: AssocBoolOper -> [ModelExpr] -> [ModelExpr]
assocSanitize _ [] = []
assocSanitize b (it@(AssocB c des):r)
  | b == c    = assocSanitize b des ++ assocSanitize b r
  | otherwise = it : assocSanitize b r
assocSanitize b (de:des) = de : assocSanitize b des

-- | Binary associative "And".
andDEs :: Express d => [d] -> ModelExpr
andDEs = assocCreate And

-- | Binary associative "Equivalence".
equivDEs :: Express a => [a] -> ModelExpr
equivDEs des
  | length des >= 2 = assocCreate Equivalence des
  | otherwise       = error $ "Need at least 2 expressions to create " ++ show Equivalence
