{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Language.Drasil.ModelExpr.Class where

import Prelude hiding (sqrt, log, sin, cos, tan, exp)
import Control.Lens ((^.))

import Drasil.Database (HasUID(..))

import Language.Drasil.Expr.Lang (AssocArithOper(..))
import Language.Drasil.ModelExpr.Lang (ModelExpr(..), DerivType(..),
  SpaceBinOp(..), StatBinOp(..), AssocBoolOper(..))
import Language.Drasil.Space (DomainDesc(..), RTopology(..), Space)
import Language.Drasil.Symbol (Symbol, HasSymbol)

-- | Helper for creating new smart constructors for Associative Binary
-- operations that require at least 1 expression.
assocCreate :: AssocBoolOper -> [ModelExpr] -> ModelExpr
assocCreate abo [] = error $ "Need at least 1 expression to create " ++ show abo
assocCreate _ [x]  = x
assocCreate b des  = AssocB b $ assocSanitize b des

-- | Helper for associative operations, removes embedded variants of the same kind
assocSanitize :: AssocBoolOper -> [ModelExpr] -> [ModelExpr]
assocSanitize _ [] = []
assocSanitize b (it@(AssocB c des):r)
  | b == c    = assocSanitize b des ++ assocSanitize b r
  | otherwise = it : assocSanitize b r
assocSanitize b (de:des) = de : assocSanitize b des

class ModelExprC r where
  -- This also wants a symbol constraint.
  -- | Gets the derivative of an 'ModelExpr' with respect to a 'Symbol'.
  deriv, pderiv :: (HasUID c, HasSymbol c) => r -> c -> r

  -- | Gets the nthderivative of an 'ModelExpr' with respect to a 'Symbol'.
  nthderiv, nthpderiv :: (HasUID c, HasSymbol c) => Integer -> r -> c -> r

  -- | One expression is "defined" by another.
  defines :: r -> r -> r

  -- | Space literals.
  space :: Space -> r

  -- | Check if a value belongs to a Space.
  isIn :: r -> Space -> r

  -- | Binary associative "Equivalence".
  equiv :: [r] -> r

  -- | Smart constructor for the summation, product, and integral functions over all Real numbers.
  intAll, sumAll, prodAll :: Symbol -> r -> r

instance ModelExprC ModelExpr where
  deriv e c  = Deriv 1 Total e (c ^. uid)
  pderiv e c = Deriv 1 Part  e (c ^. uid)
  nthderiv n e c
    | n > 0     = Deriv n Total e (c ^. uid)
    | n == 0    = Deriv 0 Total e (c ^. uid)
    | otherwise = error "non-positive argument to derivative"

  nthpderiv n e c
    | n > 0     = Deriv n Part e (c ^. uid)
    | n == 0    = Deriv 0 Total e (c ^. uid)
    | otherwise = error "non-positive argument to derivative"

  defines = StatBinaryOp Defines

  space = Spc

  isIn a s = SpaceBinaryOp IsIn a (Spc s)

  equiv des
    | length des >= 2 = assocCreate Equivalence des
    | otherwise       = error $ "Need at least 2 expressions to create " ++ show Equivalence

  -- TODO: All of the below only allow for Reals! Will be easier to fix while we add typing.
  -- | Integrate over some expression (∫).
  intAll v = Operator Add (AllDD v Continuous)
  -- | Sum over some expression (∑).
  sumAll v = Operator Add (AllDD v Discrete)
  -- | Product over some expression (∏).
  prodAll v = Operator Mul (AllDD v Discrete)
