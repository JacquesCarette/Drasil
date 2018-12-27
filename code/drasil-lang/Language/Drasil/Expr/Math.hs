{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Language.Drasil.Expr.Math where

import Prelude hiding (sqrt)
import Control.Lens ((^.))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Expr (Expr(..), Relation, DerivType(..), ($^), BinOp(..), 
  ArithOper(..), UFunc(..))
import Language.Drasil.Space (Space, RTopology(..), DomainDesc(..), RealInterval)
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol)

-- | Smart constructor to take the log of an expression
log :: Expr -> Expr
log = UnaryOp Log

-- | Smart constructor to take the ln of an expression
ln :: Expr -> Expr
ln = UnaryOp Ln

-- | Smart constructor to take the square root of an expression
sqrt :: Expr -> Expr
sqrt = UnaryOp Sqrt

-- | Smart constructor to apply sin to an expression
sin :: Expr -> Expr
sin = UnaryOp Sin

-- | Smart constructor to apply cos to an expression
cos :: Expr -> Expr 
cos = UnaryOp Cos

-- | Smart constructor to apply tan to an expression
tan :: Expr -> Expr
tan = UnaryOp Tan

-- | Smart constructor to apply sec to an expression
sec :: Expr -> Expr 
sec = UnaryOp Sec

-- | Smart constructor to apply csc to an expression
csc :: Expr -> Expr
csc = UnaryOp Csc

-- | Smart constructor to apply cot to an expression
cot :: Expr -> Expr 
cot = UnaryOp Cot

-- | Smart constructor for the exponential (base e) function
exp :: Expr -> Expr
exp = UnaryOp Exp

-- | Smart constructor for the dimension of a vector
dim :: Expr -> Expr
dim = UnaryOp Dim

-- | Smart constructor for indexing
idx :: Expr -> Expr -> Expr
idx = BinaryOp Index

-- | Smart constructors for integers, doubles, strings
int :: Integer -> Expr
int = Int

dbl :: Double -> Expr
dbl = Dbl

str :: String -> Expr
str = Str

-- | Smart constructor for set membership
-- FIXME: first argument really ought to be restricted to a
-- 'variable', as IsIn should only be used as a type proxy
isin :: Expr -> Space -> Expr
isin = IsIn

-- | Smart constructor for the summation, product, and integrals
defint, defsum, defprod :: Symbol -> Expr -> Expr -> Expr -> Expr
int_all, sum_all, prod_all :: Symbol -> Expr -> Expr

defint v low high e = Operator Add (BoundedDD v Continuous low high) e
int_all v e = Operator Add (AllDD v Continuous) e

defsum v low high e = Operator Add (BoundedDD v Discrete low high) e
sum_all v e = Operator Add (AllDD v Discrete) e

defprod v low high e = Operator Mul (BoundedDD v Discrete low high) e
prod_all v e = Operator Mul (AllDD v Discrete) e

-- | Smart constructor for 'real interval' membership
real_interval :: HasUID c => c -> RealInterval Expr Expr -> Expr
real_interval c = RealI (c ^. uid)

-- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares
euclidean :: [Expr] -> Expr
euclidean = sqrt . sum' . map square

-- | Used by 'euclidean' function (in place of 'sum') to fix representation of computation
sum' :: (Num a, Foldable t) => t a -> a
sum' x = foldr1 (+) x
  
-- | Smart constructor to cross product two expressions
cross :: Expr -> Expr -> Expr
cross = BinaryOp Cross

-- | Smart constructor for case statement (underscore as case is reserved)
case_ :: [(Expr,Relation)] -> Expr
case_ = Case

square :: Expr -> Expr
square x = x $^ 2

-- some matrix helper functions
m2x2 :: Expr -> Expr -> Expr -> Expr -> Expr
m2x2 a b c d = Matrix [[a,b],[c,d]]

vec2D :: Expr -> Expr -> Expr
vec2D a b    = Matrix [[a],[b]]

dgnl2x2 :: Expr -> Expr -> Expr
dgnl2x2 a d  = m2x2 a (Int 0) (Int 0) d

-- Some helper functions to do function application
apply :: Expr -> [Expr] -> Expr
apply = FCall

apply1 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a) => f -> a -> Expr
apply1 f a = FCall (sy f) [sy a]

apply2 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a, HasUID b, HasSymbol b) => 
    f -> a -> b -> Expr
apply2 f a b = FCall (sy f) [sy a, sy b]

-- Note how |sy| 'enforces' having a symbol
sy :: (HasUID c, HasSymbol c) => c -> Expr
sy x = C (x ^. uid)

-- This also wants a symbol constraint.
deriv, pderiv :: (HasUID c, HasSymbol c) => Expr -> c -> Expr
deriv e c = Deriv Total e (c^.uid)
pderiv e c = Deriv Part e (c^.uid)
