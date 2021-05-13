{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Language.Drasil.Expr.Math where

import Prelude hiding (sqrt)
import Control.Lens ((^.))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Expr (Expr(..), Relation, DerivType(..), ($^), AssocArithOper(..),
  LABinOp(..), VVVBinOp(..), UFunc(..), UFuncB(..), UFuncVec(..), Completeness(..))
import Language.Drasil.Space (Space, RTopology(..), DomainDesc(..), RealInterval)
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol)
import Language.Drasil.Classes (IsArgumentName)

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

-- | Smart constructor to apply arcsin to an expression
arcsin :: Expr -> Expr 
arcsin = UnaryOp Arcsin

-- | Smart constructor to apply arccos to an expression
arccos :: Expr -> Expr 
arccos = UnaryOp Arccos

-- | Smart constructor to apply arctan to an expression
arctan :: Expr -> Expr 
arctan = UnaryOp Arctan

-- | Smart constructor for the exponential (base e) function
exp :: Expr -> Expr
exp = UnaryOp Exp

-- | Smart constructor for calculating the dimension of a vector
dim :: Expr -> Expr
dim = UnaryOpVec Dim

-- | Smart constructor for calculating the normal form of a vector
norm :: Expr -> Expr
norm = UnaryOpVec Norm

-- | Smart constructor for applying logical negation to an expression
not_ :: Expr -> Expr
not_ = UnaryOpB Not

-- | Smart constructor for indexing
idx :: Expr -> Expr -> Expr
idx = LABinaryOp Index

-- | Smart constructors for integers, doubles, strings, percents
int :: Integer -> Expr
int = Int

dbl :: Double -> Expr
dbl = Dbl

str :: String -> Expr
str = Str

perc :: Integer -> Integer -> Expr
perc = Perc

-- | Smart constructor for set membership
-- FIXME: first argument really ought to be restricted to a
-- 'variable', as IsIn should only be used as a type proxy
isin :: Expr -> Space -> Expr
isin = IsIn

-- | Smart constructor for the summation, product, and integrals
defint, defsum, defprod :: Symbol -> Expr -> Expr -> Expr -> Expr
intAll, sumAll, prodAll :: Symbol -> Expr -> Expr

defint v low high = Operator Add (BoundedDD v Continuous low high)
intAll v = Operator Add (AllDD v Continuous)

defsum v low high = Operator Add (BoundedDD v Discrete low high)
sumAll v = Operator Add (AllDD v Discrete)

defprod v low high = Operator Mul (BoundedDD v Discrete low high)
prodAll v = Operator Mul (AllDD v Discrete)

-- | Smart constructor for 'real interval' membership
realInterval :: HasUID c => c -> RealInterval Expr Expr -> Expr
realInterval c = RealI (c ^. uid)

-- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares
euclidean :: [Expr] -> Expr
euclidean = sqrt . sum' . map square

{-# ANN sum' "HLint: ignore Use sum" #-}
-- | Used by 'euclidean' function (in place of 'sum') to fix representation of computation
sum' :: (Num a, Foldable t) => t a -> a
sum' = foldr1 (+)
  
-- | Smart constructor to cross product two expressions
cross :: Expr -> Expr -> Expr
cross = VVVBinaryOp Cross

-- | Smart constructor for case statement with complete set of cases
completeCase :: [(Expr,Relation)] -> Expr
completeCase = Case Complete

-- | Smart constructor for case statement with incomplete set of cases
incompleteCase :: [(Expr,Relation)] -> Expr
incompleteCase = Case Incomplete

square :: Expr -> Expr
square x = x $^ 2

-- some matrix helper functions
m2x2 :: Expr -> Expr -> Expr -> Expr -> Expr
m2x2 a b c d = Matrix [[a,b],[c,d]]

vec2D :: Expr -> Expr -> Expr
vec2D a b    = Matrix [[a],[b]]

dgnl2x2 :: Expr -> Expr -> Expr
dgnl2x2 a  = m2x2 a (Int 0) (Int 0)

-- Some helper functions to do function application

-- FIXME: These constructors should check that the UID is associated with a
-- chunk that is actually callable.
apply :: (HasUID f, HasSymbol f) => f -> [Expr] -> Expr
apply f ps = FCall (f ^. uid) ps []

apply1 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a) => f -> a -> Expr
apply1 f a = FCall (f ^. uid) [sy a] []

apply2 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a, HasUID b, HasSymbol b) 
  => f -> a -> b -> Expr
apply2 f a b = FCall (f ^. uid) [sy a, sy b] []

applyWithNamedArgs :: (HasUID f, HasSymbol f, HasUID a, IsArgumentName a) => f 
  -> [Expr] -> [(a, Expr)] -> Expr
applyWithNamedArgs f ps ns = FCall (f ^. uid) ps (zip (map ((^. uid) . fst) ns) 
  (map snd ns))

-- Note how |sy| 'enforces' having a symbol
sy :: (HasUID c, HasSymbol c) => c -> Expr
sy x = C (x ^. uid)

-- This also wants a symbol constraint.
deriv, pderiv :: (HasUID c, HasSymbol c) => Expr -> c -> Expr
deriv e c = Deriv Total e (c^.uid)
pderiv e c = Deriv Part e (c^.uid)
