{-# LANGUAGE GADTs #-}
-- | The Drasil Expression language
module Language.Drasil.Expr where

import Data.Ratio (numerator,denominator)
import Prelude hiding (sqrt)
import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Symbol
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Space (Space(..))

import Control.Lens ((^.))

--FIXME: Haddock open issue #43 seems to make it so GADT constructors cannot
-- be documented properly

type UID = String
type Relation = Expr

infixr 8 $^
infixl 7 $/
infixr 4 $=
infixr 9 $&&
infixr 9 $||

--Known math functions.
-- TODO: Move the below to a separate file somehow. How to go about it?

data BinOp = Frac | Pow | Subt | Eq | NEq | Lt | Gt | LEq | GEq | Impl | Iff | Index
  | Dot | Cross
  deriving Eq

data ArithOper = Add | Mul deriving (Eq)

data BoolOper = And | Or deriving (Eq)

-- | Unary functions
data UFunc = Norm | Abs | Log | Sin | Cos | Tan | Sec | Csc | Cot | Exp
  | Sqrt | Not | Neg | Dim

-- | Drasil Expressions
data Expr where
  Dbl      :: Double -> Expr
  Int      :: Integer -> Expr
  Str      :: String -> Expr
  AssocA   :: ArithOper -> [Expr] -> Expr
  AssocB   :: BoolOper  -> [Expr] -> Expr
  Deriv    :: DerivType -> Expr -> UID -> Expr 
  -- Derivative, syntax is:
  -- Type (Partial or total) -> principal part of change -> with respect to
  -- For example: Deriv Part y x1 would be (dy/dx1)
  C        :: UID -> Expr -- Chunk (must have a symbol)
  FCall    :: Expr -> [Expr] -> Expr -- F(x) is (FCall F [x]) or similar
                                  -- FCall accepts a list of params
                                  -- F(x,y) would be (FCall F [x,y]) or sim.
  Case     :: [(Expr,Relation)] -> Expr -- For multi-case expressions,
                                     -- each pair represents one case
  Matrix   :: [[Expr]] -> Expr
  UnaryOp  :: UFunc -> Expr -> Expr
  BinaryOp :: BinOp -> Expr -> Expr -> Expr
  -- Operators are generalized arithmetic operators over a |DomainDesc|
  --   of an |Expr|.  Could be called |BigOp|.
  -- ex: Summation is represented via |Add| over a discrete domain
  Operator :: ArithOper -> DomainDesc Expr Expr -> Expr -> Expr

  IsIn     :: Expr -> Space -> Expr --	element of
  RealI    :: UID -> RealInterval Expr Expr -> Expr -- a different kind of 'element of'

($=), ($!=), ($<), ($>), ($<=), ($>=), ($=>), ($<=>), ($.), ($-), 
  ($/) :: Expr -> Expr -> Expr
($=)  a b = BinaryOp Eq a b
($!=) a b = BinaryOp NEq a b
($<)  a b = BinaryOp Lt a b
($>)  a b = BinaryOp Gt a b
($<=) a b = BinaryOp LEq a b
($>=) a b = BinaryOp GEq a b
a $=> b = BinaryOp Impl a b
a $<=> b = BinaryOp Iff a b
a $. b   = BinaryOp Dot a b
a $- b = BinaryOp Subt a b
a $/ b = BinaryOp Frac a b

($^), ($&&), ($||) :: Expr -> Expr -> Expr
($^) = BinaryOp Pow
a $&& b = AssocB And [a,b]
a $|| b = AssocB Or  [a,b]

sy :: (Chunk c, HasSymbol c) => c -> Expr
sy x = C (x ^. uid)

deriv, pderiv :: (Chunk c, HasSymbol c) => Expr -> c -> Expr
deriv e c = Deriv Total e (c^.uid)
pderiv e c = Deriv Part e (c^.uid)

type Variable = String

data DerivType = Part | Total deriving Eq

-- TODO: have $+ flatten nest Adds
instance Num Expr where
  a + b = AssocA Add [a, b]
  a * b = AssocA Mul [a, b]
  a - b = BinaryOp Subt a b
  fromInteger a = Int a
  abs = UnaryOp Abs
  negate = UnaryOp Neg

  -- this is a Num wart
  signum _ = error "should not use signum in expressions"

instance Eq Expr where
  Dbl a == Dbl b               =  a == b
  Int a == Int b               =  a == b
  Str a == Str b               =  a == b
  AssocA o1 l1 == AssocA o2 l2   =  o1 == o2 && l1 == l2
  AssocB o1 l1 == AssocB o2 l2   =  o1 == o2 && l1 == l2
  Deriv t1 a b == Deriv t2 c d =  t1 == t2 && a == c && b == d
  C a == C b                   =  a == b
  FCall a b == FCall c d       =  a == c && b == d
  Case a == Case b             =  a == b
  IsIn  a b  == IsIn  c d      =  a == c && b == d
  BinaryOp o a b == BinaryOp p c d =  o == p && a == c && b == d
  _ == _                       =  False

instance Fractional Expr where
  a / b = BinaryOp Frac a b
  fromRational r = BinaryOp Frac (fromInteger $ numerator   r)
                                (fromInteger $ denominator r)

-- | Topology of a subset of reals.
data RTopology = Continuous | Discrete

data DomainDesc a b where
  BoundedDD :: Symbol -> RTopology -> a -> b -> DomainDesc a b
  AllDD :: Symbol -> RTopology -> DomainDesc a b

data Inclusive = Inc | Exc

-- | RealInterval. A |RealInterval| is a subset of |Real| (as a |Space|).
-- These come in different flavours.
-- For now, embed |Expr| for the bounds, but that will change as well.
data RealInterval a b where
  Bounded :: (Inclusive, a) -> (Inclusive, b) -> RealInterval a b -- (x .. y)
  UpTo :: (Inclusive, a) -> RealInterval a b -- (-infinity .. x)
  UpFrom :: (Inclusive, b) -> RealInterval a b -- (x .. infinity)
