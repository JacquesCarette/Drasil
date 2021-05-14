{-# LANGUAGE GADTs #-}
-- | The Drasil Expression language
module Language.Drasil.Expr where

import Data.Ratio (denominator, numerator)
import Prelude hiding (sqrt)

import Language.Drasil.Space (Space(..), DomainDesc, RealInterval)
import Language.Drasil.UID (UID)

-- FIXME: Haddock open issue #43 seems to make it so GADT constructors cannot
-- be documented properly

type Relation = Expr

infixr 8 $^
infixl 7 $/
infixr 4 $=
infixr 9 $&&
infixr 9 $||

-- Known math functions.
-- TODO: Move the below to a separate file somehow. How to go about it?

-- Binary functions

data ArithBinOp = Frac | Pow | Subt
  deriving Eq

data EqBinOp = Eq | NEq
  deriving Eq

data BoolBinOp = Impl | Iff
  deriving Eq

data LABinOp = Index
  deriving Eq

data OrdBinOp = Lt | Gt | LEq | GEq
  deriving Eq

-- Vector x Vector -> Vector binary operations
data VVVBinOp = Cross
  deriving Eq

-- Vector x Vector -> Number binary operations
data VVNBinOp = Dot
  deriving Eq

data AssocArithOper = Add | Mul
  deriving Eq

data AssocBoolOper = And | Or
  deriving Eq

-- | Unary functions
data UFunc = Abs | Log | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin
  | Arccos | Arctan | Exp | Sqrt | Neg
  deriving Eq

data UFuncB = Not
  deriving Eq

data UFuncVec = Norm | Dim
  deriving Eq

-- | For case expressions
data Completeness = Complete | Incomplete
  deriving Eq

-- | Drasil Expressions
data Expr where
  Dbl      :: Double -> Expr
  Int      :: Integer -> Expr
  Str      :: String -> Expr
  Perc     :: Integer -> Integer -> Expr
  AssocA   :: AssocArithOper -> [Expr] -> Expr
  AssocB   :: AssocBoolOper  -> [Expr] -> Expr
  -- | Derivative, syntax is:
  --   Type (Partial or total) -> principal part of change -> with respect to
  --   For example: Deriv Part y x1 would be (dy/dx1)
  Deriv    :: DerivType -> Expr -> UID -> Expr
  -- | C stands for "Chunk", for referring to a chunk in an expression.
  --   Implicitly assumes has a symbol.
  C        :: UID -> Expr
  -- | F(x) is (FCall F [x] []) or similar.
  --   FCall accepts a list of params and a list of named params.
  --   F(x,y) would be (FCall F [x,y]) or sim.
  --   F(x,n=y) would be (FCall F [x] [(n,y)]).
  FCall    :: UID -> [Expr] -> [(UID, Expr)] -> Expr
  -- | Actor creation given UID and parameters
  New      :: UID -> [Expr] -> [(UID, Expr)] -> Expr
  -- | Message an actor:
  --   1st UID is the actor,
  --   2nd UID is the method
  Message  :: UID -> UID -> [Expr] -> [(UID, Expr)] -> Expr
  -- | Access a field of an actor:
  --   1st UID is the actor,
  --   2nd UID is the field
  Field    :: UID -> UID -> Expr
  -- | For multi-case expressions, each pair represents one case
  Case     :: Completeness -> [(Expr,Relation)] -> Expr
  Matrix   :: [[Expr]] -> Expr
  
  -- | Unary functions/operations
  UnaryOp       :: UFunc -> Expr -> Expr
  UnaryOpB      :: UFuncB -> Expr -> Expr
  UnaryOpVec    :: UFuncVec -> Expr -> Expr

  -- | Binary functions/operations
  ArithBinaryOp :: ArithBinOp -> Expr -> Expr -> Expr
  BoolBinaryOp  :: BoolBinOp -> Expr -> Expr -> Expr
  EqBinaryOp    :: EqBinOp -> Expr -> Expr -> Expr
  LABinaryOp    :: LABinOp -> Expr -> Expr -> Expr
  OrdBinaryOp   :: OrdBinOp -> Expr -> Expr -> Expr
  VVVBinaryOp   :: VVVBinOp -> Expr -> Expr -> Expr
  VVNBinaryOp   :: VVNBinOp -> Expr -> Expr -> Expr

  -- | Operators are generalized arithmetic operators over a |DomainDesc|
  --   of an |Expr|.  Could be called |BigOp|.
  --   ex: Summation is represented via |Add| over a discrete domain
  Operator :: AssocArithOper -> DomainDesc Expr Expr -> Expr -> Expr
  -- | element of
  IsIn     :: Expr -> Space -> Expr
  -- | a different kind of 'element of'
  RealI    :: UID -> RealInterval Expr Expr -> Expr

($=), ($!=) :: Expr -> Expr -> Expr
($=)  = EqBinaryOp Eq
($!=) = EqBinaryOp NEq

($<), ($>), ($<=), ($>=) :: Expr -> Expr -> Expr
($<)  = OrdBinaryOp Lt
($>)  = OrdBinaryOp Gt
($<=) = OrdBinaryOp LEq
($>=) = OrdBinaryOp GEq

($.) :: Expr -> Expr -> Expr
($.) = VVNBinaryOp Dot

($-), ($/), ($^) :: Expr -> Expr -> Expr
($-) = ArithBinaryOp Subt
($/) = ArithBinaryOp Frac
($^) = ArithBinaryOp Pow

($=>), ($<=>) :: Expr -> Expr -> Expr
($=>)  = BoolBinaryOp Impl
($<=>) = BoolBinaryOp Iff

($&&), ($||) :: Expr -> Expr -> Expr
a $&& b = AssocB And [a,b]
a $|| b = AssocB Or  [a,b]

type Variable = String

data DerivType = Part | Total
  deriving Eq

instance Num Expr where
  (Int 0)        + b              = b
  a              + (Int 0)        = a
  (AssocA Add l) + (AssocA Add m) = AssocA Add (l ++ m)
  (AssocA Add l) + b              = AssocA Add (l ++ [b])
  a              + (AssocA Add l) = AssocA Add (a : l)
  a              + b              = AssocA Add [a, b]

  (AssocA Mul l) * (AssocA Mul m) = AssocA Mul (l ++ m)
  (AssocA Mul l) * b              = AssocA Mul (l ++ [b])
  a              * (AssocA Mul l) = AssocA Mul (a : l)
  a              * b              = AssocA Mul [a, b]

  a - b = ArithBinaryOp Subt a b
  
  fromInteger = Int
  abs         = UnaryOp Abs
  negate      = UnaryOp Neg

  -- this is a Num wart
  signum _ = error "should not use signum in expressions"

instance Eq Expr where
  Dbl a               == Dbl b               =   a == b
  Int a               == Int b               =   a == b
  Str a               == Str b               =   a == b
  AssocA o1 l1        == AssocA o2 l2        =  o1 == o2 && l1 == l2
  AssocB o1 l1        == AssocB o2 l2        =  o1 == o2 && l1 == l2
  Deriv t1 a b        == Deriv t2 c d        =  t1 == t2 && a == c && b == d
  C a                 == C b                 =   a == b
  FCall a b c         == FCall d e f         =   a == d && b == e && c == f
  Case a b            == Case c d            =   a == c && b == d 
  IsIn a b            == IsIn c d            =   a == c && b == d
  UnaryOp a b         == UnaryOp c d         =   a == c && b == d
  UnaryOpB a b        == UnaryOpB c d        =   a == c && b == d
  UnaryOpVec a b      == UnaryOpVec c d      =   a == c && b == d
  ArithBinaryOp o a b == ArithBinaryOp p c d =   o == p && a == c && b == d
  BoolBinaryOp o a b  == BoolBinaryOp p c d  =   o == p && a == c && b == d
  EqBinaryOp o a b    == EqBinaryOp p c d    =   o == p && a == c && b == d
  OrdBinaryOp o a b   == OrdBinaryOp p c d   =   o == p && a == c && b == d
  LABinaryOp o a b    == LABinaryOp p c d    =   o == p && a == c && b == d
  VVVBinaryOp o a b   == VVVBinaryOp p c d   =   o == p && a == c && b == d
  VVNBinaryOp o a b   == VVNBinaryOp p c d   =   o == p && a == c && b == d
  _                   == _                   =   False

instance Fractional Expr where
  a / b = ArithBinaryOp Frac a b
  fromRational r = ArithBinaryOp Frac (fromInteger $ numerator   r)
                                      (fromInteger $ denominator r)
