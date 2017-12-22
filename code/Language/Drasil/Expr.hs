{-# LANGUAGE GADTs #-}
-- | The Drasil Expression language
module Language.Drasil.Expr where

import Data.Ratio (numerator,denominator)
import Prelude hiding (id, sqrt)
import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Symbol
import Language.Drasil.Space (Space(..))

import Control.Lens ((^.))

--FIXME: Haddock open issue #43 seems to make it so GADT constructors cannot
-- be documented properly

type Relation = Expr

infixr 8 :^
infixl 7 :*
infixl 7 :/
infixl 6 :+
infixl 6 :-
infixr 4 $=

-- | Drasil Expressions
data Expr where
  V        :: Variable -> Expr
  Dbl      :: Double -> Expr
  Int      :: Integer -> Expr
  (:^)     :: Expr -> Expr -> Expr -- Power operator
  (:*)     :: Expr -> Expr -> Expr -- Multiplication
  (:/)     :: Expr -> Expr -> Expr -- Division
  (:+)     :: Expr -> Expr -> Expr -- Addition
  (:-)     :: Expr -> Expr -> Expr -- Subtraction
  (:.)     :: Expr -> Expr -> Expr -- Dot product
  Neg      :: Expr -> Expr -- Negation
  Deriv    :: DerivType -> Expr -> Expr -> Expr -- Derivative, syntax is:
  -- Type (Partial or total) -> principal part of change -> with respect to
  -- For example: Deriv Part y x1 would be (dy/dx1)*dx1
  C        :: (Chunk c) => c -> Expr -- Chunk (must have a symbol)
  FCall    :: Expr -> [Expr] -> Expr -- F(x) is (FCall F [x]) or similar
                                  -- FCall accepts a list of params
                                  -- F(x,y) would be (FCall F [x,y]) or sim.
  Case     :: [(Expr,Relation)] -> Expr -- For multi-case expressions,
                                     -- each pair represents one case
  Matrix   :: [[Expr]] -> Expr
  Index    :: Expr -> Expr -> Expr  -- for accessing elements of sequence/list/vect etc.
                                    -- arr[i] is (Index arr i)
  Len      :: Expr -> Expr          -- length
  Append   :: Expr -> Expr -> Expr  -- need this for now since types don't reach generation
                                    -- can probably just use addition between list types for this later
  UnaryOp  :: UFunc -> Expr
  Grouping :: Expr -> Expr
  BinaryOp :: BiFunc -> Expr
  -- Operator :: Func   -> [Expr] -> Expr
  EEquals    :: Expr -> Expr -> Expr
  ENEquals   :: Expr -> Expr -> Expr
  ELess      :: Expr -> Expr -> Expr
  EGreater   :: Expr -> Expr -> Expr
  ELessEq    :: Expr -> Expr -> Expr
  EGreaterEq :: Expr -> Expr -> Expr
  -- start of logic Expr
  (:&&)    :: Expr -> Expr -> Expr -- logical and
  (:||)    :: Expr -> Expr -> Expr -- logical or
  Not      :: Expr -> Expr -- logical not

  IsIn  :: Expr -> Space -> Expr --	element of

  ForAll   :: Symbol -> Expr -> Expr
  Exists   :: Symbol -> Expr -> Expr
  (:=>)  :: Expr -> Expr -> Expr -- implies, &rArr; \implies
  (:<=>) :: Expr -> Expr -> Expr -- if and only if, &hArr; \iff

($=), ($!=), ($<), ($>), ($<=), ($>=) :: Expr -> Expr -> Expr
($=)  = EEquals
($!=) = ENEquals
($<)  = ELess
($>)  = EGreater
($<=) = ELessEq
($>=) = EGreaterEq

type Variable = String

data DerivType = Part
               | Total
  deriving Eq

instance Num Expr where
  a + b = a :+ b
  a * b = a :* b
  a - b = a :- b
  fromInteger a = Int a
  abs = UnaryOp . Abs

  -- this is a Num wart
  signum _ = error "should not use signum in expressions"


instance Eq Expr where
  V a == V b                   =  a == b
  Dbl a == Dbl b               =  a == b
  Int a == Int b               =  a == b
  (:^) a b == (:^) c d         =  a == c && b == d
  (:*) a b == (:*) c d         =  a == c && b == d || a == d && b == c
  (:/) a b == (:/) c d         =  a == c && b == d
  (:+) a b == (:+) c d         =  a == c && b == d || a == d && b == c
  (:-) a b == (:-) c d         =  a == c && b == d
  (:.) a b == (:.) c d         =  a == c && b == d || a == d && b == c
  Not a == Not b               =  a == b
  Neg a == Neg b               =  a == b
  Deriv t1 a b == Deriv t2 c d =  t1 == t2 && a == c && b == d
  C a == C b                   =  (a ^. id) == (b ^. id)
  FCall a b == FCall c d       =  a == c && b == d
  Case a == Case b             =  a == b
  EEquals  a b == EEquals  c d      =  a == c && b == d || a == d && b == c
  ENEquals a b == ENEquals c d      =  a == c && b == d || a == d && b == c
  ELess  a b == ELess  c d          =  a == c && b == d
  EGreater  a b == EGreater  c d    =  a == c && b == d
  ELessEq a b == ELessEq c d        =  a == c && b == d
  EGreaterEq a b == EGreaterEq c d  =  a == c && b == d
  --Logic
  (:&&) a b  == (:&&) c d      =  a == c && b == d || a == d && b == c
  (:||) a b  == (:||) c d      =  a == c && b == d || a == d && b == c
  (:=>) a b  == (:=>) c d      =  a == c && b == d
  (:<=>) a b == (:<=>) c d     =  a == c && b == d || a == d && b == c
  IsIn  a b  == IsIn  c d      =  a == c && b == d
  ForAll a b == ForAll c d     =  a == c && b == d -- not quite right...
  Exists a b == Exists c d     =  a == c && b == d -- not quite right...
  _ == _                       =  False

instance Fractional Expr where
  a / b = a :/ b
  fromRational r = (fromInteger $ numerator   r) :/
                   (fromInteger $ denominator r)


--Known math functions.
-- TODO: Move the below to a separate file somehow. How to go about it?

data Bound where
  Low :: Expr -> Bound -- Starting value
  High :: Expr -> Bound -- Upper bound, could be a symbol (n), or a value.

-- | Binary Functions
data BiFunc where
  Cross :: Expr -> Expr -> BiFunc --Cross Product: HTML &#10799;
  -- Cross product of two expressions

-- | Unary functions
data UFunc where
  Summation :: (Maybe (Symbol, Bound, Bound)) -> Expr -> UFunc
    -- Sum (maybe (index,starting point, ending point)) (sum expression)
    -- where index is used in the sum (i.e. 'i') with a low and high bound
    -- OR Nothing for the first term.
    -- Expr is the expression we are summing over
  Product :: (Maybe (Symbol, Bound, Bound)) -> Expr -> UFunc
  Integral :: ((Maybe Bound), (Maybe Bound)) -> Expr -> Expr -> UFunc
    -- Integral (low,high) Bounds (if any), then (expression to integrate)
    -- and finally which chunk (variable) we are integrating with respect to.
    -- FIXME: The chunk/var wrt is currently Expr because Quantity (requisite)
    -- causes cyclic imports
  Norm   :: Expr -> UFunc -- Norm
  Abs    :: Expr -> UFunc -- Absolute value
  Log    :: Expr -> UFunc
  Sin    :: Expr -> UFunc
  Cos    :: Expr -> UFunc
  Tan    :: Expr -> UFunc
  Sec    :: Expr -> UFunc
  Csc    :: Expr -> UFunc
  Cot    :: Expr -> UFunc
  Exp    :: Expr -> UFunc
  Sqrt   :: Expr -> UFunc
