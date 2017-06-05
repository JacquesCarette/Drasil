{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs #-}
-- | The Drasil Expression language
module Language.Drasil.Expr where

import GHC.Real (Ratio(..)) -- why not Data.Ratio?
import Prelude hiding (id)
import Language.Drasil.Chunk (Chunk(..))
import Language.Drasil.Chunk.SymbolForm (SymbolForm)
import Language.Drasil.Symbol

import Control.Lens ((^.))

--FIXME: Haddock open issue #43 seems to make it so GADT constructors cannot
-- be documented properly

type Relation = Expr

infixr 8 :^
infixl 7 :*
infixl 7 :/
infixl 6 :+
infixl 6 :-
infixr 4 :=
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
  C        :: (SymbolForm c) => c -> Expr -- Chunk (must be 
  -- representable as a symbol)
  FCall    :: Expr -> [Expr] -> Expr -- F(x) is (FCall F [x]) or similar
                                  -- FCall accepts a list of params
                                  -- F(x,y) would be (FCall F [x,y]) or sim.
  Case     :: [(Expr,Relation)] -> Expr -- For multi-case expressions, 
                                     -- each pair represents one case
  UnaryOp  :: UFunc -> Expr
  Grouping :: Expr -> Expr
  BinaryOp :: BiFunc -> Expr
  -- Operator :: Func   -> [Expr] -> Expr
  (:=) :: Expr -> Expr -> Expr
  (:<) :: Expr -> Expr -> Expr
  (:>) :: Expr -> Expr -> Expr
 
type Variable = String

data DerivType = Part
               | Total  
  deriving Eq

instance Num Expr where
  a + b = a :+ b
  a * b = a :* b
  a - b = a :- b
  fromInteger a = Int a

  -- these are Num warts
  signum _ = error "should not use signum in expressions"
  abs _    = error "should not use abs in expressions"


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
  Neg a == Neg b               =  a == b
  Deriv t1 a b == Deriv t2 c d =  t1 == t2 && a == c && b == d
  C a == C b                   =  (a ^. id) == (b ^. id)
  FCall a b == FCall c d       =  a == c && b == d
  Case a == Case b             =  a == b
  (:=) a b == (:=) c d         =  a == c && b == d || a == d && b == c
  (:<) a b == (:<) c d         =  a == c && b == d
  (:>) a b == (:>) c d         =  a == c && b == d
  (:>) a b == (:<) c d         =  a == d && b == c
  _ == _                       =  False

instance Fractional Expr where
  a / b = a :/ b
  fromRational (a :% b) = (fromInteger a :/ fromInteger b)

  
--Known math functions. 
-- TODO: Move the below to a separate file somehow. How to go about it?

data Bound where
  Low :: Expr -> Bound -- Starting value
  High :: Expr -> Bound -- Upper bound, could be a symbol (n), or a value.
  
-- | Unary functions
data UFunc where 
  Log :: Expr -> UFunc
  Summation :: (Maybe (Symbol, Bound, Bound)) -> Expr -> UFunc 
    -- Sum (maybe (index,starting point, ending point)) (sum expression)
    -- where index is used in the sum (i.e. 'i') with a low and high bound
    -- OR Nothing for the first term.
    -- Expr is the expression we are summing over
  Abs :: Expr -> UFunc -- Absolute value
  Norm :: Expr -> UFunc -- Norm
  Integral :: (SymbolForm c) => 
    ((Maybe Bound), (Maybe Bound)) -> Expr -> c -> UFunc
    -- Integral (low,high) Bounds (if any), then (expression to integrate) 
    -- and finally which chunk (variable) we are integrating with respect to.
  Sin :: Expr -> UFunc
  Cos :: Expr -> UFunc
  Tan :: Expr -> UFunc
  Sec :: Expr -> UFunc
  Csc :: Expr -> UFunc
  Cot :: Expr -> UFunc
  
-- | Smart constructor to take the log of an expression
log :: Expr -> Expr
log e = UnaryOp (Log e)

-- | Smart constructor to take the absolute value of an expression
abs :: Expr -> Expr 
abs e = UnaryOp (Abs e)

-- | Smart constructor to apply sin to an expression
sin :: Expr -> Expr
sin e = UnaryOp (Sin e)

-- | Smart constructor to apply cos to an expression
cos :: Expr -> Expr 
cos e = UnaryOp (Cos e)

-- | Smart constructor to apply tan to an expression
tan :: Expr -> Expr
tan e = UnaryOp (Tan e)

-- | Smart constructor to apply sec to an expression
sec :: Expr -> Expr 
sec e = UnaryOp (Sec e)

-- | Smart constructor to apply csc to an expression
csc :: Expr -> Expr
csc e = UnaryOp (Csc e)

-- | Smart constructor to apply cot to an expression
cot :: Expr -> Expr 
cot e = UnaryOp (Cot e)

-- | Binary Functions
data BiFunc where
  Cross :: Expr -> Expr -> BiFunc --Cross Product: HTML &#10799;
  -- Cross product of two expressions
  
-- | Smart constructor to cross product two expressions
cross :: Expr -> Expr -> Expr
cross e1 e2 = BinaryOp (Cross e1 e2)
  