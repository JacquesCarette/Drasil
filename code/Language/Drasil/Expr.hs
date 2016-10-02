{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs #-}
module Language.Drasil.Expr where

import GHC.Real (Ratio(..)) -- why not Data.Ratio?

import Language.Drasil.Chunk (Chunk(..), Quantity)
import Language.Drasil.Symbol

import Control.Lens ((^.))

type Relation = Expr

infixr 8 :^
infixl 7 :*
infixl 7 :/
infixl 6 :+
infixl 6 :-
infixr 4 :=
data Expr where
  V        :: Variable -> Expr
  Dbl      :: Double -> Expr
  Int      :: Integer -> Expr
  (:^)     :: Expr -> Expr -> Expr
  (:*)     :: Expr -> Expr -> Expr
  (:/)     :: Expr -> Expr -> Expr
  (:+)     :: Expr -> Expr -> Expr
  (:-)     :: Expr -> Expr -> Expr
  (:.)     :: Expr -> Expr -> Expr
  Neg      :: Expr -> Expr
  Deriv    :: DerivType -> Expr -> Expr -> Expr
  C        :: Quantity c => c -> Expr
  FCall    :: Expr -> [Expr] -> Expr --F(x) is (FCall F [x]) or similar
                                  --FCall accepts a list of params
                                  --F(x,y) would be (FCall F [x,y]) or sim.
  Case     :: [(Expr,Relation)] -> Expr -- For multi-case expressions, 
                                     -- each pair represents one case
  UnaryOp  :: UFunc -> Expr
  Grouping :: Expr -> Expr
  -- BinaryOp :: BiFunc -> Expr
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
  C a == C b                   =  (a ^. name) == (b ^. name)
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
  
  
data UFunc where 
  Log :: Expr -> UFunc
  Summation :: (Maybe (Symbol, Bound, Bound)) -> Expr -> UFunc 
    --Sum (maybe (index,starting point, ending point)) (sum expression)
  Abs :: Expr -> UFunc
  Integral :: Quantity c => ((Maybe Bound), (Maybe Bound)) -> Expr -> c -> UFunc
    --Integral (low,high) Bounds (expression to integrate) (w.r.t. chunk)
  Sin :: Expr -> UFunc
  Cos :: Expr -> UFunc
  Tan :: Expr -> UFunc
  Sec :: Expr -> UFunc
  Csc :: Expr -> UFunc
  Cot :: Expr -> UFunc
  
log :: Expr -> Expr
log e = UnaryOp (Log e)

abs :: Expr -> Expr 
abs e = UnaryOp (Abs e)

sin :: Expr -> Expr
sin e = UnaryOp (Sin e)

cos :: Expr -> Expr 
cos e = UnaryOp (Cos e)

tan :: Expr -> Expr
tan e = UnaryOp (Tan e)

sec :: Expr -> Expr 
sec e = UnaryOp (Sec e)

csc :: Expr -> Expr
csc e = UnaryOp (Csc e)

cot :: Expr -> Expr 
cot e = UnaryOp (Cot e)

data BiFunc where
  Cross :: Expr -> Expr -> BiFunc --Cross Product: HTML &#10799;
  
--cross :: Expr -> Expr -> Expr
--cross e1 e2 = BinaryOp (Cross e1 e2)
  