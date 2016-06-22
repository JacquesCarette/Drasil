{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs #-}
module Language.Drasil.Expr where

import GHC.Real (Ratio(..)) -- why not Data.Ratio?

import Language.Drasil.Chunk (Chunk(..), Quantity)

import Control.Lens ((^.))

infixr 8 :^
infixl 7 :*
infixl 7 :/
infixl 6 :+
infixl 6 :-
data Expr where
  V     :: Variable -> Expr
  Dbl   :: Double -> Expr
  Int   :: Integer -> Expr
  (:^)  :: Expr -> Expr -> Expr
  (:*)  :: Expr -> Expr -> Expr
  (:/)  :: Expr -> Expr -> Expr
  (:+)  :: Expr -> Expr -> Expr
  (:-)  :: Expr -> Expr -> Expr
  (:.)  :: Expr -> Expr -> Expr
  Neg   :: Expr -> Expr
  Deriv :: Expr -> Expr -> Expr
  C     :: Quantity c => c -> Expr
  FCall :: Expr -> [Expr] -> Expr --F(x) would be (FCall F [x]) or similar
                                  --FCall accepts a list of params
                                  --F(x,y) would be (FCall F [x,y]) or sim.
  Case  :: [(Expr,Relation)] -> Expr -- For multi-case expressions, 
                                     -- each pair represents one case
infixr 4 :=
data Relation where
  (:=) :: Expr -> Expr -> Relation
  (:<) :: Expr -> Expr -> Relation
  (:>) :: Expr -> Expr -> Relation
 
type Variable = String

instance Num Expr where
  a + b = a :+ b
  a * b = a :* b
  a - b = a :- b
  fromInteger a = Int a

  -- these are Num warts
  signum _ = error "should not use signum in expressions"
  abs _    = error "should not use abs in expressions"


instance Eq Expr where
  V a == V b             =  a == b
  Dbl a == Dbl b         =  a == b
  Int a == Int b         =  a == b
  (:^) a b == (:^) c d   =  a == c && b == d
  (:*) a b == (:*) c d   =  a == c && b == d || a == d && b == c
  (:/) a b == (:/) c d   =  a == c && b == d
  (:+) a b == (:+) c d   =  a == c && b == d || a == d && b == c
  (:-) a b == (:-) c d   =  a == c && b == d
  (:.) a b == (:.) c d   =  a == c && b == d || a == d && b == c
  Neg a == Neg b         =  a == b
  Deriv a b == Deriv c d =  a == c && b == d
  C a == C b             =  (a ^. name) == (b ^. name)
  FCall a b == FCall c d =  a == c && b == d
  Case a == Case b       =  a == b
  _ == _                 =  False

instance Fractional Expr where
  a / b = a :/ b
  fromRational (a :% b) = (fromInteger a :/ fromInteger b)


instance Eq Relation where
  (:=) a b == (:=) c d  =  a == c && b == d || a == d && b == c
  (:<) a b == (:<) c d  =  a == c && b == d
  (:>) a b == (:>) c d  =  a == c && b == d
  (:>) a b == (:<) c d  =  a == d && b == c
  _ == _                =  False
