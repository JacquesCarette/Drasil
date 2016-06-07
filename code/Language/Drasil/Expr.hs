{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs #-}
module Language.Drasil.Expr where

import GHC.Real (Ratio(..)) -- why not Data.Ratio?

import Language.Drasil.Chunk (Quantity)

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

instance Fractional Expr where
  a / b = a :/ b
  fromRational (a :% b) = (fromInteger a :/ fromInteger b)
