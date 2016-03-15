{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs #-}
module ASTInternal where

import GHC.Real (Ratio(..)) -- why not Data.Ratio?
import Chunk (Quantity)

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
  (:=)  :: Expr -> Expr -> Expr
  Neg   :: Expr -> Expr
  Deriv :: Expr -> Expr -> Expr
  C     :: Quantity c => c -> Expr

-- infixr 4 :=
-- data Rel where
  -- (:=) :: Expr -> Expr -> Rel
  -- (:<) :: Expr -> Expr -> Rel
  -- (:>) :: Expr -> Expr -> Rel
 
type Variable = String

data DocType = SRS Filename     --Filename with no extension
             | LPM Filename
             | Code Filename
             | Website Filename
             
type Filename = String

data DocParams = DocClass String String --SqBracks vs. Braces
               | UsePackages [String] -- Package name list
               | ExDoc String String --SqBracks vs. Braces

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
