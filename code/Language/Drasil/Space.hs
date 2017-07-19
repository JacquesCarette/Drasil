{-# LANGUAGE GADTs #-}
module Language.Drasil.Space where

-- FIXME: These need to be spaces and not just types, but until Steven
--  has a chance to integrate his work I think this should be left alone
-- | Spaces
data Space where
  Integer :: Space
  Rational :: Space
  Real :: Space
  Natural :: Space
  Boolean :: Space
  Char :: Space
  String :: Space
  Radians :: Space
  Vect :: Space -> Space
  Obj :: String -> Space
  --Discrete :: Space -> Space
  --DiscreteI :: [Int] -> Space --ex. let A = {1,2,4,7}
  --DiscreteD :: [Double] -> Space
  --DiscreteS :: [String] -> Space --ex. let Meal = {"breakfast","lunch", "dinner"}
  deriving Eq
  