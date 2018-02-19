module Language.Drasil.Space where

import Control.Lens (Lens')
-- FIXME: These need to be spaces and not just types, but until Steven
--  has a chance to integrate his work I think this should be left alone
-- | Spaces
data Space =
    Integer
  | Rational
  | Real
  | Natural
  | Boolean
  | Char
  | String
  | Radians
  | Vect Space
  | Obj String
  | DiscreteI [Int]  --ex. let A = {1, 2, 4, 7}
  | DiscreteD [Double]
  | DiscreteS [String] --ex. let Meal = {"breakfast", "lunch", "dinner"}
  deriving Eq
  
-- | HasSpace is anything which has a Space...
class HasSpace c where
  typ      :: Lens' c Space
