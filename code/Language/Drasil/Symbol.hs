{-# LANGUAGE GADTs #-}

-- A 'Symbol' is actually going to be a graphical description of what
-- gets rendered as a (unique) symbol.  This is actually NOT based on
-- semantics at all, but just a description of how things look.

module Language.Drasil.Symbol where

import Language.Drasil.Unicode 
import Data.Char (toLower)

data Decoration = Hat | Vector deriving (Eq, Ord)

data Symbol where
  Atomic   :: String -> Symbol
  Special  :: Special -> Symbol
  Greek    :: Greek -> Symbol
  Atop  :: Decoration -> Symbol -> Symbol
  Corners  :: [Symbol] -> [Symbol] -> [Symbol] -> [Symbol] -> Symbol -> Symbol
            --upleft  -> lowleft  -> upright  -> lowright -> base   -> out
            -- [1] -> [2] -> [3] -> [4] -> [5] -> out
            --  Visually:  [1]   [3]
            --    (out)       [5]
            --             [2]   [4]
  Concat :: [ Symbol ]  -> Symbol
            -- [s1, s2] -> s1s2
  deriving Eq

--FIXME? The exact ordering we want may need to be updated, or should we
--  allow custom?  
instance Ord Symbol where
  compare (Corners _ _ ur lr b) (Corners _ _ u' l' b') = 
    case compare b b' of
      EQ -> case compare lr l' of
            EQ -> compare ur u'
            other -> other
      other -> other
  compare (Corners _ _ _ _ a)    b                     = compare a b
  compare (Concat (x:[]))       (Concat (y:[]))        = compare x y
  compare (Concat (x:xs))       (Concat (x':ys))       = 
    case compare x x' of
      EQ -> compare xs ys
      other -> other
  compare (Concat (a:_))         b                     = compare a b
  compare (Concat [])            _                     = 
    error "Attempting to compare empty symbol"
  compare (Atop d1 a)           (Atop d2 a')           = 
    case compare a a' of
      EQ -> compare d1 d2
      other -> other
  compare (Atop _ a)             b                     = compare a b
  compare (Atomic (x:xs))       (Atomic (y:ys))        = 
    case compare (toLower x) (toLower y) of
      EQ -> compare xs ys
      other -> other
  compare (Atomic _)             _                     = LT
  compare  _                    (Atomic _)             = GT
  compare (Greek a)             (Greek b)              = compare a b
  compare (Greek _)              _                     = LT
  compare  _                    (Greek _)              = GT
  compare (Special a)           (Special b)            = compare a b
  compare (Special _)            _                     = LT
  
  
upper_left :: Symbol -> Symbol -> Symbol
upper_left b ul = Corners [ul] [] [] [] b

sub :: Symbol -> Symbol -> Symbol
sub b lr = Corners [] [] [] [lr] b

sup :: Symbol -> Symbol -> Symbol
sup b ur = Corners [] [] [ur] [] b

hat :: Symbol -> Symbol
hat = Atop Hat

vec :: Symbol -> Symbol
vec = Atop Vector
