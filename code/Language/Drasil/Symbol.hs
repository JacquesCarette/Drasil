{-# LANGUAGE GADTs #-}

-- A 'Symbol' is actually going to be a graphical description of what
-- gets rendered as a (unique) symbol.  This is actually NOT based on
-- semantics at all, but just a description of how things look.

module Language.Drasil.Symbol where

import Language.Drasil.Unicode 

data Decoration = Hat | Vector deriving Eq

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
