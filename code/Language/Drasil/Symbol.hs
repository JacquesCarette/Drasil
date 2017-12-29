{-# LANGUAGE GADTs #-}

-- | A 'Symbol' is actually going to be a graphical description of what
-- gets rendered as a (unique) symbol.  This is actually NOT based on
-- semantics at all, but just a description of how things look.

module Language.Drasil.Symbol where

import Language.Drasil.Unicode 
import Data.Char (toLower)

-- | Decorations on symbols/characters such as hats or Vector representations
-- (bolding/etc)
data Decoration = Hat | Vector | Prime deriving (Eq, Ord)

-- | Symbols can be:
-- - atomic (strings such as "A" or "max" that represent a single idea)
-- - special characters (ex. unicode)
-- - Greek characters
-- - Decorated symbols
-- - Concatenations of symbols, including subscripts and superscripts
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
  compare (Concat (x:[]))       (Concat (y:[]))        = compare x y
  compare (Concat ((Greek Delta):xs))                b = compare (Concat xs) b 
  compare a                (Concat ((Greek Delta):ys)) = compare a           (Concat ys)
  compare (Concat (x:xs))       (Concat (x':ys))       = --The above two lines ensure symbols like "x" and "delta x" stay together
    case compare x x' of
      EQ -> compare xs ys
      other -> other
  compare (Concat a)             b                     = compare a [b]
  compare b                      (Concat a)            = compare [b] a
  compare (Corners _ _ ur lr b) (Corners _ _ u' l' b') = 
    case compare b b' of
      EQ -> case compare lr l' of
            EQ -> compare ur u'
            other -> other
      other -> other
  compare (Corners _ _ _ _ a)    b                     = compare a b
  compare b                      (Corners _ _ _ _ a)   = compare b a
  compare (Atop d1 a)           (Atop d2 a')           = 
    case compare a a' of
      EQ -> compare d1 d2
      other -> other
  compare (Atop _ a)             b                     = compare a b  
  compare b                      (Atop _ a)            = compare b a
  compare (Atomic x)             (Atomic y)            = 
    compare (map toLower x) (map toLower y)
  compare (Special a)           (Special b)            = compare a b
  compare (Special _)            _                     = LT
  compare _                     (Special _)            = GT
  compare (Atomic _)             _                     = LT
  compare  _                    (Atomic _)             = GT
  compare (Greek a)             (Greek b)              = compare a b

-- | Helper for creating a symbol with a superscript on the left side of the symbol.
-- Arguments: Base symbol, then superscripted symbol.
upper_left :: Symbol -> Symbol -> Symbol
upper_left b ul = Corners [ul] [] [] [] b

-- | Helper for creating a symbol with a subscript to the right.
-- Arguments: Base symbol, then subscripted symbol.
sub :: Symbol -> Symbol -> Symbol
sub b lr = Corners [] [] [] [lr] b

-- | Helper for creating a symbol with a superscript to the right.
-- Arguments: Base symbol, then superscripted symbol.
sup :: Symbol -> Symbol -> Symbol
sup b ur = Corners [] [] [ur] [] b

-- | Helper for creating a symbol with a hat ("^") atop it.
hat :: Symbol -> Symbol
hat = Atop Hat

-- | Helper for creating a Vector symbol.
vec :: Symbol -> Symbol
vec = Atop Vector

-- | Helper for creating a Vector symbol.
prime :: Symbol -> Symbol
prime = Atop Prime

-- | Helper for adding {} around a symbol (used for coordinates).
sCurlyBrSymb :: Symbol -> Symbol
sCurlyBrSymb x = Concat [(Special CurlyBrOpen), x, (Special CurlyBrClose)]
