-- | A 'Symbol' is actually going to be a graphical description of what
-- gets rendered as a (unique) symbol.  This is actually NOT based on
-- semantics at all, but just a description of how things look.

module Language.Drasil.Symbol(Decoration(..), Symbol(..), compsy,
 upper_left, sub, sup, hat, vec, prime, sCurlyBrSymb) where

import Language.Drasil.Unicode (Special(CurlyBrClose, CurlyBrOpen))

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
-- - empty! (this is to give this a monoid-like flavour)
data Symbol =
    Atomic  String
  | Special Special
--  | Greek   Greek
  | Atop    Decoration Symbol
  | Corners [Symbol] [Symbol] [Symbol] [Symbol] Symbol
          -- upleft   lowleft  upright  lowright base
          -- [1]      [2]      [3]      [4]      [5]
          --  Visually:  [1]   [3]
          --    (out)       [5]
          --             [2]   [4]
  | Concat [Symbol]
            -- [s1, s2] -> s1s2
  | Empty
  deriving Eq

instance Monoid Symbol where
  mempty = Empty
  mappend a b = Concat [a , b]

complsy :: [Symbol] -> [Symbol] -> Ordering
complsy [] [] = EQ
complsy [] _  = LT
complsy _  [] = GT
complsy (x : xs) (y : ys) = compsy x y `mappend` complsy xs ys

-- The default compare function sorts all the lower case after the upper case
-- Comparation is used twice for each case,
-- Once for making sure they are the same letter, once for case sensitive.
compsy :: Symbol -> Symbol -> Ordering
compsy (Concat (x:[]))       (Concat (y:[]))       = compsy x y
compsy (Concat (Atomic "Δ":(Atomic x):_)) (Atomic y)      = 
  case compare (map toLower x) (map toLower y) of
    EQ -> GT
    other -> other
compsy (Concat (Atomic "Δ":x:_))           y      = 
  case compsy x y of
    EQ -> GT
    other -> other
compsy (Atomic x)  (Concat (Atomic "Δ":(Atomic y):_))      = 
  case compare (map toLower x) (map toLower y) of
    EQ -> LT
    other -> other
compsy a           (Concat (Atomic "Δ":y:_))      = 
  case compsy a y of
    EQ -> LT
    other -> other
compsy (Concat (x:xs))       (Concat (y:ys))       = 
 compsy x y `mappend` complsy xs ys
compsy (Concat a)             b                    = complsy a [b]
compsy b                      (Concat a)           = complsy [b] a
compsy (Corners _ _ u l (Atomic b)) (Corners _ _ u' l' (Atomic b'))  =
  case compare (map toLower b) (map toLower b') of
    EQ -> case complsy l l' of
      EQ -> complsy u u'
      other -> other
    other -> other
compsy (Corners _ _ u l b) (Corners _ _ u' l' b')  =
  case compsy b b' of
    EQ -> case complsy l l' of
      EQ -> complsy u u'
      other -> other
    other -> other
compsy (Atomic a)              (Corners _ _ _ _ (Atomic b)) = 
  case compare a b of
    EQ -> LT
    _  -> case compare (map toLower a) (map toLower b) of
      EQ -> LT
      other -> other
compsy (Corners _ _ _ _ (Atomic b))     (Atomic a)          = 
  case compare b a of
    EQ -> GT
    _  -> case compare (map toLower b) (map toLower a) of
      EQ -> GT
      other -> other
compsy (Corners _ _ _ _ b)     a                   = compsy b a
compsy a                       (Corners _ _ _ _ b) = compsy a b
compsy (Atop d1 a)             (Atop d2 a')        = 
  case compsy a a' of
    EQ -> compare d1 d2
    other -> other
compsy (Atomic a)              (Atop _ (Atomic b))          = 
  case compare a b of
    EQ -> LT
    _  -> case compare (map toLower a) (map toLower b) of
      EQ -> LT
      other -> other
compsy (Atop _ (Atomic b))              (Atomic a)          = 
 case compare b a of
    EQ -> GT
    _  -> case compare (map toLower b) (map toLower a) of
      EQ -> GT
      other -> other
compsy (Atop _ a)              b                   = compsy a b  
compsy b                       (Atop _ a)          = compsy b a
compsy (Special a)             (Special b)         = compare a b
compsy _                       (Special _)         = GT
compsy (Special _)             _                   = LT
compsy (Atomic x)              (Atomic y)          = 
  case compare (map toLower x) (map toLower y) of
    EQ -> compare x y
    other -> other
compsy (Atomic _)              _                   = LT
compsy  _                      (Atomic _)          = GT
compsy  Empty                  Empty               = EQ

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
sCurlyBrSymb x = Concat [Special CurlyBrOpen, x, Special CurlyBrClose]
