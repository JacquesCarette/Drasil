-- | A 'Symbol' is actually going to be a graphical description of what
-- gets rendered as a (unique) symbol.  This is actually NOT based on
-- semantics at all, but just a description of how things look.

module Language.Drasil.Symbol(Decoration(..),Symbol(..),compsy, 
 upper_left, sub, sup, hat, vec, prime,
  sCurlyBrSymb,
 Stage(..)) where

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
-- - empty! (this is to give this a monoid-like flavour)
data Symbol =
    Atomic  String
  | Special Special
  | Greek   Greek
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

compsy :: Symbol -> Symbol -> Ordering
compsy (Concat (x:[]))       (Concat (y:[]))        = compsy x y
compsy (Concat ((Greek Delta):xs))                b = compsy (Concat xs) b 
compsy a                (Concat ((Greek Delta):ys)) = compsy a           (Concat ys)
compsy (Concat (x:xs))       (Concat (y:ys))       = --The above two lines ensure symbols like "x" and "delta x" stay together
 compsy x y `mappend` complsy xs ys
compsy (Concat a)             b                     = complsy a [b]
compsy b                      (Concat a)            = complsy [b] a
compsy (Corners _ _ ur lr b) (Corners _ _ u' l' b') = 
  case compsy b b' of
    EQ -> case complsy lr l' of
          EQ -> complsy ur u'
          other -> other
    other -> other
compsy (Corners _ _ _ _ a)    b                     = compsy a b
compsy b                      (Corners _ _ _ _ a)   = compsy b a
compsy (Atop d1 a)           (Atop d2 a')           = 
  case compsy a a' of
    EQ -> compare d1 d2
    other -> other
compsy (Atop _ a)             b                     = compsy a b  
compsy b                      (Atop _ a)            = compsy b a
compsy (Atomic x)             (Atomic y)            = 
  compare (map toLower x) (map toLower y)
compsy (Special a)           (Special b)            = compare a b
compsy (Special _)            _                     = LT
compsy _                     (Special _)            = GT
compsy (Atomic _)             _                     = LT
compsy  _                    (Atomic _)             = GT
compsy (Greek a)             (Greek b)              = compare a b
compsy  Empty                 Empty                 = EQ
compsy  _                     Empty                 = GT
compsy  Empty                 _                     = LT

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

--------------

-- Also define 'Stages' here (break out?), which are Symbol qualifiers
-- FIXME: More fine-grained stages.
-- | Stages correspond to what we're trying to look up. They range from abstract
-- to concrete.                  
data Stage = Equational -- AKA Theoretical / Abstract-design
           | Implementation -- AKA Implementation / Detailed-design

{- Note: Keep stages separate from StagedSymbols for lookup purposes, as we may
   have documents which look up both stages of a symbol and show them 
   side-by-side or one after another. -}

-- | For better error messages.
instance Show Stage where
  show Equational     = "Theoretical stage"
  show Implementation = "Implementation Stage"

