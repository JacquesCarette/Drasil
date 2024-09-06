-- | Drasil uses symbols in expressions and sentences.
module Language.Drasil.Symbol (
  -- * Types
  Decoration(..), Symbol(..),
  -- * Classes
  HasSymbol(..),
  -- * Ordering Function
  compsy
) where

import Language.Drasil.Stages (Stage)
import Language.Drasil.Unicode(Special)

import Data.Char (toLower)

-- | Decorations on symbols/characters such as hats or Vector representations
-- (determines bolding, italics, etc).
data Decoration = 
    Hat       -- ^ Places a @^@ on top of a symbol.
  | Vector    -- ^ Makes a symbol bold.
  | Prime     -- ^ Appends a @'@ to a symbol.
  | Delta     -- ^ Prepends a @Δ@ to a symbol.
  | Magnitude -- ^ Places @||@ before and after a symbol.
  deriving (Eq, Ord)

-- | A 'Symbol' is actually going to be a graphical description of what gets
-- rendered as a (unique) symbol.  This is actually NOT based on semantics at
-- all, but just a description of how things look.
-- 
-- Symbols can be:
-- 
--     * @'Variable'@ (string such as "x" that represent a value that can vary) 
--     * @'Label'@ (strings such as "max" or "target" that represent a single idea)
--     * @'Special'@ characters (ex. unicode)
--     * @Decorated@ symbols using 'Atop'
--     * @Concatenations@ of symbols, including subscripts and superscripts
--     * @'Empty'@! (this is to give this a monoid-like flavour)
data Symbol =
    Variable String -- ^ Basic variable name creation.
  | Label    String 
    -- ^ For when symbols need more context, but we don't want to add a new variable name.
    -- For example, @v_f@ may be encoded as @Concat [variable "v", label "f"]@.
  | Integ    Int -- ^ For using numbers in Symbols.
  | Special  Special 
    -- ^ For now, special characters are the degree and partial
    -- differentiation symbols. These should eventually move elsewhere
    -- and the 'Special' type removed.
  | Atop     Decoration Symbol
    -- ^ Used to decorate symbols. For things like vectors (which need to be bold),
    -- primes, magnitudes, etc. See 'Decoration' for more details.
  | Corners  [Symbol] [Symbol] [Symbol] [Symbol] Symbol
    -- ^ Order of Symbols: upleft   lowleft  upright  lowright base. Ex:
    --
    -- >Corners [1]   [2]   [3]   [4]   [5]
    -- @
    --  Visually:  [1]   [3]
    --
    --                [5]
    --
    --             [2]   [4]
    -- @
  | Concat   [Symbol] -- ^ Concatentation of two symbols: @[s1, s2] -> s1s2@
  | Empty -- ^ Placeholder for when a symbol is not needed.
  deriving Eq

-- TODO: Instead of having "Stage" as a parameter of "symbol", we can make it a typeclass parameter instead.. extensibility for cheap!
-- | A HasSymbol is anything which has a 'Symbol'.
class HasSymbol c where
  -- | Provides the 'Symbol' for a particular stage of generation.
  symbol  :: c -> Stage -> Symbol

-- | Symbols may be concatenated.
instance Semigroup Symbol where
 a <> b = Concat [a , b]

-- | Symbols can be empty or concatenated.
instance Monoid Symbol where
  mempty = Empty

-- | Gives an 'Ordering' of two lists of 'Symbol's.
complsy :: [Symbol] -> [Symbol] -> Ordering
complsy [] [] = EQ
complsy [] _  = LT
complsy _  [] = GT
complsy (x : xs) (y : ys) = compsy x y <> complsy xs ys

-- | The default compare function that sorts all the lower case symbols after
-- the upper case ones.
--
-- Comparation is used twice for each `Atomic` case, once for making sure they
-- are the same letter, once for case sensitive. As far as this comparison is
-- considered, `Δ` is a "decoration" and ignored unless the compared symbols are
-- the exact same, in which case it is ordered after the undecorated symbol.
--
-- Superscripts and subscripts are ordered after the base symbols (because they
-- add additional context to a symbol). For example: `v_f^{AB}` (expressed in
-- LaTeX notation for clarity), where `v_f` is a final velocity, and the `^{AB}`
-- adds context that it is the final velocity between points `A` and `B`. In
-- these cases, the sorting of `v_f^{AB}` should be following `v_f` as it is
-- logical to place it with its parent concept.
compsy :: Symbol -> Symbol -> Ordering
compsy (Concat x) (Concat y) = complsy x y
compsy (Concat a) b = complsy a [b]
compsy b (Concat a) = complsy [b] a
compsy (Atop d1 a) (Atop d2 a') = 
  case compsy a a' of
    EQ -> compare d1 d2
    other -> other
compsy a (Atop Magnitude b) =
  case compsy a b of
    EQ -> LT
    other -> other
compsy (Atop Magnitude b) a =
 case compsy b a of
    EQ -> GT
    other -> other
compsy a (Atop Delta b) =
  case compsy a b of
    EQ -> LT
    other -> other
compsy (Atop Delta b) a =
 case compsy b a of
    EQ -> GT
    other -> other
-- The next two cases are very specific (but common) patterns where a superscript is added
-- to some "conceptual" base symbol to add further context. For example: `v_f^{AB}` (expressed in LaTeX
-- notation for clarity), where `v_f` is a final velocity, and the `^{AB}` adds context that it is the
-- final velocity between points `A` and `B`. In these cases, the sorting of `v_f^{AB}` should be
-- following `v_f` as it is logical to place it with its parent concept.
compsy (Corners [] [] ur [] (Corners [] [] [] lr b)) a = compsy (Corners [] [] ur lr b) a
compsy a (Corners [] [] ur [] (Corners [] [] [] lr b)) = compsy a (Corners [] [] ur lr b)
compsy (Corners _ _ u l b) (Corners _ _ u' l' b')  =
  case compsy b b' of
    EQ -> case complsy l l' of
      EQ -> complsy u u'
      other -> other
    other -> other
compsy a (Corners _ _ _ _ b) =
  case compsy a b of
    EQ -> LT
    other -> other
compsy (Corners _ _ _ _ b) a =
  case compsy b a of
    EQ -> GT
    other -> other
compsy a (Atop _ b) =
  case compsy a b of
    EQ -> LT
    other -> other
compsy (Atop _ b) a =
 case compsy b a of
    EQ -> GT
    other -> other
{-
compsy a (Atop d b) =
  case d of
    Magnitude -> case compsy a b of
                  EQ -> GT
                  other -> other
    Delta -> case compsy a b of
                  EQ -> GT
                  other -> other
    _ -> case compsy a b of
                  EQ -> LT
                  other -> other
compsy (Atop d a) b =
  case d of
    Magnitude -> case compsy a b of
                  EQ -> LT
                  other -> other
    Delta -> case compsy a b of
                  EQ -> LT
                  other -> other
    _ -> case compsy a b of
                  EQ -> GT
                  other -> other-}
compsy (Special a)  (Special b)  = compare a b
compsy (Integ    x) (Integ    y) = compare x y
compsy (Variable x) (Variable y) = compsyLower x y
compsy (Variable x) (Label y)    = compsyLower x y
compsy (Label x)    (Variable y) = compsyLower x y
compsy (Label x)    (Label y)    = compsyLower x y
compsy (Special _)  _ = LT
compsy _ (Special _)  = GT
compsy (Integ _) _    = LT
compsy _ (Integ _)    = GT
compsy (Variable _) _ = LT
compsy _ (Variable _) = GT
compsy (Label _) _    = LT
compsy _ (Label _)    = GT
compsy Empty Empty    = EQ

-- | Helper for 'compsy' that compares lower case 'String's.
compsyLower :: String -> String -> Ordering
compsyLower x y = case compare (map toLower x) (map toLower y) of
  EQ    -> compare x y 
  other -> other