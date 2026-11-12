module Drasil.Document.Layout.Typography (
  -- * Types
  Fonts(..), Spacing(..), Fence(..), OverSymb(..), Special(..)
) where

-- | Different font effects (__bold__, /emphasis/).
data Fonts = Bold | Emph
-- | Spacing is Thin.
data Spacing = Thin
-- | Holds the type of "text fencing" ("(), {}, |, ||").
data Fence = Paren | Curly | Norm | Abs
-- | The "^" symbol.
data OverSymb = Hat
-- | Special characters.
data Special = Circle -- ^ The degree symbol.
  deriving (Eq, Ord)
