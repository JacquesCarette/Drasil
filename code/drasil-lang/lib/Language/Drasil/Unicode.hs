-- | Special type for unicode characters.
module Language.Drasil.Unicode(Special(Circle), RenderSpecial(special)) where

-- | Special characters include partial derivatives and the degree circle.
data Special = Circle  -- remove by refactoring how units are done
  deriving (Eq, Ord)

-- | Class for rendering special characters.
class RenderSpecial r where
  special :: Special -> r
