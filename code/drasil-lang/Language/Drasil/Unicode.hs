module Language.Drasil.Unicode(Special(Partial, Circle), RenderSpecial(special)) where

-- | Special characters include partial derivatives and the degree circle.
data Special = Partial -- remove by having Partial exist in the drasil-printer only
             | Circle  -- remove by refactoring how units are done 
  deriving (Eq, Ord)

-- | Class for rendering special characters.
class RenderSpecial r where
  special :: Special -> r
