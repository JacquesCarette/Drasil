-- | Special characters
module Language.Drasil.Unicode(Special(Partial, Circle), RenderSpecial(special)) where

-- | Special characters including partial derivatives, degree circle, and
-- underscores
data Special = Partial -- remove by having Partial exist in the drasil-printer only
             | Circle  -- remove by refactoring how units are done 
  deriving (Eq, Ord)

-- | Class for rendering special characters
class RenderSpecial r where
  special :: Special -> r
