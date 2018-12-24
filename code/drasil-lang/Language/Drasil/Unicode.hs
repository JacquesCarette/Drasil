-- | Special characters
module Language.Drasil.Unicode where

-- | Special characters including partial derivatives, degree circle, and
-- underscores
data Special = Partial -- remove by having Partial exist in the drasil-printer only
             | Circle  -- remove by refactoring how units are done 
             | Percent -- remove by having "percent" being its own type
  deriving (Eq, Ord)

-- | Class for rendering special characters
class RenderSpecial r where
  special :: Special -> r
