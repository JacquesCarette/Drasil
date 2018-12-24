-- | Special characters
module Language.Drasil.Unicode where

-- | Special characters including partial derivatives, degree circle, and
-- underscores
data Special = Partial
             | Circle
             | Percent
             | Hash
             | UScore
  deriving (Eq, Ord)

-- | Class for rendering special characters
class RenderSpecial r where
  special :: Special -> r
