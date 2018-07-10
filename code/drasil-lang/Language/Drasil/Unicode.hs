-- | Special characters
module Language.Drasil.Unicode where

-- | Greek alphabet. @_L@ following the name represents lower case version.
--data Greek = 
            
           
 -- deriving (Eq, Ord)

-- | Special characters including partial derivatives, degree circle, and
-- underscores
data Special = Partial
             | Circle
             | Percent
             | Hash
             | CurlyBrOpen
             | CurlyBrClose
             | SqBrOpen
             | SqBrClose
             | UScore
  deriving (Eq, Ord)

-- Class for rendering greek characters
{-class RenderGreek r where
  greek :: Greek -> r-}

-- | Class for rendering special characters
class RenderSpecial r where
  special :: Special -> r
