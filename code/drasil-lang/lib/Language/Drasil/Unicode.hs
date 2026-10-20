{-# LANGUAGE TemplateHaskell #-}
-- | Special type for unicode characters.
module Language.Drasil.Unicode(Special(Circle), RenderSpecial(special)) where

import Drasil.Database (declareHasChunkRefs, Generically(..))

-- | Special characters include partial derivatives and the degree circle.
data Special = Circle  -- remove by refactoring how units are done
  deriving (Eq, Ord)

declareHasChunkRefs ''Special

-- | Class for rendering special characters.
class RenderSpecial r where
  special :: Special -> r
