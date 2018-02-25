-- | Base chunk module
module Language.Drasil.Chunk where

import Control.Lens (Lens')

-- | The base Chunk class
class Chunk c where
  -- | Chunk implements the id function which provides a 
  -- preferably /unique/ id for internal Drasil use
  uid :: Lens' c String
