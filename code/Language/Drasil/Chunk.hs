-- | Base chunk module
module Language.Drasil.Chunk where

import Control.Lens (Simple,Lens)

import Prelude hiding (id)

-- | The base Chunk class
class Chunk c where
  -- | Chunk implements the id function which provides a 
  -- preferably /unique/ id for internal Drasil use
  id :: Simple Lens c String
