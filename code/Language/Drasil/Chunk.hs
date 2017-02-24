{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk where

import Control.Lens (Simple,Lens,(^.), set)

import Prelude hiding (id)

class Chunk c where
  id :: Simple Lens c String
