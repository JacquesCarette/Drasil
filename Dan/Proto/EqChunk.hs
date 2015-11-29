{-# OPTIONS -Wall #-} 
module EqChunk(EqChunk(..)) where

import ASTInternal (Expr)
import Chunk
import UnitalChunk
import Control.Lens (Simple, Lens)

data EqChunk = EC 
  { uc :: UnitalChunk
  , equat :: Expr
  }

-- don't export this
ul :: Simple Lens EqChunk UnitalChunk
ul f (EC a b) = fmap (\x -> EC x b) (f a)

-- this works because UnitalChunk is a Chunk
instance Chunk EqChunk where
  name = ul . name
  descr = ul . descr
  symbol = ul . symbol

