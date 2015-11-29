{-# OPTIONS -Wall #-} 
module UnitalChunk (UnitalChunk(..), unit) where

import Chunk (VarChunk, Chunk(..))
import Spec (Spec)
import Unit (Unit(..))
import Control.Lens (Simple, Lens, (^.))

data UnitalChunk = UC { ch :: VarChunk
                      , usiu :: Unit }

-- don't export this
vc :: Simple Lens UnitalChunk VarChunk
vc f (UC a b) = fmap (\x -> UC x b) (f a)

instance Chunk UnitalChunk where
  name = vc . name
  descr = vc . descr
  symbol = vc . symbol

------
-- useful routines

unit :: Chunk c => c -> Spec
unit c = c ^. symbol
