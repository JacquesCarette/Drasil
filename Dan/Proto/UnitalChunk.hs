{-# OPTIONS -Wall #-} 
module UnitalChunk (UnitalChunk(..), unit) where

import Chunk (VarChunk, Chunk(..))
import Unit (Unit(..))
import Control.Lens (Simple, Lens, (^.))
import Symbol

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

unit :: Chunk c => c -> Symbol
unit c = c ^. symbol --This needs to be changed to usiu, but cyclical imports
                     -- will occur if we try to wrap it in a spec. May need to
                     -- return a type "Unit" and then get wrapped at the call point
                     --However, cyclical imports will still occur because of Unit's
                     -- use of Expr.
