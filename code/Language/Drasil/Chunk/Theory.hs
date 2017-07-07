{-# Language GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Theory 
  ( tc', Theory(..), TheoryChunk, TheoryModel
  )where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Quantity

import Control.Lens (Simple, Lens)
import Prelude hiding (id)

class Theory t where
  valid_context :: Simple Lens t [t]
  spaces :: Simple Lens t [SpaceDefn] 
  quantities :: Simple Lens t [QWrapper]
  operations :: Simple Lens t [CWrapper] -- FIXME: Should not be Concept
  defined_quant :: Simple Lens t [QDefinition]
  invariants :: Simple Lens t [Constraint]
  defined_fun :: Simple Lens t [QDefinition]
  
data SpaceDefn = SpaceDefn -- FIXME: This should be defined.
  
data TheoryChunk where
  TC :: String -> [TheoryChunk] -> [SpaceDefn] -> [QWrapper] -> [CWrapper] -> 
    [QDefinition] -> [Constraint] -> [QDefinition] -> TheoryChunk
    
instance Theory TheoryChunk where
  valid_context f (TC c m n o p q r s) = 
    fmap (\x -> TC c x n o p q r s) (f m)
  spaces f (TC c t s q o dq inv df) = 
    fmap (\x -> TC c t x q o dq inv df) (f s)
  quantities f (TC c t s q o dq inv df) = 
    fmap (\x -> TC c t s x o dq inv df) (f q)
  operations f (TC c t s q o dq inv df) = 
    fmap (\x -> TC c t s q x dq inv df) (f o)
  defined_quant f (TC c t s q o dq inv df) = 
    fmap (\x -> TC c t s q o x inv df) (f dq)
  invariants f (TC c t s q o dq inv df) = 
    fmap (\x -> TC c t s q o dq x df) (f inv)
  defined_fun f (TC c t s q o dq inv df) = 
    fmap (\x -> TC c t s q o dq inv x) (f df)
instance Chunk TheoryChunk where
  id f (TC c t s q o dq inv df) = 
    fmap (\x -> TC x t s q o dq inv df) (f c)

data TheoryModel where
  TM :: (Concept c, Theory t) => c -> t -> TheoryModel
  
tc :: (Quantity q, Concept c) => String -> [TheoryChunk] -> [SpaceDefn] -> [q] ->
  [c] -> [QDefinition] -> [Constraint] -> [QDefinition] -> TheoryChunk
tc cid t s q c = TC cid t s (map qw q) (map cw c)

tc' :: (Quantity q, Concept c) => String -> [q] -> [c] -> [QDefinition] -> 
  [Constraint] -> [QDefinition] -> TheoryChunk
tc' cid q c = tc cid [] [] (map qw q) (map cw c)
