{-# Language GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Theory 
  ( tc', Theory(..), TheoryChunk, TheoryModel, tm, tw
  )where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Quantity

import Control.Lens (Simple, Lens, set, (^.))
import Prelude hiding (id)

class Chunk t => Theory t where
  valid_context :: Simple Lens t [TheoryChunk]
  spaces :: Simple Lens t [SpaceDefn] 
  quantities :: Simple Lens t [QuantityDict]
  operations :: Simple Lens t [ConceptChunk] -- FIXME: Should not be Concept
  defined_quant :: Simple Lens t [QDefinition]
  invariants :: Simple Lens t [TheoryConstraint]
  defined_fun :: Simple Lens t [QDefinition]
  
data SpaceDefn -- FIXME: This should be defined.
  
data TheoryChunk where
  TC :: String -> [TheoryChunk] -> [SpaceDefn] -> [QuantityDict] -> [ConceptChunk] -> 
    [QDefinition] -> [TheoryConstraint] -> [QDefinition] -> TheoryChunk
    
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

tw :: Theory t => t -> TheoryChunk
tw t = TC (t ^. id) (t ^. valid_context) (t ^. spaces) (t ^. quantities)
  (t ^. operations) (t ^. defined_quant) (t ^. invariants) (t ^. defined_fun)

data TheoryModel where
  TM :: (Concept c, Theory t) => c -> t -> TheoryModel
  
instance Chunk TheoryModel where
  id = cl id
instance NamedIdea TheoryModel where
  term = cl term
instance Idea TheoryModel where
  getA (TM c _) = getA c
instance Definition TheoryModel where
  defn = cl defn
instance ConceptDomain TheoryModel where
  cdom = cl cdom
instance Concept TheoryModel where
instance Theory TheoryModel where
  valid_context = tl valid_context
  spaces        = tl spaces
  quantities    = tl quantities
  operations    = tl operations
  defined_quant = tl defined_quant
  invariants    = tl invariants
  defined_fun   = tl defined_fun
  
  
cl :: (forall c. (Concept c) => Simple Lens c a) -> Simple Lens TheoryModel a
cl l f (TM c t) = fmap (\x -> TM (set l x c) t) (f (c ^. l))

tl :: (forall t. (Theory t) => Simple Lens t a) -> Simple Lens TheoryModel a
tl l f (TM c t) = fmap (\x -> TM c (set l x t)) (f (t ^. l))
  
tc :: (Theory t, Quantity q, Concept c) => String -> [t] -> 
  [SpaceDefn] -> [q] -> [c] -> [QDefinition] -> [TheoryConstraint] -> 
  [QDefinition] -> TheoryChunk
tc cid t s q c = TC cid (map tw t) s (map qw q) (map cw c)

tc' :: (Quantity q, Concept c) => String -> [q] -> [c] -> [QDefinition] -> 
  [TheoryConstraint] -> [QDefinition] -> TheoryChunk
tc' cid q c = tc cid ([] :: [TheoryChunk]) [] q c

tm :: (Concept c, Theory t) => c -> t -> TheoryModel
tm = TM
