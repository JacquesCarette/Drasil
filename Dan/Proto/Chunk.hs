{-# OPTIONS -Wall #-} 
module Chunk where

import Control.Lens
import Symbol

-------- BEGIN CLASSES --------

-- BEGIN CHUNK --
-- a chunk has a name
class Chunk c where
   name :: Simple Lens c String
-- END CHUNK --
   
-- BEGIN CONCEPT --
-- a concept has a description
class Chunk c => Concept c where
   descr :: Simple Lens c String
-- END CONCEPT --

-- BEGIN QUANTITY --
-- a quantity is a concept that can be represented graphically
class Concept c => Quantity c where
   symbol :: Simple Lens c Symbol
-- END QUANTITY --

-------- BEGIN DATATYPES/INSTANCES --------

-- BEGIN CONCEPTCHUNK --
data ConceptChunk = CC String String
instance Chunk ConceptChunk where
  name f (CC a b) = fmap (\x -> CC x b) (f a)
instance Concept ConceptChunk where
  descr f (CC a b) = fmap (\x -> CC a x) (f b)
-- END CONCEPTCHUNK --

-- BEGIN VARCHUNK --
data VarChunk = VC { vname :: String
                   , vdesc :: String
                   , vsymb :: Symbol}

instance Eq VarChunk where
  c1 == c2 = (c1 ^. name) == (c2 ^. name)

instance Chunk VarChunk where
  name f (VC n d s) = fmap (\x -> VC x d s) (f n)

instance Concept VarChunk where
  descr f (VC n d s) = fmap (\x -> VC n x s) (f d)

instance Quantity VarChunk where
  symbol f (VC n d s) = fmap (\x -> VC n d x) (f s)

-- END VARCHUNK --
