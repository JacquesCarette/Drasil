{-# OPTIONS -Wall #-} 
module RelationChunk where

import ASTInternal (Relation)
import Chunk
import Control.Lens (Simple, Lens)
import Spec (Spec(..))

-- BEGIN RelationChunk --
data RelationChunk = RC
  { cc :: ConceptChunk
  , relat :: Relation
  }

instance Chunk RelationChunk where
  name = cp . name

instance Concept RelationChunk where
  descr = cp . descr

-- END RelationChunk --

-- don't export this
cp :: Simple Lens RelationChunk ConceptChunk
cp f (RC a b) = fmap (\x -> RC x b) (f a)

makeRC :: String -> Spec -> Relation -> RelationChunk
makeRC nm desc rel = RC (CC nm desc) rel