{-# OPTIONS -Wall #-} 
module Language.Drasil.Chunk.Relation(RelationChunk(..),makeRC) where

import Control.Lens (Simple, Lens)

import Language.Drasil.Expr (Relation)
import Language.Drasil.Chunk
import Language.Drasil.Spec (Sentence(..))

data RelationChunk = RC
  { cc :: ConceptChunk
  , relat :: Relation
  }

instance Chunk RelationChunk where
  name = cp . name

instance Concept RelationChunk where
  descr = cp . descr


-- don't export this
cp :: Simple Lens RelationChunk ConceptChunk
cp f (RC a b) = fmap (\x -> RC x b) (f a)

makeRC :: String -> Sentence -> Relation -> RelationChunk
makeRC nm desc rel = RC (CC nm desc) rel
