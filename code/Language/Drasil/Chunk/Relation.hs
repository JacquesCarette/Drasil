{-# OPTIONS -Wall #-} 
module Language.Drasil.Chunk.Relation(RelationChunk(..),makeRC) where

import Control.Lens (Simple, Lens)
import Prelude hiding (id)
import Language.Drasil.Expr (Relation)
import Language.Drasil.Chunk
import Language.Drasil.Spec (Sentence(..))

data RelationChunk = RC
  { cc :: NamedChunk
  , relat :: Relation
  }

instance Chunk RelationChunk where
  id = cp . id

instance NamedIdea RelationChunk where
  term = cp . term


-- don't export this
cp :: Simple Lens RelationChunk NamedChunk
cp f (RC a b) = fmap (\x -> RC x b) (f a)

makeRC :: String -> Sentence -> Relation -> RelationChunk
makeRC nm desc rel = RC (ccWithDescrSent nm desc) rel
