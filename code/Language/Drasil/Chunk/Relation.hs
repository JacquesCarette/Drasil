{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Relation(RelationChunk(..),makeRC, relat, cc) where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Expr (Relation)
import Language.Drasil.Chunk
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.Wrapper

data RelationChunk where 
  RC :: NamedIdea c => c -> Relation -> RelationChunk

cc :: RelationChunk -> NWrapper
cc (RC c _) = nw c

relat :: RelationChunk -> Relation
relat (RC _ r) = r

instance Chunk RelationChunk where
  id = cp id

instance NamedIdea RelationChunk where
  term = cp term


-- don't export this
cp :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens RelationChunk a
cp l f (RC a b) = fmap (\x -> RC (set l x a) b) (f (a ^. l))

makeRC :: String -> Sentence -> Relation -> RelationChunk
makeRC nm desc rel = RC (ncWDS nm desc) rel
