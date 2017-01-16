{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Relation(NamedRelation(..),makeNR, relat, cc) where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Expr (Relation)
import Language.Drasil.Chunk
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.Wrapper

data NamedRelation where 
  NR :: NamedIdea c => c -> Relation -> NamedRelation

cc :: NamedRelation -> NWrapper
cc (NR c _) = nw c

relat :: NamedRelation -> Relation
relat (NR _ r) = r

instance Chunk NamedRelation where
  id = cp id

instance NamedIdea NamedRelation where
  term = cp term


-- don't export this
cp :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens NamedRelation a
cp l f (NR a b) = fmap (\x -> NR (set l x a) b) (f (a ^. l))

makeNR :: String -> Sentence -> Relation -> NamedRelation
makeNR nm desc rel = NR (ncWDS nm desc) rel
