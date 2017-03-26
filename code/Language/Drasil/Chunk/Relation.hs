{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Relation
  ( NamedRelation, RelationConcept
  , makeNR, relat
  , namewrap, nrelat, makeRC
  ) where

import Control.Lens (Simple, Lens, (^.), set)
import Prelude hiding (id)
import Language.Drasil.Expr (Relation)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.Wrapper

data NamedRelation where 
  NR :: NamedIdea c => c -> Relation -> NamedRelation

namewrap :: NamedRelation -> NWrapper
namewrap (NR c _) = nw c

nrelat :: NamedRelation -> Relation
nrelat (NR _ r) = r

instance Chunk NamedRelation where
  id = cp id

instance NamedIdea NamedRelation where
  term = cp term
  getA (NR c _) = getA c
  
data RelationConcept where 
  RC :: Concept c => c -> Relation -> RelationConcept

relat :: RelationConcept -> Relation
relat (RC _ r) = r

instance Chunk RelationConcept where
  id = rcl id

instance NamedIdea RelationConcept where
  term = rcl term
  getA (RC c _) = getA c
  
instance Concept RelationConcept where
  defn = rcl defn
  cdom = rcl cdom

-- don't export this
rcl:: (forall c. (Concept c) => Simple Lens c a) -> Simple Lens RelationConcept a
rcl l f (RC a b) = fmap (\x -> RC (set l x a) b) (f (a ^. l))

makeRC :: String -> String -> Sentence -> Relation -> RelationConcept
makeRC rID rTerm rDefn rel = RC (dccWDS rID rTerm rDefn) rel

-- don't export this
cp :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens NamedRelation a
cp l f (NR a b) = fmap (\x -> NR (set l x a) b) (f (a ^. l))

makeNR :: String -> Sentence -> Relation -> NamedRelation
makeNR nm desc rel = NR (ncWDS nm desc) rel
