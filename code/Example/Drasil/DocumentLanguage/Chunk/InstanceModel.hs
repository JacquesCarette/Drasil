{-# Language GADTs,Rank2Types #-}
module Drasil.DocumentLanguage.Chunk.InstanceModel where

import Language.Drasil

import Control.Lens (Simple, Lens, (^.), set)

import Prelude hiding (id)

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints.
data InstanceModel where
  IM ::  RelationConcept -> InputConstraints -> OutputConstraints -> InstanceModel
  
instance Chunk InstanceModel where
  id = rcl id
instance NamedIdea InstanceModel where
  term = rcl term
  getA (IM a _ _) = getA a
instance Concept InstanceModel where
  defn = rcl defn
  cdom = rcl cdom
instance ExprRelat InstanceModel where
  relat = rcl relat
instance HasAttributes InstanceModel where
  attributes f (IM a b c) = fmap (\x -> IM a b x) (f c)

type InputConstraints  = [Constraint]
type OutputConstraints = [Constraint]
  
rcl :: Simple Lens RelationConcept a -> Simple Lens InstanceModel a
rcl l f (IM a b c) = fmap (\x -> IM (set l x a) b c) (f (a ^. l))

