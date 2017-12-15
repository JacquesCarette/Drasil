{-# Language GADTs,Rank2Types #-}
module Drasil.DocumentLanguage.Chunk.InstanceModel 
  ( InstanceModel
  , inCons, outCons, outputs, inputs, im
  )where

import Language.Drasil

import Control.Lens (Simple, Lens, (^.), set)

import Prelude hiding (id)

type Inputs = [QWrapper]
type Outputs = [QWrapper]

type InputConstraints  = [Constraint]
type OutputConstraints = [Constraint]

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints. It also has attributes (like Derivation, source, etc.)
data InstanceModel where
  IM :: RelationConcept -> Inputs -> InputConstraints ->
    Outputs -> OutputConstraints -> Attributes -> InstanceModel
  
instance Chunk InstanceModel where
  id = rcl id
instance NamedIdea InstanceModel where
  term = rcl term
  getA (IM a _ _ _ _ _) = getA a
instance Concept InstanceModel where
  defn = rcl defn
  cdom = rcl cdom
instance ExprRelat InstanceModel where
  relat = rcl relat
instance HasAttributes InstanceModel where
  attributes f (IM rc ins inc outs outc attribs) = fmap (\x -> IM rc ins inc outs outc x) (f attribs)

inputs :: InstanceModel -> Inputs
inputs (IM _ ins _ _ _ _) = ins
  
outputs :: InstanceModel -> Outputs
outputs (IM _ _ _ outs _ _) = outs

-- | Function for retrieving the input constraints (if any) of an instance model
inCons :: InstanceModel -> InputConstraints
inCons (IM _ _ ics _ _ _) = ics

-- | Function for retrieving the output constraints (if any) of an instance model
outCons :: InstanceModel -> OutputConstraints
outCons (IM _ _ _ _ ocs _) = ocs

-- | Smart constructor for instance models
im :: RelationConcept -> Inputs -> InputConstraints -> Outputs -> 
  OutputConstraints -> Attributes -> InstanceModel
im = IM

-- DO NOT EXPORT BELOW THIS LINE --
  
rcl :: Simple Lens RelationConcept a -> Simple Lens InstanceModel a
rcl l f (IM rc ins inc outs outc attribs) = fmap (\x -> IM (set l x rc) ins inc outs outc attribs) (f (rc ^. l))
