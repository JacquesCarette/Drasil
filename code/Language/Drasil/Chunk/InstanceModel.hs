{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.InstanceModel 
  ( InstanceModel
  , inCons, outCons, modelOutputs, modelInputs, im, imQD
  )where

import Language.Drasil.Spec
import Language.Drasil.Expr
import Language.Drasil.Expr.Extract
import Language.Drasil.ChunkDB

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.ExprRelat
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Relation

import Control.Lens (makeLenses,(^.))

type Inputs = [QuantityDict]
type Outputs = [QuantityDict]

type InputConstraints  = [TheoryConstraint]
type OutputConstraints = [TheoryConstraint]

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints. It also has attributes (like Derivation, source, etc.)
data InstanceModel = IM { _rc :: RelationConcept
                        , _modelInputs :: Inputs
                        , _inCons :: InputConstraints
                        , _modelOutputs :: Outputs
                        , _outCons :: OutputConstraints
                        , _attribs :: Attributes 
                        }
makeLenses ''InstanceModel
  
instance Chunk InstanceModel where uid = rc . uid
instance NamedIdea InstanceModel where term = rc . term
instance Idea InstanceModel where getA (IM a _ _ _ _ _) = getA a
instance Definition InstanceModel where defn = rc . defn
instance ConceptDomain InstanceModel where cdom = rc . cdom
instance Concept InstanceModel where
instance ExprRelat InstanceModel where relat = rc . relat
instance HasAttributes InstanceModel where attributes = attribs

-- | Smart constructor for instance models
im :: RelationConcept -> Inputs -> InputConstraints -> Outputs -> 
  OutputConstraints -> Attributes -> InstanceModel
im = IM

-- | Smart constructor for instance model from qdefinition 
-- (Sentence is the "concept" definition for the relation concept)
imQD :: HasSymbolTable ctx => ctx -> QDefinition -> Sentence -> InputConstraints 
  -> OutputConstraints -> Attributes -> InstanceModel
imQD ctx qd dfn incon ocon att = IM (makeRC (qd ^. uid) (qd ^. term) dfn 
  (C qd $= qd ^. equat)) (vars (qd^.equat) ctx) incon [qw qd] ocon att
