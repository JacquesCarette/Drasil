{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.InstanceModel 
  ( InstanceModel
  , inCons, outCons, imOutput, imInputs, im, imQD
  )where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn),ConceptDomain(cdom,DOM), Concept, HasAttributes(attributes),
  ExprRelat(relat), HasDerivation(derivations), 
  HasReference(getReferences))
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Chunk.Attribute.References (References)
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Constrained.Core (TheoryConstraint)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Relation
import Language.Drasil.Chunk.Quantity
import Language.Drasil.ChunkDB
import Language.Drasil.Expr
import Language.Drasil.Expr.Math (sy)
import Language.Drasil.Expr.Extract (vars)
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Chunk.Attribute.Derivation

import Control.Lens (makeLenses, (^.))

type Inputs = [QuantityDict]
type Output = QuantityDict

type InputConstraints  = [TheoryConstraint]
type OutputConstraints = [TheoryConstraint]

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints. It also has attributes (like Derivation, source, etc.)
data InstanceModel = IM { _rc :: RelationConcept
                        , _imInputs :: Inputs
                        , _inCons :: InputConstraints
                        , _imOutput :: Output
                        , _outCons :: OutputConstraints
                        , _ref :: References
                        , _attribs :: Attributes 
 			, _deri :: Derivation
                        }
makeLenses ''InstanceModel
  
instance HasUID        InstanceModel where uid = rc . uid
instance NamedIdea     InstanceModel where term = rc . term
instance Idea          InstanceModel where getA (IM a _ _ _ _ _ _ _) = getA a
instance Concept       InstanceModel where
instance Definition    InstanceModel where defn = rc . defn
instance ConceptDomain InstanceModel where
  type DOM InstanceModel = ConceptChunk
  cdom = rc . cdom
instance ExprRelat     InstanceModel where relat = rc . relat
instance HasAttributes InstanceModel where attributes = attribs
instance HasDerivation InstanceModel where derivations = deri
-- error used below is on purpose. These shortnames should be made explicit as necessary
instance HasReference  InstanceModel where getReferences = ref
instance HasShortName  InstanceModel where
  shortname _ = error "No explicit name given for instance model -- build a custom Ref"

-- | Smart constructor for instance models
im :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> Attributes -> InstanceModel
im rc i ic o oc atts = IM rc i ic o oc [] atts []

-- | Smart constructor for instance model from qdefinition 
-- (Sentence is the "concept" definition for the relation concept)
imQD :: HasSymbolTable ctx => ctx -> QDefinition -> Sentence -> InputConstraints -> OutputConstraints -> Attributes -> InstanceModel
imQD ctx qd dfn incon ocon atts = IM (makeRC (qd ^. uid) (qd ^. term) dfn 
  (sy qd $= qd ^. equat) atts) (vars (qd^.equat) ctx) incon (qw qd) ocon [] atts []--FIXME: atts used twice?
