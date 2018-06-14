{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.InstanceModel 
  ( InstanceModel
  , inCons, outCons, imOutput, imInputs, im, imQD
  )where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn),ConceptDomain(cdom), Concept, ExprRelat(relat),
  HasDerivation(derivations), HasReference(getReferences))
import Language.Drasil.Chunk.References (References)
import Language.Drasil.Chunk.Derivation (Derivation)
import Language.Drasil.Chunk.ShortName (ShortName, HasShortName(shortname), shortname')
import Language.Drasil.Chunk.Derivation
import Language.Drasil.Chunk.ShortName
import Language.Drasil.Chunk.Constrained.Core (TheoryConstraint)
import Language.Drasil.Chunk.Eq (QDefinition, equat)
import Language.Drasil.Chunk.Relation (RelationConcept, makeRC)
import Language.Drasil.Chunk.Quantity (QuantityDict, qw)
import Language.Drasil.ChunkDB (HasSymbolTable)
import Language.Drasil.Expr (($=))
import Language.Drasil.Expr.Math (sy)
import Language.Drasil.Expr.Extract (vars)
import Language.Drasil.Spec (Sentence)

import Control.Lens (makeLenses, (^.), view)

type Inputs = [QuantityDict]
type Output = QuantityDict

type InputConstraints  = [TheoryConstraint]
type OutputConstraints = [TheoryConstraint]

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints. It also has attributes like derivation, source, etc.
data InstanceModel = IM { _rc :: RelationConcept
                        , _imInputs :: Inputs
                        , _inCons :: InputConstraints
                        , _imOutput :: Output
                        , _outCons :: OutputConstraints
                        , _ref :: References
                        , _deri :: Derivation
                        , _refName :: ShortName
                        }
makeLenses ''InstanceModel
  
instance HasUID        InstanceModel where uid = rc . uid
instance NamedIdea     InstanceModel where term = rc . term
instance Idea          InstanceModel where getA (IM a _ _ _ _ _ _ _) = getA a
instance Concept       InstanceModel where
instance Definition    InstanceModel where defn = rc . defn
instance ConceptDomain InstanceModel where cdom = rc . cdom
instance ExprRelat     InstanceModel where relat = rc . relat
instance HasDerivation InstanceModel where derivations = deri
instance HasReference  InstanceModel where getReferences = ref
instance HasShortName  InstanceModel where shortname = view refName

-- | Smart constructor for instance models
im :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> String -> InstanceModel
im rc i ic o oc sn = IM rc i ic o oc [] [] (shortname' sn)

-- | Smart constructor for instance model from qdefinition 
-- (Sentence is the "concept" definition for the relation concept)
-- FIXME: get the shortname from the QDefinition?
imQD :: HasSymbolTable ctx => ctx -> QDefinition -> Sentence -> InputConstraints -> OutputConstraints -> 
  String -> InstanceModel
imQD ctx qd dfn incon ocon sn = IM (makeRC (qd ^. uid) (qd ^. term) dfn 
  (sy qd $= qd ^. equat)) (vars (qd^.equat) ctx) incon (qw qd) ocon [] [] (shortname' sn)
