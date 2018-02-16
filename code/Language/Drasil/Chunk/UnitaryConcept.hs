{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.UnitaryConcept (ucw, UnitaryConceptDict) where

import Prelude hiding (id)

import Language.Drasil.Chunk.Concept (DefnAndDomain(DAD), ConceptChunk, Concept,
  Definition(defn), ConceptDomain(cdom))
import Language.Drasil.Chunk.Unitary (UnitaryChunk, mkUnitary, Unitary)
import Language.Drasil.Chunk.Quantity (Quantity(typ, getSymb, getUnit, getStagedS))
import Language.Drasil.Chunk.NamedIdea (Idea(getA),NamedIdea(term))
import Language.Drasil.Chunk (Chunk(id))

import Control.Lens ((^.), makeLenses, view)

data UnitaryConceptDict = UCC {_unitary :: UnitaryChunk, _dad :: DefnAndDomain ConceptChunk}
makeLenses ''UnitaryConceptDict

instance Chunk UnitaryConceptDict where id = unitary . id
instance NamedIdea UnitaryConceptDict where term = unitary . term
instance Idea UnitaryConceptDict where getA u = getA (u ^. unitary)
instance Definition UnitaryConceptDict where defn = dad . defn
instance ConceptDomain UnitaryConceptDict where cdom = dad . cdom
instance Concept UnitaryConceptDict where
instance Quantity UnitaryConceptDict where
  typ = unitary . typ
  getSymb stage = getSymb stage . view unitary
  getUnit = getUnit . view unitary
  getStagedS = getStagedS . view unitary

instance Eq UnitaryConceptDict where
  a == b = (a ^. id) == (b ^. id)
instance Ord UnitaryConceptDict where
  compare a b = compare (a ^. id) (b ^. id)

ucw :: (Unitary c, Concept c) => c -> UnitaryConceptDict
ucw c = UCC (mkUnitary c) (DAD (c ^. defn) (c ^. cdom))
