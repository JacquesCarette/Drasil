{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.UnitaryConcept (ucw, UnitaryConceptDict) where

import Language.Drasil.Chunk.Concept (DefnAndDomain(DAD), ConceptChunk, Concept,
  Definition(defn), ConceptDomain(cdom))
import Language.Drasil.Chunk.Unitary (UnitaryChunk, mkUnitary, Unitary)
import Language.Drasil.Chunk.Quantity (Quantity(getUnit),HasSpace(typ))
import Language.Drasil.Chunk.NamedIdea (Idea(getA),NamedIdea(term))
import Language.Drasil.Chunk.SymbolForm (HasSymbol(symbol))
import Language.Drasil.Chunk (Chunk(uid))

import Control.Lens ((^.), makeLenses, view)

data UnitaryConceptDict = UCC {_unitary :: UnitaryChunk, _dad :: DefnAndDomain ConceptChunk}
makeLenses ''UnitaryConceptDict

instance Chunk UnitaryConceptDict where uid = unitary . uid
instance NamedIdea UnitaryConceptDict where term = unitary . term
instance Idea UnitaryConceptDict where getA u = getA (u ^. unitary)
instance Definition UnitaryConceptDict where defn = dad . defn
instance ConceptDomain UnitaryConceptDict where cdom = dad . cdom
instance Concept UnitaryConceptDict where
instance HasSpace UnitaryConceptDict where typ = unitary . typ
instance HasSymbol UnitaryConceptDict where symbol stage = symbol stage . view unitary
instance Quantity UnitaryConceptDict where getUnit = getUnit . view unitary

instance Eq UnitaryConceptDict where a == b = (a ^. uid) == (b ^. uid)

ucw :: (Unitary c, Concept c) => c -> UnitaryConceptDict
ucw c = UCC (mkUnitary c) (DAD (c ^. defn) (c ^. cdom))
