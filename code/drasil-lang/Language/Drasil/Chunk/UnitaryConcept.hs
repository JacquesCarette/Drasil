{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.UnitaryConcept (ucw, UnitaryConceptDict) where

import Control.Lens ((^.), makeLenses)

import Language.Drasil.Chunk.Concept (DefnAndDomain(DAD))
import Language.Drasil.Chunk.Unitary (UnitaryChunk, mkUnitary, Unitary)
import Language.Drasil.Chunk.Quantity (Quantity, HasSpace(typ))
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, HasSymbol(symbol))
import Language.Drasil.Development.Unit (MayHaveUnit(getUnit))

data UnitaryConceptDict = UCC { _unitary :: UnitaryChunk
                              , _dad :: DefnAndDomain
                              }
makeLenses ''UnitaryConceptDict

instance HasUID        UnitaryConceptDict where uid = unitary . uid
instance NamedIdea     UnitaryConceptDict where term = unitary . term
instance Idea          UnitaryConceptDict where getA u = getA (u ^. unitary)
instance Definition    UnitaryConceptDict where defn = dad . defn
instance ConceptDomain UnitaryConceptDict where cdom = dad . cdom
instance Concept       UnitaryConceptDict where
instance HasSpace      UnitaryConceptDict where typ = unitary . typ
instance HasSymbol     UnitaryConceptDict where symbol c stage = symbol (c^.unitary) stage
instance Quantity      UnitaryConceptDict where 
instance Eq            UnitaryConceptDict where a == b = (a ^. uid) == (b ^. uid)
instance MayHaveUnit   UnitaryConceptDict where getUnit u = getUnit $ u ^. unitary

ucw :: (Unitary c, Concept c) => c -> UnitaryConceptDict
ucw c = UCC (mkUnitary c) (DAD (c ^. defn) (c ^. cdom))
