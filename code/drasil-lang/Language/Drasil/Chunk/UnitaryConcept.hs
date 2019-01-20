{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.UnitaryConcept (ucw, UnitaryConceptDict) where

import Control.Lens ((^.), makeLenses)

import Language.Drasil.Chunk.Unitary (UnitaryChunk, mkUnitary, Unitary(unit))
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), Quantity, Concept,
  Definition(defn), ConceptDomain(cdom), HasSpace(typ))
import Language.Drasil.Development.Unit (MayHaveUnit(getUnit))
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)

data UnitaryConceptDict = UCC { _unitary :: UnitaryChunk
                              , _defn' :: Sentence
                              , cdom' :: [UID]
                              }
makeLenses ''UnitaryConceptDict

instance HasUID        UnitaryConceptDict where uid = unitary . uid
instance NamedIdea     UnitaryConceptDict where term = unitary . term
instance Idea          UnitaryConceptDict where getA u = getA (u ^. unitary)
instance Definition    UnitaryConceptDict where defn = defn'
instance ConceptDomain UnitaryConceptDict where cdom = cdom'
instance HasSpace      UnitaryConceptDict where typ = unitary . typ
instance HasSymbol     UnitaryConceptDict where symbol c stage = symbol (c^.unitary) stage
instance Quantity      UnitaryConceptDict where 
instance Eq            UnitaryConceptDict where a == b = (a ^. uid) == (b ^. uid)
instance MayHaveUnit   UnitaryConceptDict where getUnit u = getUnit $ u ^. unitary
instance Unitary       UnitaryConceptDict where unit x = unit (x ^. unitary)

ucw :: (Unitary c, Concept c, MayHaveUnit c) => c -> UnitaryConceptDict
ucw c = UCC (mkUnitary c) (c ^. defn) (cdom c)
