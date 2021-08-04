{-# Language TemplateHaskell #-}
-- | Adds concepts to a quantitative idea with units.
module Language.Drasil.Chunk.UnitaryConcept (
  -- * Chunk Type
  UnitaryConceptDict,
  -- * Constructor
  ucw) where

import Control.Lens ((^.), makeLenses)

import Language.Drasil.Chunk.Unitary (UnitaryChunk, mkUnitary, Unitary(unit))
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), Quantity, Concept,
  Definition(defn), ConceptDomain(cdom), HasSpace(typ))
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit))
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)

-- | Contains a 'UnitaryChunk', a definition, and a list of related 'UID's.
--
-- Ex. A pendulum arm is an idea associated with a symbol (l) and units (cm, m, etc.).
data UnitaryConceptDict = UCC { _unitary :: UnitaryChunk
                              , _defn' :: Sentence
                              , cdom' :: [UID]
                              }
makeLenses ''UnitaryConceptDict

-- | Finds 'UID' of the 'UnitaryChunk' used to make the 'UnitaryConceptDict'.
instance HasUID        UnitaryConceptDict where uid = unitary . uid
-- | Finds term ('NP') of the 'UnitaryChunk' used to make the 'UnitaryConceptDict'.
instance NamedIdea     UnitaryConceptDict where term = unitary . term
-- | Finds the idea contained in the 'UnitaryChunk' used to make the 'UnitaryConceptDict'.
instance Idea          UnitaryConceptDict where getA u = getA (u ^. unitary)
-- | Finds definition of the 'UnitaryConceptDict'.
instance Definition    UnitaryConceptDict where defn = defn'
-- | Finds the domain of the 'UnitaryConceptDict'.
instance ConceptDomain UnitaryConceptDict where cdom = cdom'
-- | Finds the 'Space' of the 'UnitaryChunk' used to make the 'UnitaryConceptDict'.
instance HasSpace      UnitaryConceptDict where typ = unitary . typ
-- | Finds the 'Symbol' of the 'UnitaryChunk' used to make the 'UnitaryConceptDict'.
instance HasSymbol     UnitaryConceptDict where symbol c = symbol (c^.unitary)
-- | 'UnitaryConceptDict's have a 'Quantity'. 
instance Quantity      UnitaryConceptDict where
-- | Equal if 'UID's are equal.
instance Eq            UnitaryConceptDict where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the units of the 'UnitaryChunk' used to make the 'UnitaryConceptDict'.
instance MayHaveUnit   UnitaryConceptDict where getUnit u = getUnit $ u ^. unitary
-- | Finds the quantity of the 'UnitaryChunk' used to make the 'UnitaryConceptDict'.
instance Unitary       UnitaryConceptDict where unit x = unit (x ^. unitary)


-- | Constructs a UnitaryConceptDict from a 'Concept' with 'Units'.
ucw :: (Unitary c, Concept c, MayHaveUnit c) => c -> UnitaryConceptDict
ucw c = UCC (mkUnitary c) (c ^. defn) (cdom c)
