{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.ConVar (DefinedQuantityDictCV(DQD), dqd) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn),ConceptDomain(cdom,DOM),Concept,HasSymbol(symbol), HasSpace(typ))
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Quantity (Quantity(getUnit))
import Language.Drasil.Symbol (Symbol,Stage)
import Language.Drasil.Space (Space)

import Control.Lens ((^.),makeLenses)

-- | ConVar is a 'Concept' as well as a 'Quantity'. 
-- It adds a 'Space' and 'Symbol' to an existing 'ConceptChunk'.
-- FIXME: ConVar renamed to DefinedQuantityDictCV
data DefinedQuantityDictCV = DQD { _con :: ConceptChunk
                                 , _symb :: Stage -> Symbol
                                 , _spa :: Space 
                                 }
makeLenses ''DefinedQuantityDictCV
                     
instance Eq            DefinedQuantityDictCV where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        DefinedQuantityDictCV where uid = con . uid
instance NamedIdea     DefinedQuantityDictCV where term = con . term
instance Idea          DefinedQuantityDictCV where getA (DQD c _ _) = getA c
instance Definition    DefinedQuantityDictCV where defn = con . defn
instance ConceptDomain DefinedQuantityDictCV where
  type DOM DefinedQuantityDictCV = ConceptChunk
  cdom = con . cdom
instance HasSymbol     DefinedQuantityDictCV where symbol c = (c ^. symb)
instance HasSpace      DefinedQuantityDictCV where typ = spa
instance Concept       DefinedQuantityDictCV where
instance Quantity      DefinedQuantityDictCV where getUnit _ = Nothing

-- | Constructor for 'DefinedQuantityDictCV' with explicit 'Space'
dqd :: ConceptChunk -> Symbol -> Space -> DefinedQuantityDictCV
dqd c s = DQD c (\_ -> s)