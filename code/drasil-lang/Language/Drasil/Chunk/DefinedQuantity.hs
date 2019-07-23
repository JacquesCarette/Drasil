{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqd, dqdNoUnit, dqd',
  dqdQd, dqdWr) where

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), Concept,
  Definition(defn), ConceptDomain(cdom), HasSpace(typ), IsUnit, Quantity)
import Language.Drasil.Chunk.Concept (ConceptChunk, cw)

import Language.Drasil.Chunk.UnitDefn (UnitDefn, unitWrapper,
  MayHaveUnit(getUnit))
import Language.Drasil.Space (Space)
import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)

import Control.Lens ((^.), makeLenses, view)

-- | DefinedQuantity = Concept + Quantity
data DefinedQuantityDict = DQD { _con :: ConceptChunk
                               , _symb :: Stage -> Symbol
                               , _spa :: Space
                               , _unit' :: Maybe UnitDefn
                               }
  
makeLenses ''DefinedQuantityDict

instance HasUID        DefinedQuantityDict where uid = con . uid
instance Eq            DefinedQuantityDict where a == b = (a ^. uid) == (b ^. uid)
instance NamedIdea     DefinedQuantityDict where term = con . term
instance Idea          DefinedQuantityDict where getA = getA . view con
instance Definition    DefinedQuantityDict where defn = con . defn
instance ConceptDomain DefinedQuantityDict where cdom = cdom . view con
instance HasSpace      DefinedQuantityDict where typ = spa
instance HasSymbol     DefinedQuantityDict where symbol = view symb
instance Quantity      DefinedQuantityDict where 
instance MayHaveUnit   DefinedQuantityDict where getUnit = view unit'

-- For when the symbol is constant through stages
dqd :: (IsUnit u) => ConceptChunk -> Symbol -> Space -> u -> DefinedQuantityDict
dqd c s sp = DQD c (const s) sp . Just . unitWrapper

dqdNoUnit :: ConceptChunk -> Symbol -> Space -> DefinedQuantityDict
dqdNoUnit c s sp = DQD c (const s) sp Nothing

-- For when the symbol changes depending on the stage
dqd' :: ConceptChunk -> (Stage -> Symbol) -> Space -> Maybe UnitDefn -> DefinedQuantityDict
dqd' = DQD

-- When the input already has all the necessary information. A 'projection' operator
dqdWr :: (Quantity c, Concept c, MayHaveUnit c) => c -> DefinedQuantityDict
dqdWr c = DQD (cw c) (symbol c) (c ^. typ) (getUnit c)

-- When we want to merge a quantity and a concept. This is suspicious.
dqdQd :: (Quantity c, MayHaveUnit c) => c -> ConceptChunk -> DefinedQuantityDict
dqdQd c cc = DQD cc (symbol c) (c ^. typ) (getUnit c)
