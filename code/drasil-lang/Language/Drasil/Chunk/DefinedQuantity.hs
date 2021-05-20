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

-- | DefinedQuantityDict is the combination of a 'Concept' and a 'Quantity'. Contains a 'ConceptChunk', a 'Symbol' dependent on 'Stage', a 'Space', and maybe a 'UnitDefn'.
data DefinedQuantityDict = DQD { _con :: ConceptChunk
                               , _symb :: Stage -> Symbol
                               , _spa :: Space
                               , _unit' :: Maybe UnitDefn
                               }
  
makeLenses ''DefinedQuantityDict

instance HasUID        DefinedQuantityDict where uid = con . uid
-- ^ Finds the 'UID' of the 'ConceptChunk' used to make the 'DefinedQuantityDict'.
instance Eq            DefinedQuantityDict where a == b = (a ^. uid) == (b ^. uid)
-- ^ Equal if 'UID's are equal.
instance NamedIdea     DefinedQuantityDict where term = con . term
-- ^ Finds the term ('NP') of the 'ConceptChunk' used to make the 'DefinedQuantityDict'.
instance Idea          DefinedQuantityDict where getA = getA . view con
-- ^ Finds the idea contained in the 'ConceptChunk' used to make the 'DefinedQuantityDict'.
instance Definition    DefinedQuantityDict where defn = con . defn
-- ^ Finds the definition contained in the 'ConceptChunk' used to make the 'DefinedQuantityDict'.
instance ConceptDomain DefinedQuantityDict where cdom = cdom . view con
-- ^ Finds the domain of the 'ConceptChunk' used to make the 'DefinedQuantityDict'.
instance HasSpace      DefinedQuantityDict where typ = spa
-- ^ Finds the 'Space' of the 'DefinedQuantityDict'.
instance HasSymbol     DefinedQuantityDict where symbol = view symb
-- ^ Finds the 'Stage' -> 'Symbol' of the 'DefinedQuantityDict'.
instance Quantity      DefinedQuantityDict where 
instance MayHaveUnit   DefinedQuantityDict where getUnit = view unit'
-- ^ Finds the units of the 'DefinedQuantityDict'.

-- | Smart constructor that creates a DefinedQuantityDict with a 'ConceptChunk', a 'Symbol' independent of 'Stage', a 'Space', and a unit.
dqd :: (IsUnit u) => ConceptChunk -> Symbol -> Space -> u -> DefinedQuantityDict
dqd c s sp = DQD c (const s) sp . Just . unitWrapper

-- | Similar to 'dqd', but without any units.
dqdNoUnit :: ConceptChunk -> Symbol -> Space -> DefinedQuantityDict
dqdNoUnit c s sp = DQD c (const s) sp Nothing

-- | Similar to 'dqd', but the 'Symbol' is now dependent on the 'Stage'.
dqd' :: ConceptChunk -> (Stage -> Symbol) -> Space -> Maybe UnitDefn -> DefinedQuantityDict
dqd' = DQD

-- | When the input already has all the necessary information. A 'projection' operator from some a type with instances of listed classes to a 'DefinedQuantityDict'.
dqdWr :: (Quantity c, Concept c, MayHaveUnit c) => c -> DefinedQuantityDict
dqdWr c = DQD (cw c) (symbol c) (c ^. typ) (getUnit c)

-- | When we want to merge a quantity and a concept. This is suspicious.
dqdQd :: (Quantity c, MayHaveUnit c) => c -> ConceptChunk -> DefinedQuantityDict
dqdQd c cc = DQD cc (symbol c) (c ^. typ) (getUnit c)
