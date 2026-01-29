{-# LANGUAGE TemplateHaskell #-}
-- | Defines chunks to add units to a quantity. Similar to 'UnitaryChunk'.
module Language.Drasil.Chunk.Unital (
  -- * Chunk Type
  UnitalChunk(..),
  -- * Constructors
  uc, uc', ucStaged, ucStaged', ucuc, ucw) where

import Control.Lens (makeLenses, view, (^.))

import Drasil.Database (HasUID(..))

import Language.Drasil.Chunk.Concept (dccWDS,cw)
import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqd, dqd', dqdWr)
import Language.Drasil.Chunk.Unitary (Unitary(..))
import Language.Drasil.Symbol
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), Express(express),
  Definition(defn), ConceptDomain(cdom), Concept, IsUnit, Quantity)
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit), TempHasUnit(findUnit),  UnitDefn, unitWrapper)
import Language.Drasil.Expr.Class (sy)
import Language.Drasil.NounPhrase.Types (NP)
import Language.Drasil.Space (Space(..), HasSpace(..))
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Stages (Stage)

-- | Similar to a `DefinedQuantityDict`, UnitalChunks are concepts
-- with quantities that must have a unit definition.
-- Contains 'DefinedQuantityDict's and a 'UnitDefn'.
--
-- Ex. A pendulum arm is a tangible object with a symbol (l) and units (cm, m, etc.).
data UnitalChunk = UC { _defq' :: DefinedQuantityDict
                      , _uni :: UnitDefn
                      }
makeLenses ''UnitalChunk

-- | Finds 'UID' of the 'DefinedQuantityDict' used to make the 'UnitalChunk'.
instance HasUID        UnitalChunk where uid = defq' . uid
-- | Finds term ('NP') of the 'DefinedQuantityDict' used to make the 'UnitalChunk'.
instance NamedIdea     UnitalChunk where term = defq' . term
-- | Finds the idea contained in the 'DefinedQuantityDict' used to make the 'UnitalChunk'.
instance Idea          UnitalChunk where getA (UC qc _) = getA qc
-- | Finds definition of the 'DefinedQuantityDict' used to make the 'UnitalChunk'.
instance Definition    UnitalChunk where defn = defq' . defn
-- | Finds the domain contained in the 'DefinedQuantityDict' used to make the 'UnitalChunk'.
instance ConceptDomain UnitalChunk where cdom = cdom . view defq'
-- | Finds the 'Space' of the 'DefinedQuantityDict' used to make the 'UnitalChunk'.
instance HasSpace      UnitalChunk where typ = defq' . typ
-- | Finds the 'Symbol' of the 'DefinedQuantityDict' used to make the 'UnitalChunk'.
instance HasSymbol     UnitalChunk where symbol c = symbol (c^.defq')
-- | 'UnitalChunk's have a 'Quantity'.
instance Quantity      UnitalChunk where
-- | Finds the unit definition of a 'UnitalChunk'.
instance Unitary       UnitalChunk where unit = view uni
-- | Finds the units used to make the 'UnitalChunk'.
instance MayHaveUnit   UnitalChunk where getUnit = Just . view uni
-- | Finds the units used to make the 'UnitalChunk'.
instance TempHasUnit       UnitalChunk where findUnit = view uni
-- | Equal if 'UID's are equal.
instance Eq            UnitalChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Convert the symbol of the 'UnitalChunk' to a 'ModelExpr'.
instance Express       UnitalChunk where express = sy

--{BEGIN HELPER FUNCTIONS}--

-- | Used to create a 'UnitalChunk' from a 'Concept', 'Symbol', and 'Unit'.
uc :: (Concept c, IsUnit u) => c -> Symbol -> Space -> u -> UnitalChunk
uc a sym space c = UC (dqd (cw a) sym space un) un
 where un = unitWrapper c

-- | Similar to 'uc', except it builds the 'Concept' portion of the 'UnitalChunk'
-- from a given 'UID', term, and definition (as a 'Sentence') which are its first three arguments.
uc' :: (IsUnit u) => String -> NP -> Sentence -> Symbol -> Space -> u -> UnitalChunk
uc' i t d sym space u = UC (dqd (dccWDS i t d) sym space un) un
 where un = unitWrapper u

-- | Similar to 'uc', but 'Symbol' is dependent on the 'Stage'.
ucStaged :: (Concept c, IsUnit u) => c ->  (Stage -> Symbol) ->
  Space -> u -> UnitalChunk
ucStaged a sym space u = UC (dqd' (cw a) sym space (Just un)) un
 where un = unitWrapper u

-- | Similar to 'uc'', but 'Symbol' is dependent on the 'Stage'.
ucStaged' :: (IsUnit u) => String -> NP -> Sentence -> (Stage -> Symbol) ->
  Space -> u -> UnitalChunk
ucStaged' i t d sym space u = UC (dqd' (dccWDS i t d) sym space (Just un)) un
 where un = unitWrapper u

-- | Attach units to a chunk that has a symbol and definition.
ucuc :: (Quantity c, Concept c, MayHaveUnit c) => c -> UnitDefn -> UnitalChunk
ucuc c = UC (dqdWr c)

-- | Constructs a UnitalChunk from a 'Concept' with 'Units'.
ucw :: (Unitary c, Concept c, MayHaveUnit c) => c -> UnitalChunk
ucw c = ucuc c (unit c)
