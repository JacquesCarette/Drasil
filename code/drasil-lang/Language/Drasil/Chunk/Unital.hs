{-# LANGUAGE TemplateHaskell #-}
-- | Defines chunks to add units to a quantity. Similar to 'UnitaryChunk'.
module Language.Drasil.Chunk.Unital (
  -- * Chunk Type
  UnitalChunk(..),
  -- * Constructors
  makeUCWDS , uc , uc' , ucStaged, ucs , ucs', ucsWS, ucuc, ucw) where

import Control.Lens (makeLenses, view, (^.))

import Language.Drasil.Chunk.Concept (dcc, dccWDS,cw)
import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqd, dqd', tempdqdWr')
import Language.Drasil.Chunk.Unitary (Unitary(..))
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), Express(express),
  Definition(defn), ConceptDomain(cdom), Concept, IsUnit, Quantity, HasSpace(typ))
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit), TempHasUnit(findUnit),  UnitDefn, unitWrapper)
import Language.Drasil.Expr (sy)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space(..))
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
instance Express       UnitalChunk where express = express . sy

--{BEGIN HELPER FUNCTIONS}--

-- | Used to create a 'UnitalChunk' from a 'Concept', 'Symbol', and 'Unit'.
-- Assumes the 'Space' is Real.
uc :: (Concept c, IsUnit u) => c -> Symbol -> u -> UnitalChunk
uc a b = ucs' a b Real

-- | Similar to 'uc' but does not assume the 'Space'.
ucs' :: (Concept c, IsUnit u) => c -> Symbol -> Space -> u -> UnitalChunk
ucs' a sym space c = UC (dqd (cw a) sym space un) un
 where un = unitWrapper c

-- | Similar to 'uc', except it builds the 'Concept' portion of the 'UnitalChunk'
-- from a given 'UID', term, and definition (which are the first three arguments).
uc' :: (IsUnit u) => String -> NP -> String -> Symbol -> u -> UnitalChunk
uc' i t d s u = UC (dqd (dcc i t d) s Real un) un
 where un = unitWrapper u

-- | Similar to 'uc'', but 'Symbol' is dependent on the 'Stage'.
ucStaged :: (IsUnit u) => String -> NP -> String -> (Stage -> Symbol) -> u -> 
  UnitalChunk
ucStaged i t d s u = UC (dqd' (dcc i t d) s Real (Just un)) un
 where un = unitWrapper u

-- | Similar to 'uc'', but does not assume the 'Space'.
ucs :: (IsUnit u) => String -> NP ->
  String -> Symbol -> Space -> u -> UnitalChunk
ucs nam trm desc sym space un = UC (dqd (dcc nam trm desc) sym space uu) uu
  where uu = unitWrapper un

-- | Similar to 'ucs', but uses a 'Sentence' for description.
ucsWS :: (IsUnit u) => String -> NP ->
  Sentence -> Symbol -> Space -> u -> UnitalChunk
ucsWS nam trm desc sym space un = UC (dqd (dccWDS nam trm desc) sym space uu) uu
  where uu = unitWrapper un

--Better names will come later.
-- | Creates a 'UnitalChunk' in the same way as 'uc'', but with a 'Sentence' for
-- the definition instead of a 'String'.
makeUCWDS :: (IsUnit u) => String -> NP -> Sentence -> Symbol ->
  u -> UnitalChunk
makeUCWDS nam trm desc sym un = UC (dqd (dccWDS nam trm desc) sym Real uu) uu
  where uu = unitWrapper un

-- | Attach units to a chunk that has a symbol and definition.
ucuc :: (Quantity c, Concept c, MayHaveUnit c) => c -> UnitDefn -> UnitalChunk
ucuc c = UC (tempdqdWr' c)

-- | Constructs a UnitalChunk from a 'Concept' with 'Units'.
ucw :: (Unitary c, Concept c, MayHaveUnit c) => c -> UnitalChunk
ucw c = ucuc c (unit c)
