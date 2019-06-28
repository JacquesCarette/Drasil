{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Unital 
  ( UnitalChunk(..) , makeUCWDS , uc , uc' , ucs , ucs' , ucs'' , ucsWS) where

import Control.Lens (makeLenses, view, (^.))

import Language.Drasil.Chunk.Concept (dcc, dccWDS,cw)
import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqd, dqd')
import Language.Drasil.Chunk.Unitary (Unitary(..))
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, IsUnit, Quantity, HasSpace(typ))
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit), UnitDefn, unitWrapper)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space(..))
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Stages (Stage)

-- | UnitalChunks are Unitary DefinedQuantityDict
data UnitalChunk = UC { _defq' :: DefinedQuantityDict
                      , _uni :: UnitDefn
                      }
makeLenses ''UnitalChunk

instance HasUID        UnitalChunk where uid = defq' . uid
instance NamedIdea     UnitalChunk where term = defq' . term
instance Idea          UnitalChunk where getA (UC qc _) = getA qc
instance Definition    UnitalChunk where defn = defq' . defn
instance ConceptDomain UnitalChunk where cdom = cdom . view defq'
instance HasSpace      UnitalChunk where typ = defq' . typ
instance HasSymbol     UnitalChunk where symbol c = symbol (c^.defq')
instance Quantity      UnitalChunk where 
instance Unitary       UnitalChunk where unit = view uni
instance MayHaveUnit   UnitalChunk where getUnit = Just . view uni
instance Eq            UnitalChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid) 

--{BEGIN HELPER FUNCTIONS}--

-- | Used to create a UnitalChunk from a 'Concept', 'Symbol', and 'Unit'.
-- Assumes the 'Space' is Real
uc :: (Concept c, IsUnit u) => c -> Symbol -> u -> UnitalChunk
uc a b = ucs' a b Real

ucs' :: (Concept c, IsUnit u) => c -> Symbol -> Space -> u -> UnitalChunk
ucs' a sym space c = UC (dqd (cw a) sym space un) un
 where un = unitWrapper c

-- | Same as 'uc', except it builds the Concept portion of the UnitalChunk
-- from a given uid, term, and defn. Those are the first three arguments
uc' :: (IsUnit u) => String -> NP -> String -> Symbol ->
  u -> UnitalChunk
uc' i t d s u = UC (dqd (dcc i t d) s Real un) un
 where un = unitWrapper u

-- | Same as 'uc'', but does not assume the 'Space'
ucs :: (IsUnit u) => String -> NP ->
  String -> Symbol -> Space -> u -> UnitalChunk
ucs nam trm desc sym space un = UC (dqd (dcc nam trm desc) sym space uu) uu
  where uu = unitWrapper un

-- | For when the symbol changes depending on the stage
ucs'' :: String -> NP -> String -> (Stage -> Symbol) -> Space -> UnitDefn ->
  UnitalChunk
ucs'' nam trm desc sym space un = UC (dqd' (dcc nam trm desc) sym space (Just un)) un

-- ucs With a Sentence for desc
ucsWS :: (IsUnit u) => String -> NP ->
  Sentence -> Symbol -> Space -> u -> UnitalChunk
ucsWS nam trm desc sym space un = UC (dqd (dccWDS nam trm desc) sym space uu) uu
  where uu = unitWrapper un

--Better names will come later.
-- | Create a UnitalChunk in the same way as 'uc'', but with a 'Sentence' for
-- the definition instead of a String
makeUCWDS :: (IsUnit u) => String -> NP -> Sentence -> Symbol ->
  u -> UnitalChunk
makeUCWDS nam trm desc sym un = UC (dqd (dccWDS nam trm desc) sym Real uu) uu
  where uu = unitWrapper un
