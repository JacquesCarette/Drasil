{-# LANGUAGE TemplateHaskell #-}
-- | Contains types that define quantities from concepts.
module Language.Drasil.Chunk.DefinedQuantity (
  -- * Chunk Type
  DefinedQuantityDict,
  -- * Type classes
  DefinesQuantity(defLhs),
  -- * Constructors
  dqd, dqdNoUnit, dqdNoUnit', dqd', dqdQd, dqdWr,
  implVar, implVar', implVarAU, implVarAU'
) where

import Control.Lens ((^.), makeLenses, view, Getter)

import Drasil.Database (HasChunkRefs(..), HasUID(..))

import Language.Drasil.Symbol (HasSymbol(symbol), Symbol (Empty))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), Concept, Express(..),
  Definition(defn), ConceptDomain(cdom), IsUnit, Quantity)
import Language.Drasil.Chunk.Concept (ConceptChunk, cw, dcc, dccWDS, dccA, dccAWDS, cc')
import Language.Drasil.Expr.Class (sy)
import Language.Drasil.Chunk.UnitDefn (UnitDefn, unitWrapper,
  MayHaveUnit(getUnit))
import Language.Drasil.Space (Space, HasSpace(..))
import Language.Drasil.Stages (Stage (Implementation, Equational))
import Language.Drasil.NounPhrase.Types (NP)
import Language.Drasil.Sentence (Sentence)

-- | DefinedQuantityDict is the combination of a 'Concept' and a 'Quantity'.
-- Contains a 'ConceptChunk', a 'Symbol' dependent on 'Stage', a 'Space', and maybe a 'UnitDefn'.
-- Used when we want to assign a quantity to a concept. Includes the space, symbol, and units for that quantity.
--
-- Ex. A pendulum arm can be defined as a concept with a symbol (l), space (Real numbers), and units (cm, m, etc.).
data DefinedQuantityDict = DQD { _con :: ConceptChunk
                               , _symb :: Stage -> Symbol
                               , _spa :: Space
                               , _unit' :: Maybe UnitDefn
                               }
makeLenses ''DefinedQuantityDict

class DefinesQuantity d where
  defLhs :: Getter d DefinedQuantityDict

instance HasChunkRefs DefinedQuantityDict where
  chunkRefs = const mempty -- FIXME: `chunkRefs` should actually collect the referenced chunks.

-- | Finds the 'UID' of the 'ConceptChunk' used to make the 'DefinedQuantityDict'.
instance HasUID        DefinedQuantityDict where uid = con . uid
-- | Equal if 'UID's are equal.
instance Eq            DefinedQuantityDict where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the term ('NP') of the 'ConceptChunk' used to make the 'DefinedQuantityDict'.
instance NamedIdea     DefinedQuantityDict where term = con . term
-- | Finds the idea contained in the 'ConceptChunk' used to make the 'DefinedQuantityDict'.
instance Idea          DefinedQuantityDict where getA = getA . view con
-- | Finds the definition contained in the 'ConceptChunk' used to make the 'DefinedQuantityDict'.
instance Definition    DefinedQuantityDict where defn = con . defn
-- | Finds the domain of the 'ConceptChunk' used to make the 'DefinedQuantityDict'.
instance ConceptDomain DefinedQuantityDict where cdom = cdom . view con
-- | Finds the 'Space' of the 'DefinedQuantityDict'.
instance HasSpace      DefinedQuantityDict where typ = spa
-- | Finds the 'Stage' -> 'Symbol' of the 'DefinedQuantityDict'.
instance HasSymbol     DefinedQuantityDict where symbol = view symb
-- | 'DefinedQuantityDict's have a 'Quantity'.
instance Quantity      DefinedQuantityDict where
-- | Finds the units of the 'DefinedQuantityDict'.
instance MayHaveUnit   DefinedQuantityDict where getUnit = view unit'
-- | Convert the symbol of the 'DefinedQuantityDict' to a 'ModelExpr'.
instance Express       DefinedQuantityDict where express = sy

-- | Smart constructor that creates a DefinedQuantityDict with a 'ConceptChunk', a 'Symbol' independent of 'Stage', a 'Space', and a unit.
dqd :: (IsUnit u) => ConceptChunk -> Symbol -> Space -> u -> DefinedQuantityDict
dqd c s sp = DQD c (const s) sp . Just . unitWrapper

-- | Similar to 'dqd', but without any units.
dqdNoUnit :: ConceptChunk -> Symbol -> Space -> DefinedQuantityDict
dqdNoUnit c s sp = DQD c (const s) sp Nothing

dqdNoUnit' :: ConceptChunk -> (Stage -> Symbol) -> Space -> DefinedQuantityDict
dqdNoUnit' c s sp = DQD c s sp Nothing

-- | Similar to 'dqd', but the 'Symbol' is now dependent on the 'Stage'.
dqd' :: ConceptChunk -> (Stage -> Symbol) -> Space -> Maybe UnitDefn -> DefinedQuantityDict
dqd' = DQD

-- | When the input already has all the necessary information. A 'projection' operator from some a type with instances of listed classes to a 'DefinedQuantityDict'.
dqdWr :: (Quantity c, Concept c, MayHaveUnit c) => c -> DefinedQuantityDict
dqdWr c = DQD (cw c) (symbol c) (c ^. typ) (getUnit c)

-- | When we want to merge a quantity and a concept. This is suspicious.
dqdQd :: (Quantity c, MayHaveUnit c) => c -> Sentence -> DefinedQuantityDict
dqdQd c cc = DQD (cc' c cc) (symbol c) (c ^. typ) (getUnit c)

-- | Makes a variable that is implementation-only.
implVar :: String -> NP -> String -> Space -> Symbol -> DefinedQuantityDict
implVar i ter desc sp sym = dqdNoUnit' (dcc i ter desc) f sp
  where
    f :: Stage -> Symbol
    f Implementation = sym
    f Equational = Empty

-- | Similar to 'implVar', but takes in a 'Sentence' for the description rather than a 'String'.
implVar' :: String -> NP -> Sentence -> Space -> Symbol -> DefinedQuantityDict
implVar' i ter desc sp sym = dqdNoUnit' (dccWDS i ter desc) f sp
  where
    f :: Stage -> Symbol
    f Implementation = sym
    f Equational = Empty

-- | Similar to 'implVar' but allows specification of abbreviation and unit.
implVarAU :: String -> NP -> String -> Maybe String -> Space -> Symbol ->
  Maybe UnitDefn -> DefinedQuantityDict
implVarAU s np desc a t sym = dqd' (dccA s np desc a) f t
  where f :: Stage -> Symbol
        f Implementation = sym
        f Equational = Empty

-- | Similar to 'implVarAU' but takes a Sentence for the description rather than a String.
implVarAU' :: String -> NP -> Sentence -> Maybe String -> Space -> Symbol ->
  Maybe UnitDefn -> DefinedQuantityDict
implVarAU' s np desc a t sym = dqd' (dccAWDS s np desc a) f t
  where f :: Stage -> Symbol
        f Implementation = sym
        f Equational = Empty
