{-# LANGUAGE TemplateHaskell #-}
-- | Contains types that define quantities from concepts.
module Language.Drasil.Chunk.DefinedQuantity (
  -- * Chunk Type
  DefinedQuantityDict,
  -- * Type classes
  DefinesQuantity(defLhs),
  -- * Constructors
  quant, quant', quantAU, quantNoUnit, quantNoUnit',
  dqd, dqdNoUnit, dqdNoUnit', dqd', dqdWr,
  implVar, implVar', implVarAU, implVarAU',
  referenceToDefinedQuantityDict
) where

import Control.Lens ((^.), makeLenses, view, Getter)

import Drasil.Database (HasChunkRefs(..), HasUID(..), UID, (+++))
import qualified Data.Set as Set

import Language.Drasil.Symbol (HasSymbol(symbol), Symbol (Empty))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA), Concept, Express(..),
  Definition(defn), ConceptDomain(cdom), Quantity)
import Language.Drasil.Chunk.Concept (ConceptChunk, cw, cncpt'', cncpt''')
import Language.Drasil.Expr.Class (sy)
import Language.Drasil.Chunk.UnitDefn (UnitDefn, MayHaveUnit(getUnit))
import Language.Drasil.Space (Space (Reference), HasSpace(..))
import Language.Drasil.Stages (Stage (Implementation, Equational))
import Language.Drasil.NaturalLanguage.English.NounPhrase.Core (NP)
import Language.Drasil.Sentence (Sentence(..))

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
  chunkRefs d = Set.unions
    [ chunkRefs (d ^. con)
    , chunkRefs (d ^. unit')
    ]
  {-# INLINABLE chunkRefs #-}

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

-- | Construct a 'DefinedQuantityDict' (/with/ a unit)
quant ::
  -- | The 'UID'.
  UID ->
  -- | The quantity being defined.
  NP ->
  -- | The definition of the quantity.
  Sentence ->
  -- | The 'Symbol' used for the quantity.
  Symbol ->
  -- | The 'Space' of the quantity.
  Space ->
  -- | The unit of the quantity.
  UnitDefn -> DefinedQuantityDict
quant u trm def s sp un = DQD (cncpt''' u trm def) (const s) sp (Just un)

-- | Construct a 'DefinedQuantityDict' (/with/ a unit and a symbol dependent on stage)
quant' ::
  -- | The 'UID'.
  UID ->
  -- | The quantity being defined.
  NP ->
  -- | The definition of the quantity.
  Sentence ->
  -- | The 'Symbol' used for the quantity, dependent on the 'Stage'.
  (Stage -> Symbol) ->
  -- | The 'Space' of the quantity.
  Space ->
  -- | The unit of the quantity.
  UnitDefn -> DefinedQuantityDict
quant' u trm def s sp un = DQD (cncpt''' u trm def) s sp (Just un)

-- | Construct a 'DefinedQuantityDict' (/with/ an optional unit, optional
-- abbreviation and a symbol dependent on stage)
quantAU ::
  -- | The 'UID'.
  UID ->
  -- | The quantity being defined.
  NP ->
  -- | The definition of the quantity.
  Sentence ->
  -- | The (optional) abbreviation for the quantity.
  Maybe String ->
  -- | The 'Symbol' used for the quantity, dependent on the 'Stage'.
  (Stage -> Symbol) ->
  -- | The 'Space' of the quantity.
  Space ->
  -- | The (optional) unit of the quantity.
  Maybe UnitDefn -> DefinedQuantityDict
quantAU u trm def a = DQD cc
  where cc = maybe (cncpt''' u trm def) (cncpt'' u trm def) a

-- | Construct a 'DefinedQuantityDict' (/without/ a unit)
quantNoUnit ::
  -- | The 'UID'.
  UID ->
  -- | The quantity being defined.
  NP ->
  -- | The definition of the quantity.
  Sentence ->
  -- | The 'Symbol' used for the quantity.
  Symbol ->
  -- | The 'Space' of the quantity.
  Space -> DefinedQuantityDict
quantNoUnit u trm def s sp = DQD (cncpt''' u trm def) (const s) sp Nothing

-- | Construct a 'DefinedQuantityDict' (/wihout/ a unit and /with/ a symbol dependent on stage)
quantNoUnit' ::
  -- | The 'UID'.
  UID ->
  -- | The quantity being defined.
  NP ->
  -- | The definition of the quantity.
  Sentence ->
  -- | The 'Symbol' used for the quantity, dependent on the 'Stage'.
  (Stage -> Symbol) ->
  -- | The 'Space' of the quantity.
  Space -> DefinedQuantityDict
quantNoUnit' u trm def s sp = DQD (cncpt''' u trm def) s sp Nothing

{-# DEPRECATED dqd, dqd', dqdNoUnit, dqdNoUnit'
  "Smart constructors allow externally-known chunk nesting; use one of `quant, quant', quantNoUnit, quantNoUnit'` instead." #-}

-- | Smart constructor that creates a DefinedQuantityDict with a 'ConceptChunk', a 'Symbol' independent of 'Stage', a 'Space', and a unit.
dqd :: ConceptChunk -> Symbol -> Space -> UnitDefn -> DefinedQuantityDict
dqd c s sp = DQD c (const s) sp . Just

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

-- | Given a DefinedQuantityDict, change its space to be a Reference to its original space
referenceToDefinedQuantityDict :: DefinedQuantityDict -> DefinedQuantityDict
referenceToDefinedQuantityDict quantDict = quantAU (quantDict +++ "ref")
  (quantDict ^. term) (quantDict ^. defn) (getA quantDict) (symbol quantDict)
  (Reference $ quantDict ^. typ) (getUnit quantDict)

-- | Makes a variable that is implementation-only.
implVar :: UID -> NP -> String -> Space -> Symbol -> DefinedQuantityDict
implVar i ter desc sp sym = quantNoUnit' i ter (S desc) f sp
  where
    f :: Stage -> Symbol
    f Implementation = sym
    f Equational = Empty

-- | Similar to 'implVar', but takes in a 'Sentence' for the description rather than a 'String'.
implVar' :: UID -> NP -> Sentence -> Space -> Symbol -> DefinedQuantityDict
implVar' i ter desc sp sym = quantNoUnit' i ter desc f sp
  where
    f :: Stage -> Symbol
    f Implementation = sym
    f Equational = Empty

-- | Similar to 'implVar' but allows specification of abbreviation and unit.
implVarAU :: UID -> NP -> String -> Maybe String -> Space -> Symbol ->
  Maybe UnitDefn -> DefinedQuantityDict
implVarAU s np desc a t sym = quantAU s np (S desc) a f t
  where f :: Stage -> Symbol
        f Implementation = sym
        f Equational = Empty

-- | Similar to 'implVarAU' but takes a Sentence for the description rather than a String.
implVarAU' :: UID -> NP -> Sentence -> Maybe String -> Space -> Symbol ->
  Maybe UnitDefn -> DefinedQuantityDict
implVarAU' s np desc a t sym = quantAU s np desc a f t
  where f :: Stage -> Symbol
        f Implementation = sym
        f Equational = Empty
