{-# LANGUAGE GADTs,Rank2Types #-}
module Language.Drasil.Chunk.Quantity 
  ( Quantity(..), QuantityDict, qw, symbol, eqSymb, codeSymb,
  ) where

import Control.Lens

import Language.Drasil.Space
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Chunk.ConVar
import Language.Drasil.Symbol (Symbol)

import Prelude hiding (id)

import qualified Language.Drasil.Chunk.SymbolForm as SF (SF(..), symbol, Stage(..)
                                        , getSymbForStage, StagedSymbolChunk)
import Language.Drasil.Unit(UnitDefn)

-- | A Quantity is an 'Idea' with a 'Space' that may have 
-- a symbol and units
class Idea c => Quantity c where
  -- | Lens to the Space
  typ      :: Simple Lens c Space
  -- | Provides the 'Language.Drasil.Chunk.SymbolForm.SymbolForm' 
  -- (chunk which contains a symbol) for a quantity for a particular stage of 
  -- generation
  getSymb  :: SF.Stage -> c -> SF.SF
  -- | Provides the units a quantity is measured in, if any, otherwise returns
  -- 'Nothing'
  getUnit  :: c -> Maybe UnitDefn
  -- | Provides the StagedSymbolChunk corresponding to a quantity
  getStagedS :: c -> SF.StagedSymbolChunk

instance Quantity VarChunk where
  getSymb st (VC _ s _) = SF.SF $ SF.getSymbForStage st s 
  getUnit _  = Nothing
  typ f (VC n s t) = fmap (\x -> VC n s x) (f t)
  getStagedS (VC _ s _) = s
  
instance Quantity ConVar where
  typ    f (CV c s t) = fmap (\x -> CV c s x) (f t)
  getSymb st (CV _ s _) = SF.SF $ SF.getSymbForStage st s
  getUnit _ = Nothing
  getStagedS (CV _ s _) = s

data QuantityDict = QD { _id :: IdeaDict, _typ :: Space,
  _symb :: SF.Stage -> SF.SF,
  _unit :: Maybe UnitDefn, _stagedS :: SF.StagedSymbolChunk }

instance Chunk QuantityDict where
  id = qlens . id
  
instance NamedIdea QuantityDict where
  term = qlens . term

instance Idea QuantityDict where
  getA  qd = getA (qd ^. qlens)
  
instance Quantity QuantityDict where
  typ f qd = fmap (\x -> qd {_typ = x}) (f (_typ qd))
  getSymb s qd = _symb qd s
  getUnit qd = _unit qd
  getStagedS qd = _stagedS qd
  
instance Eq QuantityDict where
  a == b = (a ^. id) == (b ^. id)

instance Ord QuantityDict where
  compare a b = -- FIXME: Ordering hack. Should be context-dependent
    compare ((getSymb SF.Equational a) ^. SF.symbol) ((getSymb SF.Equational b) ^. SF.symbol)

qlens :: Simple Lens QuantityDict IdeaDict
qlens f qd = fmap (\x -> qd {_id = x}) (f (_id qd))

qw :: Quantity q => q -> QuantityDict
qw q = QD (nw q) (q^.typ) (\stg -> getSymb stg q) (getUnit q) (getStagedS q)

-- | Helper function for getting a symbol at a given stage from a quantity.
symbol :: Quantity q => SF.Stage -> q -> Symbol
symbol s q = (getSymb s q) ^. SF.symbol

-- | Helper function for getting a symbol in the Equational Stage
eqSymb :: Quantity q => q -> Symbol
eqSymb = symbol SF.Equational

-- | Helper function for getting a symbol in the Implementation Stage
codeSymb :: Quantity q => q -> Symbol
codeSymb = symbol SF.Implementation
