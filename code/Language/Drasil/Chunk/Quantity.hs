{-# LANGUAGE GADTs,Rank2Types #-}
module Language.Drasil.Chunk.Quantity 
  ( Quantity(..), QuantityDict, qw, symbol, eqSymb, codeSymb, mkQuant, HasSpace(typ)
  ) where

import Control.Lens

import Language.Drasil.Space
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Chunk.ConVar
import Language.Drasil.Chunk.SymbolForm (ssc')
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.NounPhrase

import Prelude hiding (id)

import qualified Language.Drasil.Chunk.SymbolForm as SF (Stage(..)
  , getSymbForStage, StagedSymbolChunk)
import Language.Drasil.Unit(UnitDefn)

-- | HasSpace is anything which has a Space...
class HasSpace c where
  typ      :: Simple Lens c Space

-- | A Quantity is an 'Idea' with a 'Space' and a symbol and 
-- may have units
class (Idea c, HasSpace c) => Quantity c where
  -- | Provides the Symbol --  for a quantity for a particular stage of generation
  getSymb  :: SF.Stage -> c -> Symbol
  -- | Provides the units a quantity is measured in, if any, otherwise returns
  -- 'Nothing'
  getUnit  :: c -> Maybe UnitDefn
  -- | Provides the StagedSymbolChunk corresponding to a quantity
  getStagedS :: c -> SF.StagedSymbolChunk

instance HasSpace VarChunk where
  typ f (VC n s t) = fmap (\x -> VC n s x) (f t)
instance Quantity VarChunk where
  getSymb st (VC _ s _) = SF.getSymbForStage st s 
  getUnit _  = Nothing
  getStagedS (VC _ s _) = s
  
instance HasSpace ConVar where
  typ    f (CV c s t) = fmap (\x -> CV c s x) (f t)
instance Quantity ConVar where
  getSymb st (CV _ s _) = SF.getSymbForStage st s
  getUnit _ = Nothing
  getStagedS (CV _ s _) = s

data QuantityDict = QD { _id :: IdeaDict, _typ :: Space,
  _symb :: SF.Stage -> Symbol,
  _unit :: Maybe UnitDefn, _stagedS :: SF.StagedSymbolChunk }

instance Chunk QuantityDict where
  id = qlens . id
  
instance NamedIdea QuantityDict where
  term = qlens . term

instance Idea QuantityDict where
  getA  qd = getA (qd ^. qlens)
  
instance HasSpace QuantityDict where
  typ f qd = fmap (\x -> qd {_typ = x}) (f (_typ qd))
instance Quantity QuantityDict where
  getSymb s qd = _symb qd s
  getUnit qd = _unit qd
  getStagedS qd = _stagedS qd
  
instance Eq QuantityDict where
  a == b = (a ^. id) == (b ^. id)

instance Ord QuantityDict where
  compare a b = -- FIXME: Ordering hack. Should be context-dependent
    compare (getSymb SF.Equational a) (getSymb SF.Equational b)

qlens :: Simple Lens QuantityDict IdeaDict
qlens f qd = fmap (\x -> qd {_id = x}) (f (_id qd))

qw :: Quantity q => q -> QuantityDict
qw q = QD (nw q) (q^.typ) (\stg -> getSymb stg q) (getUnit q) (getStagedS q)

mkQuant :: String -> NP -> Symbol -> Space -> Maybe UnitDefn -> QuantityDict
mkQuant i t s sp u = QD (mkIdea i t Nothing) sp (\_ -> s) u (ssc' i s)

-- | Helper function for getting a symbol at a given stage from a quantity.
symbol :: Quantity q => SF.Stage -> q -> Symbol
symbol = getSymb

-- | Helper function for getting a symbol in the Equational Stage
eqSymb :: Quantity q => q -> Symbol
eqSymb = symbol SF.Equational

-- | Helper function for getting a symbol in the Implementation Stage
codeSymb :: Quantity q => q -> Symbol
codeSymb = symbol SF.Implementation
