{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs,Rank2Types #-}
module Language.Drasil.Chunk.Quantity 
  ( Quantity(..), QWrapper, qw, symbol, eqSymb, codeSymb, qs
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

data QWrapper where
  QW :: (Quantity q) => q -> QWrapper
  
instance Chunk QWrapper where
  id = qlens id
  
instance NamedIdea QWrapper where
  term = qlens term

instance Idea QWrapper where
  getA (QW a) = getA a
  
instance Quantity QWrapper where
  typ = qlens typ
  getSymb s (QW a) = getSymb s a
  getUnit (QW a) = getUnit a
  getStagedS (QW a) = getStagedS a
  
instance Eq QWrapper where
  a == b = (a ^. id) == (b ^. id)

instance Ord QWrapper where
  compare a b = -- FIXME: Ordering hack. Should be context-dependent
    compare ((getSymb SF.Equational a) ^. SF.symbol) ((getSymb SF.Equational b) ^. SF.symbol)

qlens :: (forall c. (Quantity c) => 
  Simple Lens c a) -> Simple Lens QWrapper a
qlens l f (QW a) = fmap (\x -> QW (set l x a)) (f (a ^. l))

-- | qw and qs do the same thing since SymbolForm was removed as a constraint
-- on many chunks and Quantity now must have a Symbol.
qw, qs :: Quantity q => q -> QWrapper
qw = QW

qs = QW

-- | Helper function for getting a symbol at a given stage from a quantity.
symbol :: Quantity q => SF.Stage -> q -> Symbol
symbol s q = (getSymb s q) ^. SF.symbol

-- | Helper function for getting a symbol in the Equational Stage
eqSymb :: Quantity q => q -> Symbol
eqSymb = symbol SF.Equational

-- | Helper function for getting a symbol in the Implementation Stage
codeSymb :: Quantity q => q -> Symbol
codeSymb = symbol SF.Implementation
