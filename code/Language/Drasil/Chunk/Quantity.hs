{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs,Rank2Types #-}
module Language.Drasil.Chunk.Quantity 
  ( Quantity(..), QWrapper, qw
  ) where

import Control.Lens

import Language.Drasil.Space
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Chunk.ConVar

import Prelude hiding (id)

import Language.Drasil.Chunk.SymbolForm (SF(..))
import Language.Drasil.Unit(UnitDefn)

-- | A Quantity is a 'NamedIdea' with a 'Space' that may have 
-- a symbol and units
class NamedIdea c => Quantity c where
  -- | Lens to the Space
  typ      :: Simple Lens c Space
  -- | Provides the 'Language.Drasil.Chunk.SymbolForm.SymbolForm' 
  -- (chunk which contains a symbol) for a quantity 
  -- if it exists, otherwise returns 'Nothing'
  getSymb  :: c -> Maybe SF
  -- | Provides the units a quantity is measured in, if any, otherwise returns
  -- 'Nothing'
  getUnit  :: c -> Maybe UnitDefn

instance Quantity VarChunk where
  getSymb    = Just . SF 
  getUnit _  = Nothing
  typ f (VC n s t) = fmap (\x -> VC n s x) (f t)
  
instance Quantity ConVar where
  typ    f (CV c s t) = fmap (\x -> CV c s x) (f t)
  getSymb   = Just . SF 
  getUnit _ = Nothing

data QWrapper where
  QW :: (Quantity q) => q -> QWrapper
  
instance Chunk QWrapper where
  id = qlens id
  
instance NamedIdea QWrapper where
  term = qlens term
  getA (QW a) = getA a
  
instance Quantity QWrapper where
  typ = qlens typ
  getSymb (QW a) = getSymb a
  getUnit (QW a) = getUnit a

qlens :: (forall c. (Quantity c) => 
  Simple Lens c a) -> Simple Lens QWrapper a
qlens l f (QW a) = fmap (\x -> QW (set l x a)) (f (a ^. l))

qw :: Quantity q => q -> QWrapper
qw = QW
