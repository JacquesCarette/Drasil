{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs #-}
module Language.Drasil.Chunk.Quantity where

import Control.Lens

import Language.Drasil.Space
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
  