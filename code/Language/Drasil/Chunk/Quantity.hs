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

class NamedIdea c => Quantity c where
  typ      :: Simple Lens c Space
  getSymb  :: c -> Maybe SF
  getUnit  :: c -> Maybe UnitDefn

instance Quantity VarChunk where
  getSymb    = Just . SF 
  getUnit _  = Nothing
  typ f (VC n s t) = fmap (\x -> VC n s x) (f t)
  
instance Quantity ConVar where
  typ    f (CV c s t) = fmap (\x -> CV c s x) (f t)
  getSymb   = Just . SF 
  getUnit _ = Nothing