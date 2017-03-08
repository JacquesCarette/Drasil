{-# Language GADTs, Rank2Types #-}

module Language.Drasil.Chunk.ConVar
  ( ConVar(..)
  , cv
  , cvR
  ) where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk.Concept

import Control.Lens (Simple, Lens, (^.))

import Language.Drasil.Symbol
import Language.Drasil.Space

import Prelude hiding (id)
  
data ConVar = CV { _con :: ConceptChunk
                 , _symb :: Symbol
                 , _typ :: Space }
                     
instance Eq ConVar where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk ConVar where
  id = cvl . id
instance NamedIdea ConVar where
  term = cvl . term
  getA (CV c _ _) = getA c
instance Concept ConVar where
  defn = cvl . defn
instance SymbolForm ConVar where
  symbol f (CV c s t) = fmap (\x -> CV c x t) (f s)

cvl :: Simple Lens ConVar ConceptChunk
cvl f (CV c s t) = fmap (\x -> CV x s t) (f c)

cv :: ConceptChunk -> Symbol -> Space -> ConVar
cv = CV

--FIXME: Remove this hack
cvR :: ConceptChunk -> Symbol -> ConVar
cvR c s = CV c s Rational
