{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk.VarChunk where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.SymbolForm
import Control.Lens ((^.))

import Language.Drasil.Symbol
import Language.Drasil.Spec
import Language.Drasil.Space

import Prelude hiding (id)
  
data VarChunk = VC { vid :: String
                   , vdesc :: Sentence
                   , vsymb :: Symbol
                   , vtyp  :: Space }

instance Eq VarChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)

instance Chunk VarChunk where
  id f (VC n d s t) = fmap (\x -> VC x d s t) (f n)

instance NamedIdea VarChunk where
  term f (VC n d s t) = fmap (\x -> VC n x s t) (f d)
  getA _ = Nothing

instance SymbolForm VarChunk where
  symbol f (VC n d s t) = fmap (\x -> VC n d x t) (f s)
  
-- the code generation system needs VC to have a type (for now)
-- Setting all varchunks to have Rational type so it compiles
makeVC :: String -> String -> Symbol -> VarChunk
makeVC i des sym = VC i (S des) sym Rational

vc :: String -> Sentence -> Symbol -> Space -> VarChunk
vc i d sy t = VC i d sy t

makeVCObj :: String -> String -> Symbol -> String -> VarChunk
makeVCObj i des sym s = VC i (S des) sym (Obj s)

vcFromCC :: NamedChunk -> Symbol -> VarChunk
vcFromCC cc sym = VC (cc ^. id) (cc ^. term) sym Rational
