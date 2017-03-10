{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk.VarChunk where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.SymbolForm
import Control.Lens ((^.), set, Simple, Lens)
import Language.Drasil.Chunk.Wrapper (NWrapper, nw)

import Language.Drasil.Symbol
import Language.Drasil.Spec
import Language.Drasil.Space

import Prelude hiding (id)
  
data VarChunk = VC { _ni :: NWrapper
                   , _vsymb :: Symbol
                   , _vtyp  :: Space }

instance Eq VarChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)

instance Chunk VarChunk where
  id = nl id

instance NamedIdea VarChunk where
  term = nl term
  getA (VC n _ _) = getA n

instance SymbolForm VarChunk where
  symbol f (VC n s t) = fmap (\x -> VC n x t) (f s)
  
nl :: (forall c. (NamedIdea c) => Simple Lens c a) -> Simple Lens VarChunk a
nl l f (VC n s t) = fmap (\x -> VC (set l x n) s t) (f (n ^. l))
  
-- the code generation system needs VC to have a type (for now)
-- Setting all varchunks to have Rational type so it compiles
makeVC :: String -> String -> Symbol -> VarChunk
makeVC i des sym = VC (nw $ nc i des) sym Rational

vc :: String -> Sentence -> Symbol -> Space -> VarChunk
vc i d sy t = VC (nw $ ncWDS i d) sy t

vc' :: NamedIdea c => c -> Symbol -> Space -> VarChunk
vc' n s t = VC (nw n) s t

makeVCObj :: String -> String -> Symbol -> String -> VarChunk
makeVCObj i des sym s = VC (nw $ nc i des) sym (Obj s)

vcFromCC :: NamedIdea c => c -> Symbol -> VarChunk
vcFromCC cc sym = VC (nw cc) sym Rational
