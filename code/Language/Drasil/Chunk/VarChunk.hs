{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.VarChunk where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.SymbolForm
import Control.Lens ((^.), makeLenses, view)

import Language.Drasil.Symbol
import Language.Drasil.Space

import Language.Drasil.NounPhrase (NP)

import Prelude hiding (id)
  
-- | VarChunks are Quantities that have symbols, but not units.
data VarChunk = VC { _ni :: IdeaDict
                   , _vsymb :: StagedSymbolChunk
                   , _vtyp  :: Space }
makeLenses ''VarChunk

instance Eq        VarChunk where c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk     VarChunk where id = ni . id
instance NamedIdea VarChunk where term = ni . term
instance Idea      VarChunk where getA = getA . view ni

-- the code generation system needs VC to have a type (for now)
-- Setting all varchunks to have Real type so it compiles
-- | Creates a VarChunk from an id, term, and symbol. Assumes Real 'Space'
-- Needs to be removed
makeVC :: String -> NP -> Symbol -> VarChunk
makeVC i des sym = vc i des sym Real

makeVC' :: String -> NP -> Symbol -> VarChunk
makeVC' i des sym = makeVC'' i des sym Real

makeVC'' :: String -> NP -> Symbol -> Space -> VarChunk
makeVC'' i des sym typ = vcSt i des (ssc'' i [(Implementation, sym)]) typ
-- | Creates a VarChunk from an id, term, symbol, and space
vc :: String -> NP -> Symbol -> Space -> VarChunk
vc i des sym space = VC (nw $ nc i des) (ssc' i sym) space

vcSt :: String -> NP -> StagedSymbolChunk -> Space -> VarChunk
vcSt i des sym space = VC (nw $ nc i des) sym space

-- | Creates a VarChunk from an 'Idea', symbol, and space
vc' :: Idea c => c -> Symbol -> Space -> VarChunk
vc' n s t = VC (nw n) (ssc' (n ^. id) s) t

codeVC :: Idea c => c -> Symbol -> Space -> VarChunk
codeVC  n s t = VC (nw n) (ssc'' (n ^. id) [(Implementation,s)]) t

-- | Creates a VarChunk from an 'Idea''s id and term and symbol
vc'' :: Idea c => c -> Symbol -> Space -> VarChunk
vc'' n sy space = vc (n ^. id) (n ^. term) sy space

-- | Creates a VarChunk from an id, term, symbol, and space
makeVCObj :: String -> NP -> Symbol -> String -> VarChunk
makeVCObj i des sym s = VC (nw $ nc i des) (ssc' i sym) (Obj s)
