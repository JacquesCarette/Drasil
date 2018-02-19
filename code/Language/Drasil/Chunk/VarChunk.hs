{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.VarChunk where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.SymbolForm (Stage(..), HasSymbol(symbol))
import Language.Drasil.Chunk.Quantity (Quantity(getUnit))

import Language.Drasil.Symbol
import Language.Drasil.Space

import Language.Drasil.NounPhrase (NP)

import Prelude hiding (id)
import Control.Lens ((^.), makeLenses, view)
  
-- | VarChunks are Quantities that have symbols, but not units.
data VarChunk = VC { _ni :: IdeaDict
                   , _vsymb :: Stage -> Symbol
                   , _vtyp  :: Space }
makeLenses ''VarChunk

instance Eq        VarChunk where c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk     VarChunk where id = ni . id
instance NamedIdea VarChunk where term = ni . term
instance Idea      VarChunk where getA = getA . view ni
instance HasSymbol VarChunk where symbol st (VC _ s _) = s st
instance HasSpace  VarChunk where typ f (VC n s t) = fmap (\x -> VC n s x) (f t)
instance Quantity  VarChunk where getUnit _  = Nothing
  

-- the code generation system needs VC to have a type (for now)
-- Setting all varchunks to have Real type so it compiles
-- | Creates a VarChunk from an id, term, and symbol. Assumes Real 'Space'
-- Needs to be removed
makeVC'' :: String -> NP -> Symbol -> Space -> VarChunk
makeVC'' i des sym ty = vcSt i des f ty
  where
    f :: Stage -> Symbol
    f Implementation = sym
    f Equational = Empty

-- | Creates a VarChunk from an id, term, symbol, and space
vc :: String -> NP -> Symbol -> Space -> VarChunk
vc i des sym space = VC (nw $ nc i des) (\_ -> sym) space

vcSt :: String -> NP -> (Stage -> Symbol) -> Space -> VarChunk
vcSt i des sym space = VC (nw $ nc i des) sym space

-- | Creates a VarChunk from an 'Idea', symbol, and space
vc' :: Idea c => c -> Symbol -> Space -> VarChunk
vc' n s t = VC (nw n) (\_ -> s) t

codeVC :: Idea c => c -> Symbol -> Space -> VarChunk
codeVC  n s t = VC (nw n) f t
  where
    f :: Stage -> Symbol
    f Implementation = s
    f Equational = Empty

-- | Creates a VarChunk from an 'Idea''s id and term and symbol
vc'' :: Idea c => c -> Symbol -> Space -> VarChunk
vc'' n sy space = vc (n ^. id) (n ^. term) sy space

-- | Creates a VarChunk from an id, term, symbol, and space
makeVCObj :: String -> NP -> Symbol -> String -> VarChunk
makeVCObj i des sym s = VC (nw $ nc i des) (\_ -> sym) (Obj s)
