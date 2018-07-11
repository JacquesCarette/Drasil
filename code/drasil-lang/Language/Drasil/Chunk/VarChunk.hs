{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.VarChunk(VarChunk,implVar,codeVC,vc,vcSt,vc'') where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), HasSpace(typ))
import Language.Drasil.Chunk.NamedIdea (IdeaDict, nw, nc)
import Language.Drasil.Chunk.Quantity (Quantity(getUnit))
import Language.Drasil.Symbol (Symbol(Empty), Stage(..))
import Language.Drasil.Space (Space)

import Language.Drasil.NounPhrase (NP)

import Control.Lens ((^.), makeLenses, view)
  
-- | VarChunks are Quantities that have symbols, but not units.
data VarChunk = VC { _ni :: IdeaDict
                   , _vsymb :: Stage -> Symbol
                   , _vtyp  :: Space
                   }
makeLenses ''VarChunk

instance Eq            VarChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        VarChunk where uid = ni . uid
instance NamedIdea     VarChunk where term = ni . term
instance Idea          VarChunk where getA = getA . view ni
instance HasSymbol     VarChunk where symbol = (^. vsymb)
instance HasSpace      VarChunk where typ = vtyp
instance Quantity      VarChunk where getUnit _  = Nothing

-- | implVar makes an variable that is implementation-only
implVar :: String -> NP -> Symbol -> Space -> VarChunk
implVar i des sym ty = vcSt i des f ty
  where
    f :: Stage -> Symbol
    f Implementation = sym
    f Equational = Empty

-- | Creates a VarChunk from an uid, term, symbol, and space
vc :: String -> NP -> Symbol -> Space -> VarChunk
vc i des sym space = VC (nw $ nc i des) (\_ -> sym) space

-- | Like cv, but creates a VarChunk from something that knows about stages
vcSt :: String -> NP -> (Stage -> Symbol) -> Space -> VarChunk
vcSt i des sym space = VC (nw $ nc i des) sym space

codeVC :: Idea c => c -> Symbol -> Space -> VarChunk
codeVC n s t = VC (nw n) f t
  where
    f :: Stage -> Symbol
    f Implementation = s
    f Equational = Empty

-- | Creates a VarChunk from an 'Idea', symbol and space
vc'' :: Idea c => c -> Symbol -> Space -> VarChunk
vc'' n sy space = vc (n ^. uid) (n ^. term) sy space
