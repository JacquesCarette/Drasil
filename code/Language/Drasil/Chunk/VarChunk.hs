{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.VarChunk(VarChunk,implVar,codeVC,vc,vcSt,vc'') where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), HasSpace(typ), HasAttributes(attributes))
import Language.Drasil.Chunk.NamedIdea (IdeaDict, nw, nc)
import Language.Drasil.Chunk.Quantity (Quantity(getUnit))
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Symbol (Symbol(Empty),Stage(..))
import Language.Drasil.Space (Space)

import Language.Drasil.NounPhrase (NP)

import Control.Lens ((^.), makeLenses, view)
  
-- | VarChunks are Quantities that have symbols, but not units.
data VarChunk = VC { _ni :: IdeaDict
                   , _vsymb :: Stage -> Symbol
                   , _vtyp  :: Space
                   , _attribs :: Attributes -- FIXME: Attributes included for consistency,
                                            -- since every chunk should eventually have the
                                            -- capability for attributes.
                   }
makeLenses ''VarChunk

instance Eq            VarChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID        VarChunk where uid = ni . uid
instance NamedIdea     VarChunk where term = ni . term
instance Idea          VarChunk where getA = getA . view ni
instance HasSymbol     VarChunk where symbol x = (x ^. vsymb)
instance HasSpace      VarChunk where typ = vtyp
instance Quantity      VarChunk where getUnit _  = Nothing
instance HasAttributes VarChunk where attributes = attribs

-- | implVar makes an variable that is implementation-only
implVar :: String -> NP -> Symbol -> Space -> Attributes -> VarChunk
implVar i des sym ty atts = vcSt i des f ty atts
  where
    f :: Stage -> Symbol
    f Implementation = sym
    f Equational = Empty

-- | Creates a VarChunk from an uid, term, symbol, and space
vc :: String -> NP -> Symbol -> Space -> Attributes -> VarChunk
vc i des sym space atts = VC (nw $ nc i des) (\_ -> sym) space atts

-- | Like cv, but creates a VarChunk from something that knows about stages
vcSt :: String -> NP -> (Stage -> Symbol) -> Space -> Attributes -> VarChunk
vcSt i des sym space atts = VC (nw $ nc i des) sym space atts

codeVC :: Idea c => c -> Symbol -> Space -> Attributes -> VarChunk
codeVC n s t atts = VC (nw n) f t atts
  where
    f :: Stage -> Symbol
    f Implementation = s
    f Equational = Empty

-- | Creates a VarChunk from an 'Idea', symbol and space
vc'' :: Idea c => c -> Symbol -> Space -> Attributes -> VarChunk
vc'' n sy space atts = vc (n ^. uid) (n ^. term) sy space atts
