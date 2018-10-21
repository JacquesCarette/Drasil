{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.VarChunk(VarChunk) where

import Language.Drasil.Chunk.NamedIdea (IdeaDict, nw, nc)
import Language.Drasil.Chunk.Quantity (Quantity)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  HasSymbol(symbol), HasSpace(typ))
import Language.Drasil.Development.Unit (MayHaveUnit(getUnit))
import Language.Drasil.Symbol (Symbol(Empty))
import Language.Drasil.Stages (Stage(..))
import Language.Drasil.Space (Space)

import Language.Drasil.NounPhrase (NP)

import Control.Lens ((^.), makeLenses, view)
  
-- | VarChunks are Quantities that have symbols, but not units.
data VarChunk = VC { _ni :: IdeaDict
                   , _vsymb :: Stage -> Symbol
                   , _vtyp  :: Space
                   }
makeLenses ''VarChunk

instance Eq          VarChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance HasUID      VarChunk where uid = ni . uid
instance NamedIdea   VarChunk where term = ni . term
instance Idea        VarChunk where getA = getA . view ni
instance HasSymbol   VarChunk where symbol = (^. vsymb)
instance HasSpace    VarChunk where typ = vtyp
instance Quantity    VarChunk where 
instance MayHaveUnit VarChunk where getUnit _  = Nothing

