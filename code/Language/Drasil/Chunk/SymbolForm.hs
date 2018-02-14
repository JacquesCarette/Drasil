{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk.SymbolForm 
  (SymbolForm(..), SymbolChunk, sc, ssc
  , Stage(..), ssc', StagedSymbolChunk, getSymbForStage, ssc''
  , hasStageSymbol) where

import Language.Drasil.Chunk
import Control.Lens (Simple, Lens, (^.))

import Language.Drasil.Symbol

import qualified Data.Map as Map

import Prelude hiding (id)

-- | A SymbolForm is a 'Chunk' with a symbol that represents it
class Chunk c => SymbolForm c where
  symbol :: Simple Lens c Symbol
  
-- | Symbols should be kept in chunks, so that we can remove the SymbolForm
-- instance from non-symbols.
data SymbolChunk = SC String Symbol

instance Chunk SymbolChunk where
  id f (SC i s) = fmap (\x -> SC x s) (f i)
instance Eq SymbolChunk where 
  -- This is the only case where we don't only match ids
  a == b = ((a ^. id) == (b ^. id)) || ((a ^. symbol) == (b ^. symbol))
instance SymbolForm SymbolChunk where
  symbol f (SC i s) = fmap (\x -> SC i x) (f s)

-- | Smart constructor for chunks for symbols
sc :: String -> Symbol -> SymbolChunk
sc = SC

-- | Staged Symbol Chunks are mini-maps from a stage of generation to the 
-- appropriate symbol representation. They are not (in-themselves) symbols!
-- They contain symbols.
data StagedSymbolChunk = SSC String (Map.Map Stage SymbolChunk)
instance Chunk StagedSymbolChunk where
  id f (SSC i stageMap) = fmap (\x -> SSC x stageMap) (f i)
instance Eq StagedSymbolChunk where
  a == b = ((a ^. id) == (b ^. id))

-- | Smart constructor for StagedSymbolChunks.
-- Assumes symbols are in order of stage precedence.
-- FIXME? Record of symbols instead?
ssc :: String -> Symbol -> Symbol -> StagedSymbolChunk
ssc i s1 s2 = SSC i (Map.fromList [(Equational, symC s1), (Implementation, symC s2)])
  where symC = sc i

-- | Alternate smart constructor for StagedSymbolChunks.
-- Assumes one symbol used at all stages
ssc' :: String -> Symbol -> StagedSymbolChunk
ssc' i s = SSC i (Map.fromList [(Equational, symC), (Implementation, symC)])
  where symC = sc i s

ssc'' :: String -> [(Stage, Symbol)] -> StagedSymbolChunk
ssc'' i ss = SSC i (Map.fromList ss')
  where ss' = map (\(a,b) -> (a, sc i b)) ss
-- FIXME: More fine-grained stages.
-- | Stages correspond to what we're trying to look up. They range from abstract
-- to concrete.                  
data Stage = Equational -- AKA Theoretical / Abstract-design
           | Implementation -- AKA Implementation / Detailed-design
  deriving (Eq, Ord)

{- Note: Keep stages separate from StagedSymbols for lookup purposes, as we may
-- have documents which look up both stages of a symbol and show them 
-- side-by-side or one after another (think LPM). -}

-- | For better error messages.
instance Show Stage where
  show Equational     = "Theoretical stage"
  show Implementation = "Implementation Stage"

hasStageSymbol :: Stage -> StagedSymbolChunk -> Bool
hasStageSymbol st (SSC _ sm) = Map.member st sm
  
getSymbForStage :: Stage -> StagedSymbolChunk -> SymbolChunk
getSymbForStage st (SSC i sm) = let lookC = Map.lookup st sm in
  getS lookC
  where getS (Just x) = x
        getS Nothing = error ("Missing symbol for " ++ i ++ " chunk at " ++ show st)
