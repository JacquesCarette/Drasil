{-# LANGUAGE TemplateHaskellQuotes #-}
module Drasil.Database.TH (
  -- * The Magic
  declareHasChunkRefs,
  -- * Re-exports from GHC.Generics for convenience
  Generic,
  Generically(..)
) where

import GHC.Generics (Generic, Generically(..))
import Language.Haskell.TH (Name, Q, Dec(..), Type(..), DerivStrategy(..))

import Drasil.Database.Chunk (HasChunkRefs)

-- | Declares that a type is a chunk type; Generates an instance of
-- 'HasChunkRefs'.
declareHasChunkRefs :: Name -> Q [Dec]
declareHasChunkRefs = deriveGenerically ''HasChunkRefs

-- TODO: Create a 'declareChunkType' that creates a `HasUID` instance along with
-- the `HasChunkRefs` instance.

-- | Generates:
--   deriving stock instance Generic Ty
--   deriving via Generically Ty instance TheClass Ty
deriveGenerically :: Name -> Name -> Q [Dec]
deriveGenerically cls ty = do
  let typeCon = ConT ty

  return
    [ -- deriving stock instance Generic Ty
      StandaloneDerivD (Just StockStrategy) [] (AppT (ConT ''Generic) typeCon)
      -- deriving via Generically Ty instance TheClass Ty
    , StandaloneDerivD
        (Just (ViaStrategy (AppT (ConT ''Generically) typeCon)))
        []
        (AppT (ConT cls) typeCon)
    ]
