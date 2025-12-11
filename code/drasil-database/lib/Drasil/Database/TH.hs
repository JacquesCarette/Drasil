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
declareHasChunkRefs = deriveGenerically [''HasChunkRefs]

-- | Generates:
--
-- 1. A 'Generic' instance for the type:
-- @
--   deriving stock instance Generic Ty
-- @
--
-- 2. For all type classes to be derived generically:
-- @
--   deriving via Generically Ty instance TheClass Ty
-- @
deriveGenerically :: [Name] -> Name -> Q [Dec]
deriveGenerically clss ty = do
  let typeCon = ConT ty

      -- deriving stock instance Generic Ty
      drvGeneric = StandaloneDerivD
        (Just StockStrategy)
        []
        (AppT (ConT ''Generic) typeCon)

      -- deriving via Generically Ty instance TheClass Ty
      drvCls cls = StandaloneDerivD
        (Just (ViaStrategy (AppT (ConT ''Generically) typeCon)))
        []
        (AppT (ConT cls) typeCon)

      -- Gather all classes we want to derive generically
      clsDrvs = map drvCls clss

  return $ drvGeneric : clsDrvs
