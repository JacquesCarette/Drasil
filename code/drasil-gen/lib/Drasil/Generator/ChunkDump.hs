{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.ChunkDump (
  -- * Tools for dumping a chunk database
  buildDebugData
) where

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import System.Environment (lookupEnv)

import Drasil.Database (dumpChunkDB, dumpChunkDeps)
import Drasil.FileHandling (FileLayout, PathSegment, directory, file, ps)
import Drasil.System (systemdb, HasSystemMeta)

-- | Builds the `.drasil` chunk dump directory if the `DEBUG_ENV` environment
-- variable is non-empty.
buildDebugData :: HasSystemMeta sys => sys -> IO (Maybe FileLayout)
buildDebugData si = do
  maybeDebugging <- lookupEnv "DEBUG_ENV"
  case maybeDebugging of
    (Just (_:_)) -> pure $ Just $ dumpEverything si
    _ -> pure Nothing

-- | Internal: For debugging purposes, constructs a `FileLayout` with a dump of
-- the chunk maps.
dumpEverything :: HasSystemMeta sys => sys -> FileLayout
dumpEverything si =
  directory [ps|.drasil|]
  [ dumpTo [ps|initial_chunks.json|] $ dumpChunkDB db
  , dumpTo [ps|initial_chunk_dependants.json|] dependants
  , dumpTo [ps|initial_chunk_dependencies.json|] dependencies
  ]
  where
    db = si ^. systemdb
    (dependants, dependencies) = dumpChunkDeps db

-- | Internal: Build a JSON file from arbitrary data.
dumpTo :: ToJSON a => PathSegment -> a -> FileLayout
dumpTo targetPath = file targetPath . encodePretty
