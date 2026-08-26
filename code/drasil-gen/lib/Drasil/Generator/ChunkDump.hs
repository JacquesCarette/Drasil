{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.ChunkDump (
  -- * Tools for dumping a chunk database
  buildDebuggingFiles
) where

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)

import Drasil.Database (dumpChunkDB, dumpChunkDeps)
import Drasil.FileHandling (FileLayout, PathSegment, file, ps)
import Drasil.System (systemdb, HasSystemMeta)

-- | Internal: For system debugging purposes, dump everything we can to a set of
-- files.
buildDebuggingFiles :: HasSystemMeta sys => sys -> [FileLayout]
buildDebuggingFiles si =
  [ dumpTo [ps|initial_chunks.json|] $ dumpChunkDB db
  , dumpTo [ps|initial_chunk_dependants.json|] dependants
  , dumpTo [ps|initial_chunk_dependencies.json|] dependencies
  -- FIXME: One more file containing the system meta-information.
  ]
  where
    db = si ^. systemdb
    (dependants, dependencies) = dumpChunkDeps db

-- | Internal: Build a JSON file from arbitrary data.
dumpTo :: ToJSON a => PathSegment -> a -> FileLayout
dumpTo targetPath = file targetPath . encodePretty
