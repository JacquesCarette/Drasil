{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.ChunkDump (
  -- * Tools for dumping a chunk database
  buildDebugData
) where

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB
import System.Environment (lookupEnv)

import Drasil.Database (dumpChunkDB)
import Drasil.FileHandling (FileLayout, PathSegment, directory, file, ps)
import Drasil.System (SmithEtAlSRS, systemdb, traceTable, refbyTable)

-- | Builds the `.drasil` chunk dump directory if the `DEBUG_ENV` environment
-- variable is non-empty.
buildDebugData :: SmithEtAlSRS -> IO (Maybe FileLayout)
buildDebugData si = do
  maybeDebugging <- lookupEnv "DEBUG_ENV"
  case maybeDebugging of
    (Just (_:_)) -> pure $ Just $ dumpEverything si
    _ -> pure Nothing

-- | Internal: For debugging purposes, constructs a `FileLayout` with a dump of
-- the chunk maps.
dumpEverything :: SmithEtAlSRS -> FileLayout
dumpEverything si =
  directory [ps|.drasil|]
  [ dumpTo [ps|seeds.json|] $ dumpChunkDB (si ^. systemdb),
    dumpTo [ps|trace.json|] $ si ^. traceTable,
    dumpTo [ps|reverse_trace.json|] $ si ^. refbyTable
  ]

-- | Internal: Build a JSON file from arbitrary data.
dumpTo :: ToJSON a => PathSegment -> a -> FileLayout
dumpTo targetPath = file targetPath . LB.unpack . encodePretty
