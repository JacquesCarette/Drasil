{-# LANGUAGE QuasiQuotes #-}

module Drasil.Generator.ChunkDump (
  -- * Tools for dumping a chunk database to disk for debugging
  dumpEverything
) where

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB
import Text.PrettyPrint.HughesPJ (Doc, text)

import Drasil.Build.Artifacts (FileLayout, PathSegment, directory, file, ps)
import Drasil.Database (dumpChunkDB)
import Drasil.System (SmithEtAlSRS, systemdb, traceTable, refbyTable)

-- | For debugging purposes, constructs a `FileLayout` with a dump of the
-- chunk maps.
dumpEverything :: SmithEtAlSRS -> FileLayout Doc
dumpEverything si =
  directory [ps|.drasil|]
  [
    dumpTo chunkDump [ps|seeds.json|],
    dumpTo traceDump [ps|trace.json|],
    dumpTo refByDump [ps|reverse_trace.json|]
  ]
  where chunkDb = si ^. systemdb
        chunkDump = dumpChunkDB chunkDb
        traceDump = si ^. traceTable
        refByDump = si ^. refbyTable

-- FIXME: This is more of a general utility than it is drasil-database specific
dumpTo :: ToJSON a => a -> PathSegment -> FileLayout Doc
dumpTo d targetPath =
  file targetPath (text $ LB.unpack $ encodePretty d)
