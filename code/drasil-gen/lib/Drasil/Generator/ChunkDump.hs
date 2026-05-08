module Drasil.Generator.ChunkDump (
  -- * Tools for dumping a chunk database to disk for debugging
  dumpEverything
) where

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB
import System.IO (IOMode(WriteMode), openFile, hClose)
import System.Environment (lookupEnv)

import Drasil.Build.Artifacts.Legacy (createDirIfMissing)
import Drasil.Database (dumpChunkDB)
import Drasil.System (SmithEtAlSRS, systemdb, traceTable, refbyTable)

type Path = String
type TargetFile = String

-- | For debugging purposes, if the system has a `DEBUG_ENV` environment
-- variable set to anything, we can dump the chunk maps in a system to the host
-- system.
dumpEverything :: SmithEtAlSRS -> Path -> IO ()
dumpEverything si p = do
  maybeDebugging <- lookupEnv "DEBUG_ENV"
  case maybeDebugging of
    (Just (_:_)) -> do
      dumpEverything0 si p
    _ -> mempty

dumpEverything0 :: SmithEtAlSRS -> Path -> IO ()
dumpEverything0 si targetPath = do
  createDirIfMissing True targetPath
  let chunkDb = si ^. systemdb
      chunkDump = dumpChunkDB chunkDb
      traceDump = si ^. traceTable
      refByDump = si ^. refbyTable

  dumpTo chunkDump $ targetPath ++ "seeds.json"
  dumpTo traceDump $ targetPath ++ "trace.json"
  dumpTo refByDump $ targetPath ++ "reverse_trace.json"

-- FIXME: This is more of a general utility than it is drasil-database specific
dumpTo :: ToJSON a => a -> TargetFile -> IO ()
dumpTo d targetFile = do
  trg <- openFile targetFile WriteMode
  LB.hPutStrLn trg $ encodePretty d
  hClose trg
