-- FIXME: Why is this a `Language` top-level module name?
module Language.Drasil.Dump where

import qualified Database.Drasil as DB
import SysInfo.Drasil (SystemInformation(_sysinfodb))

import System.Directory
import System.IO
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map.Strict as SM

import Utils.Drasil (invert, atLeast2)
import Database.Drasil (traceTable, refbyTable)
import Control.Lens ((^.))
import System.Environment (lookupEnv)

type Path = String
type TargetFile = String

-- | For debugging purposes, if the system has a `DEBUG_ENV` environment
--   variable set to anything, we can dump the chunk maps in a system to the
--   host system.
dumpEverything :: SystemInformation -> Path -> IO ()
dumpEverything si p = do
  maybeDebugging <- lookupEnv "DEBUG_ENV"
  case maybeDebugging of
    (Just (_:_)) -> do
      dumpEverything0 si p
    _ -> mempty

dumpEverything0 :: SystemInformation -> Path -> IO ()
dumpEverything0 si targetPath = do
  createDirectoryIfMissing True targetPath
  let chunkDb = _sysinfodb si
      chunkDump = DB.dumpChunkDB chunkDb
      invertedChunkDump = invert chunkDump
      sharedUIDs = SM.filter atLeast2 invertedChunkDump
      traceDump = chunkDb ^. traceTable
      refByDump = chunkDb ^. refbyTable

  dumpTo chunkDump $ targetPath ++ "seeds.json"
  dumpTo invertedChunkDump $ targetPath ++ "inverted_seeds.json"
  dumpTo sharedUIDs $ targetPath ++ "problematic_seeds.json"
  dumpTo traceDump $ targetPath ++ "trace.json"
  dumpTo refByDump $ targetPath ++ "reverse_trace.json"

-- FIXME: This is more of a general utility than it is drasil-database specific
dumpTo :: ToJSON a => a -> TargetFile -> IO ()
dumpTo d targetFile = do
  trg <- openFile targetFile WriteMode
  LB.hPutStrLn trg $ encodePretty d
  hClose trg
