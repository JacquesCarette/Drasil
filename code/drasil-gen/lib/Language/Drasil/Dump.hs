-- FIXME: Why is this a `Language` top-level module name?
module Language.Drasil.Dump where

import qualified Database.Drasil as DB
import System.Drasil (System, systemdb)

import System.IO
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map.Strict as SM

import Utils.Drasil (invert, atLeast2, createDirIfMissing)
import Database.Drasil (traceTable, refbyTable, ChunkDB (termTable))
import Control.Lens ((^.))
import System.Environment (lookupEnv)

import Language.Drasil.Printers (PrintingInformation, printAllDebugInfo)
import Text.PrettyPrint

type Path = String
type TargetFile = String

-- | For debugging purposes, if the system has a `DEBUG_ENV` environment
--   variable set to anything, we can dump the chunk maps in a system to the
--   host system.
dumpEverything :: System -> PrintingInformation -> Path -> IO ()
dumpEverything si pinfo p = do
  maybeDebugging <- lookupEnv "DEBUG_ENV"
  case maybeDebugging of
    (Just (_:_)) -> do
      dumpEverything0 si pinfo p
    _ -> mempty

dumpEverything0 :: System -> PrintingInformation -> Path -> IO ()
dumpEverything0 si pinfo targetPath = do
  createDirIfMissing True targetPath
  let chunkDb = si ^. systemdb
      chunkDump = DB.dumpChunkDB chunkDb
      invertedChunkDump = invert chunkDump
      (sharedUIDs, nonsharedUIDs) = SM.partition atLeast2 invertedChunkDump
      traceDump = chunkDb ^. traceTable
      refByDump = chunkDb ^. refbyTable
      justTerms = SM.intersection nonsharedUIDs $ termTable chunkDb

  dumpTo chunkDump $ targetPath ++ "seeds.json"
  dumpTo invertedChunkDump $ targetPath ++ "inverted_seeds.json"
  dumpTo justTerms $ targetPath ++ "uids_are_just_terms.json"
  dumpTo sharedUIDs $ targetPath ++ "problematic_seeds.json"
  dumpTo traceDump $ targetPath ++ "trace.json"
  dumpTo refByDump $ targetPath ++ "reverse_trace.json"

  dumpChunkTables pinfo $ targetPath ++ "tables.txt"

-- FIXME: This is more of a general utility than it is drasil-database specific
dumpTo :: ToJSON a => a -> TargetFile -> IO ()
dumpTo d targetFile = do
  trg <- openFile targetFile WriteMode
  LB.hPutStrLn trg $ encodePretty d
  hClose trg

dumpChunkTables :: PrintingInformation -> TargetFile -> IO ()
dumpChunkTables pinfo targetFile = do
  trg <- openFile targetFile WriteMode
  mapM_ (hPutStrLn trg . render) $ printAllDebugInfo pinfo
  hClose trg
