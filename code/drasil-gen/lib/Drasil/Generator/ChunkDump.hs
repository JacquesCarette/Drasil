module Drasil.Generator.ChunkDump (
  -- * Tools for dumping a chunk database to disk for debugging
  dumpEverything
) where

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map.Strict as SM
import System.IO (IOMode(WriteMode), openFile, hClose, hPutStrLn)
import System.Environment (lookupEnv)
import Text.PrettyPrint (render)

import Language.Drasil.Printers (PrintingInformation, printAllDebugInfo)
import Utils.Drasil (createDirIfMissing)
import Drasil.Database (dumpChunkDB, invert)
import Drasil.System (System, systemdb, traceTable, refbyTable)

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
      chunkDump = dumpChunkDB chunkDb
      invertedChunkDump = invert chunkDump
      (sharedUIDs, _) = SM.partition atLeast2 invertedChunkDump
      traceDump = si ^. traceTable
      refByDump = si ^. refbyTable

  dumpTo chunkDump $ targetPath ++ "seeds.json"
  dumpTo invertedChunkDump $ targetPath ++ "inverted_seeds.json"
  dumpTo sharedUIDs $ targetPath ++ "problematic_seeds.json"
  dumpTo traceDump $ targetPath ++ "trace.json"
  dumpTo refByDump $ targetPath ++ "reverse_trace.json"

  dumpChunkTables si pinfo $ targetPath ++ "tables.txt"

atLeast2 :: [a] -> Bool
atLeast2 (_:_:_) = True
atLeast2 _       = False

-- FIXME: This is more of a general utility than it is drasil-database specific
dumpTo :: ToJSON a => a -> TargetFile -> IO ()
dumpTo d targetFile = do
  trg <- openFile targetFile WriteMode
  LB.hPutStrLn trg $ encodePretty d
  hClose trg

dumpChunkTables :: System -> PrintingInformation -> TargetFile -> IO ()
dumpChunkTables si pinfo targetFile = do
  trg <- openFile targetFile WriteMode
  mapM_ (hPutStrLn trg . render) $ printAllDebugInfo pinfo (si ^. refbyTable) (si ^. traceTable)
  hClose trg
