-- FIXME: Why is this a `Language` top-level module name?
module Language.Drasil.Dump where

import qualified Database.Drasil as DB
import SysInfo.Drasil (SystemInformation(_sysinfodb))

import System.Directory
import System.IO
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map.Strict as SM

import Utils.Drasil (invert)

type Path = String
type TargetFile = String

dumpEverything :: SystemInformation -> Path -> IO ()
dumpEverything si targetPath = do
  createDirectoryIfMissing True targetPath
  let chunkDump = DB.dumpChunkDB $ _sysinfodb si
  let invertedChunkDump = invert chunkDump
  let sharedUIDs = SM.filter (\x -> length x > 1) invertedChunkDump

  dumpTo chunkDump $ targetPath ++ "seeds.json"
  dumpTo invertedChunkDump $ targetPath ++ "inverted_seeds.json"
  dumpTo sharedUIDs $ targetPath ++ "problematic_seeds.json"

-- FIXME: This is more of a general utility than it is drasil-database specific
dumpTo :: ToJSON a => a -> TargetFile -> IO ()
dumpTo d targetFile = do
  trg <- openFile targetFile WriteMode
  LB.hPutStrLn trg $ encodePretty d
  hClose trg
