-- FIXME: Why is this a `Language` top-level module name?
module Language.Drasil.Dump where

import qualified Database.Drasil as DB
import SysInfo.Drasil (SystemInformation(SI))

import System.Directory
import System.IO
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB

type Path = String
type TargetFile = String

dumpEverything :: SystemInformation -> String -> IO ()
dumpEverything (SI _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ chks _ _) targetPath = do
    createDirectoryIfMissing True targetPath
    dumpChunkDB chks $ targetPath ++ "seeds.json"
    -- TODO: Dump more things!!! DIFFS! OVERLAPPING UIDS!

dumpChunkDB :: DB.ChunkDB -> String -> IO ()
dumpChunkDB chks targetFile = do
    let dumpped = DB.dumpChunkDB chks
    trg <- openFile targetFile WriteMode
    LB.hPutStrLn trg $ encode $ dumpped
    hClose trg
