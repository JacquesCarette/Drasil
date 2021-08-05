module Language.Drasil.Log.Print where

import Language.Drasil
import Database.Drasil
import System.IO
import System.Directory
import qualified Data.Map as Map

{-
Want to go from SystemInformation to Log files.
We can start by just listing all of the UIDs in a chunk database.

-}
printAllChunkUIDs :: SystemInformation -> IO ()
printAllChunkUIDs SI{_sys = sys, _sysinfodb = db} = do
  createDirectoryIfMissing "SRSlogs" False
  setCurrentDirectory "SRSlogs"
  handle <- openFile (abrv sys ++ "_SRS.log") WriteMode
  hPutStrLn handle $ "List of all Chunk UIDs in" ++ abrv sys
  let listUIDs = mkListAll db
  map (hPutStrLn handle) listUIDs
  hClose handle

mkListAll :: ChunkDB -> [UID]
mkListAll db = concatMap ((^. uid) . fst . Map.assocs . ($ db)) [symbolTable, termTable, defTable, unitTable, traceTable, refbyTable, dataDefnTable, insmodelTable, gendefTable, theoryModelTable, conceptinsTable, sectionTable, labelledcontentTable, refTable]




