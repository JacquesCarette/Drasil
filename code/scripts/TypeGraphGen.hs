-- FIXME: use real parser (Low Priority; see line 119)
module TypeGraphGen (main) where

import Data.List
import Data.Maybe
import System.IO
import System.Directory
import System.FilePath (takeDirectory)
import Control.Monad
import qualified Data.Map as Map
import qualified DirectoryController as DC (createFolder, createFile, finder, 
  getDirectories, DrasilPack, FileName, FolderName, File(..), Folder(..))
import SourceCodeReaderTypes as SCRT (extractEntryData, EntryData(..), 
  DataDeclRecord(..), DataDeclConstruct(..), NewtypeDecl(..), TypeDecl(..))
import Data.List.Split (splitOn)
import Data.Char (toLower)

type EntryString = String

-- Entry data type for storing entry data for each file entry
data Entry = Entry { drasilPack :: DC.DrasilPack
                   , fileName :: DC.FileName -- change to entryName?
                   , filePath :: FilePath -- change to entryPath?
                   , dataTypeRecords :: [DataDeclRecord]
                   , dataTypeConstructors :: [DataDeclConstruct]
                   , newtypes :: [NewtypeDecl]
                   , types :: [TypeDecl]
                   } deriving (Show)

-- main controller function; initiates function calls to generate output file
main :: IO ()
main = do
  -- directory variables (for scripts, code and output directories)
  scriptsDirectory <- getCurrentDirectory
  -- obtains code directory and output directory filepaths
  let codeDirectory = takeDirectory scriptsDirectory
      outputDirectory = codeDirectory ++ "/analysis"

  -- gets names + filepaths of all drasil- packages/directories
  drctyList <- DC.getDirectories codeDirectory "drasil-"

  -- imports configuration settings (drasil- package names + class types order)
  packageNames <- config scriptsDirectory
  
  -- uses ordering (imported from config file) iff imported ordering is complete
  let ordered
        | ldL == lpN = map (getFolder drctyDict) packageNames
        | otherwise = drctyList
      (ldL,lpN) = (length drctyList,length packageNames)
      -- creates dictionary (Map.Map format) from drasil- directories list
      drctyDict = Map.fromList $ map toDictList drctyList

  -- all files + filepaths stored here; obtained from ordered list using finder
  allFiles <- mapM DC.finder ordered

  -- adds newline File entries (to separate Files by drasil- package)
  --let rawFileData = intercalate [DC.createFile "" "" "newline"] allFiles

  -- creates Entry instances (w/ file data) from File instances (File -> Entry)
  rawEntryData <- zipWithM (createEntry codeDirectory) (concat allFiles) (map DC.fileName (concat allFiles))

  --let ordrdClasses = ordClasses classInstOrd rawEntryData
   --   ordrdClassNames = map className ordrdClasses

  -- creates EntryString instances containing entry data
  --bakedEntryData <- zipWithM (compileEntryData ordrdClassNames) rawEntryData (map fileName rawEntryData)
  -- contains joined string with each file EntryString ("\n" separated)
  --let entryData = intercalate "\n" bakedEntryData

  -- creates and writes to output data file
  mapM (output outputDirectory) rawEntryData
  return mempty

output :: FilePath -> Entry -> IO ()
output fp entry = do
    let macroOutputNames = map ddrName (dataTypeRecords entry) ++ map ddcName (dataTypeConstructors entry) ++ map ntdName (newtypes entry) ++ map tdName (types entry)
    mkMacroOutput fp macroOutputNames (drasilPack entry)
    mapM (mkGraphDROutput fp) $ dataTypeRecords entry
    mapM (mkGraphDCOutput fp) $ dataTypeConstructors entry
    mapM (mkGraphNTOutput fp) $ newtypes entry
    mapM (mkGraphTOutput fp) $ types entry
    return mempty



---
--Output sections
---

mkMacroOutput :: FilePath -> [String] -> String -> IO ()
mkMacroOutput outputFilePath nms nm = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (nm ++ ".dot") WriteMode
    hPutStrLn typeGraph "digraph alltypes {"
    hPutStrLn typeGraph "define(`digraph', `subgraph')"
    mapM (hPutStrLn typeGraph) $ map (\x -> "sinclude(" ++ x ++ ".dot)") nms
    hPutStrLn typeGraph "}"
    hClose typeGraph

--data types that are records
mkGraphDROutput :: FilePath -> SCRT.DataDeclRecord -> IO ()
mkGraphDROutput outputFilePath ddr = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (ddrName ddr ++ ".dot") WriteMode
    hPutStrLn typeGraph $ "digraph " ++ map toLower (ddrName ddr) ++ "{"
    mapM (hPutStrLn typeGraph) $ getEdgesDR (ddrName ddr) (ddrContent ddr)
    hPutStrLn typeGraph $ getNodesDR $ ddrName ddr
    hPutStrLn typeGraph "}"
    hClose typeGraph

getEdgesDR :: String -> [String] -> [String]
getEdgesDR _ [] = []
getEdgesDR nm (c:cs) = (c ++ " -> " ++ nm ++ ";"): getEdgesDR nm cs

getNodesDR :: String -> String
getNodesDR nm = "[shape=oval, color=cyan3, label=" ++ nm ++ "];"

-- data types that use constructors
mkGraphDCOutput :: FilePath -> SCRT.DataDeclConstruct -> IO ()
mkGraphDCOutput outputFilePath ddc = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (ddcName ddc ++ ".dot") WriteMode
    hPutStrLn typeGraph $ "digraph " ++ map toLower (ddcName ddc) ++ "{"
    mapM (hPutStrLn typeGraph) $ getEdgesDC (ddcName ddc) (ddcContent ddc) (if length (ddcContent ddc) == 1 then True else False)
    hPutStrLn typeGraph $ getNodesDC $ ddcName ddc
    hPutStrLn typeGraph "}"
    hClose typeGraph

-- This boolean checks if a datatype has only one value it can be made of. If True, there is only one value
-- so we treat it like a record with a single field. If false, use dotted line to show the possibility of one type going into the next.
getEdgesDC :: String -> [String] -> Bool -> [String]
getEdgesDC _ [] _ = []
getEdgesDC nm (c:cs) s 
    | s = (c ++ " -> " ++ nm ++ ";") : getEdgesDC nm cs s
    | otherwise =  (c ++ " -> " ++ nm ++ " [style=\"dotted\"];") : getEdgesDC nm cs s

getNodesDC :: String -> String
getNodesDC nm = "[shape=oval, color=darkviolet, label=" ++ nm ++ "];"

-- for types that use newtype as a constructor
mkGraphNTOutput :: FilePath -> SCRT.NewtypeDecl -> IO ()
mkGraphNTOutput outputFilePath ntd = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (ntdName ntd ++ ".dot") WriteMode
    hPutStrLn typeGraph $ "digraph " ++ map toLower (ntdName ntd) ++ "{"
    mapM (hPutStrLn typeGraph) $ getEdgesNT (ntdName ntd) (ntdContent ntd)
    hPutStrLn typeGraph $ getNodesNT $ ntdName ntd
    hPutStrLn typeGraph "}"
    hClose typeGraph

getEdgesNT :: String -> [String] -> [String]
getEdgesNT _ [] = []
getEdgesNT nm (c:cs) = (c ++ " -> " ++ nm ++ ";") : getEdgesNT nm cs

getNodesNT :: String -> String
getNodesNT nm = "[shape=oval, color=darkgreen, label=" ++ nm ++ "];"

-- for types that use type as a constructor
mkGraphTOutput :: FilePath -> SCRT.TypeDecl -> IO ()
mkGraphTOutput outputFilePath td = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (tdName td ++ ".dot") WriteMode
    hPutStrLn typeGraph $ "digraph " ++ map toLower (tdName td) ++ "{"
    mapM (hPutStrLn typeGraph) $ getEdgesT (tdName td) (tdContent td)
    hPutStrLn typeGraph $ getNodesT $ tdName td
    hPutStrLn typeGraph "}"
    hClose typeGraph

getEdgesT :: String -> [String] -> [String]
getEdgesT _ [] = []
getEdgesT nm (c:cs) = (c ++ " -> " ++ nm ++ ";") : getEdgesT nm cs

getNodesT :: String -> String
getNodesT nm = "[shape=oval, color=red2, label=" ++ nm ++ "];"


-- makes Entry data instance
makeEntry :: DC.DrasilPack -> DC.FileName -> FilePath -> [DataDeclRecord] -> 
    [DataDeclConstruct] -> [NewtypeDecl] -> [TypeDecl] -> Entry
makeEntry drpk fn fp dtR dtC ntd td = Entry {drasilPack=drpk,fileName=fn,
  filePath=fp,dataTypeRecords=dtR, dataTypeConstructors=dtC, newtypes=ntd, types=td}

-- import configurations function (drasil- packages)
config :: FilePath -> IO ([String])
config configFilePath = do
  setCurrentDirectory configFilePath
  c <- readFile "DTG_Config.txt"
  let l = map words $ filter isInfoLine (lines c)
      -- FIXME: use real parser to extract settings from config file
      -- will improve error messages regarding config file settings
      packageNames = head l
  return packageNames

-- creates an entry for each file (new Entry data-oriented format)
createEntry :: FilePath -> DC.File -> DC.FileName -> IO Entry
-- creates actual file entries
createEntry homeDirectory file filename = do
  let drpk = DC.fileDrasilPack file
      fn = filename
      fp = DC.filePath file
      -- entry file path is stripped of home directory and drasil- pack
      efp = (++ "/") $ (\\ homeDirectory ++ "/drasil-" ++ drpk ++ "/") fp

  -- extracts entry data from File data type
  rEntryData <- SCRT.extractEntryData fn fp
  
  let dataTypeDeclR = SCRT.dRNs rEntryData
      dataTypeDeclC = SCRT.dCNs rEntryData
      newtypeDecl = SCRT.ntNs rEntryData
      typeDecl = SCRT.tNs rEntryData

  let entry = makeEntry drpk fn efp dataTypeDeclR dataTypeDeclC newtypeDecl typeDecl
  return entry

-- used to filter out info lines (i.e. removes comment and empty lines)
isInfoLine :: String -> Bool
isInfoLine line = (line /="") && not ("#" `isPrefixOf` line)

-- gets folder from dictionary using folder name (iff it exists in dictionary)
getFolder :: Map.Map DC.FolderName DC.Folder -> DC.FolderName -> DC.Folder
getFolder dict name = fromJust $ Map.lookup name dict

-- converts list to dictionary list format (for use by drasil- directories only)
toDictList :: DC.Folder -> (DC.FolderName,DC.Folder)
toDictList folder = (DC.folderDrasilPack folder,folder) 
