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
type Colour = String

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
      outputDirectory = codeDirectory ++ "/analysis/TypeDependencyGraphs"

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

  -- creates Entry instances (w/ file data) from File instances (File -> Entry)
  rawEntryData <- zipWithM (createEntry codeDirectory) (concat allFiles) (map DC.fileName (concat allFiles))

  -- creates and writes to output data file
  mapM_ (createPackageFiles outputDirectory) packageNames
  mapM_ (output outputDirectory) rawEntryData
  mapM_ (closePackageFiles outputDirectory) packageNames
  mkDrasilTypeGraph outputDirectory rawEntryData
  return mempty

----------
-- Main output functions
----------

-- Creates a type graph for all files and types within drasil.
mkDrasilTypeGraph :: FilePath -> [Entry] -> IO ()
mkDrasilTypeGraph fp entries = do
  createDirectoryIfMissing False fp
  setCurrentDirectory fp
  typeGraph <- openFile "drasil.dot" AppendMode
  hPutStrLn typeGraph "digraph alltypes {"
  mapM_ (mkFullOutputSub typeGraph) entries
  hPutStrLn typeGraph "}"
  hClose typeGraph

-- Helper to create drasil- package files
createPackageFiles :: FilePath -> String -> IO()
createPackageFiles fp lang = do
  createDirectoryIfMissing False fp
  setCurrentDirectory fp
  typeGraph <- openFile (lang ++ ".dot") WriteMode
  hPutStrLn typeGraph "digraph alltypes {"
  hClose typeGraph

-- Helper to close drasil- package files
closePackageFiles :: FilePath -> String -> IO()
closePackageFiles fp lang = do
  createDirectoryIfMissing False fp
  setCurrentDirectory fp
  typeGraph <- openFile (lang ++ ".dot") AppendMode
  hPutStrLn typeGraph "}"
  hClose typeGraph

-- output function creates all type graphs except for drasil.dot
output :: FilePath -> Entry -> IO ()
output fp entry = do
    mkFullOutput fp entry
    mapM_ (mkGraphDROutput (fp ++ "/" ++ drasilPack entry)) $ dataTypeRecords entry
    mapM_ (mkGraphDCOutput (fp ++ "/" ++ drasilPack entry)) $ dataTypeConstructors entry
    mapM_ (mkGraphNTOutput (fp ++ "/" ++ drasilPack entry)) $ newtypes entry
    mapM_ (mkGraphTOutput  (fp ++ "/" ++ drasilPack entry)) $ types entry
    return mempty

-------
--Output sections
-------

-- Helper to create package-wide type graphs
mkFullOutput :: FilePath -> Entry -> IO ()
mkFullOutput outputFilePath entry = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (drasilPack entry ++ ".dot") AppendMode
    hPutStrLn typeGraph $ "\tsubgraph " ++ map toLower (fileName entry \\ ".hs") ++ " {"
    mapM_ (mkGraphDROutputSub typeGraph) $ dataTypeRecords entry
    mapM_ (mkGraphDCOutputSub typeGraph) $ dataTypeConstructors entry
    mapM_ (mkGraphNTOutputSub typeGraph) $ newtypes entry
    mapM_ (mkGraphTOutputSub  typeGraph) $ types entry
    hPutStrLn typeGraph "\t}"
    hClose typeGraph


-- Helper to make a graph from datatypes that use the @data@ syntax and are records
mkGraphDROutput :: FilePath -> SCRT.DataDeclRecord -> IO ()
mkGraphDROutput outputFilePath ddr = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (ddrName ddr ++ ".dot") WriteMode
    hPutStrLn typeGraph $ "digraph " ++ map toLower (ddrName ddr) ++ "{"
    mapM_ (hPutStrLn typeGraph) $ makeEdgesDi (ddrName ddr) (ddrContent ddr)
    hPutStrLn typeGraph $ makeNodesDi "cyan3" $ ddrName ddr
    hPutStrLn typeGraph "}"
    hClose typeGraph

-- Helper to make a graph from datatypes that use the @data@ syntax and are not records
mkGraphDCOutput :: FilePath -> SCRT.DataDeclConstruct -> IO ()
mkGraphDCOutput outputFilePath ddc = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (ddcName ddc ++ ".dot") WriteMode
    hPutStrLn typeGraph $ "digraph " ++ map toLower (ddcName ddc) ++ "{"
    mapM_ (hPutStrLn typeGraph) $ makeEdgesDi (ddcName ddc) (ddcContent ddc) -- (if length (ddcContent ddc) == 1 then True else False)
    hPutStrLn typeGraph $ makeNodesDi "darkviolet" $ ddcName ddc
    hPutStrLn typeGraph "}"
    hClose typeGraph

-- Helper to make a graph from datatypes that use the @newtype@ syntax
mkGraphNTOutput :: FilePath -> SCRT.NewtypeDecl -> IO ()
mkGraphNTOutput outputFilePath ntd = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (ntdName ntd ++ ".dot") WriteMode
    hPutStrLn typeGraph $ "digraph " ++ map toLower (ntdName ntd) ++ "{"
    mapM_ (hPutStrLn typeGraph) $ makeEdgesDi (ntdName ntd) (ntdContent ntd)
    hPutStrLn typeGraph $ makeNodesDi "darkgreen" $ ntdName ntd
    hPutStrLn typeGraph "}"
    hClose typeGraph

-- Helper to make a graph from datatypes that use the @type@ syntax
mkGraphTOutput :: FilePath -> SCRT.TypeDecl -> IO ()
mkGraphTOutput outputFilePath td = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (tdName td ++ ".dot") WriteMode
    hPutStrLn typeGraph $ "digraph " ++ map toLower (tdName td) ++ "{"
    mapM_ (hPutStrLn typeGraph) $ makeEdgesDi (tdName td) (tdContent td)
    hPutStrLn typeGraph $ makeNodesDi "red2" $ tdName td
    hPutStrLn typeGraph "}"
    hClose typeGraph

----------
-- Output for subgraphs
---------

-- Helper to create the drasil.dot graph file
mkFullOutputSub :: Handle -> Entry -> IO ()
mkFullOutputSub typeGraph entry = do
    hPutStrLn typeGraph $ "\tsubgraph " ++ map toLower (fileName entry \\ ".hs") ++ " {"
    mapM_ (mkGraphDROutputSub typeGraph) $ dataTypeRecords entry
    mapM_ (mkGraphDCOutputSub typeGraph) $ dataTypeConstructors entry
    mapM_ (mkGraphNTOutputSub typeGraph) $ newtypes entry
    mapM_ (mkGraphTOutputSub  typeGraph) $ types entry
    hPutStrLn typeGraph "\t}"

-- Helper to make a graph from datatypes that use the @data@ syntax and are records (for larger package-wide graphs)
mkGraphDROutputSub :: Handle -> SCRT.DataDeclRecord -> IO ()
mkGraphDROutputSub typeGraph ddr = do
    hPutStrLn typeGraph $ "\t\tsubgraph " ++ map toLower (ddrName ddr) ++ "{"
    mapM_ (hPutStrLn typeGraph) $ makeEdgesSub (ddrName ddr) (ddrContent ddr)
    hPutStrLn typeGraph $ makeNodesSub "cyan3" $ ddrName ddr
    hPutStrLn typeGraph "\t\t}"

-- Helper to make a graph from datatypes that use the @data@ syntax and are not records (for larger package-wide graphs)
mkGraphDCOutputSub :: Handle -> SCRT.DataDeclConstruct -> IO ()
mkGraphDCOutputSub typeGraph ddc = do
    hPutStrLn typeGraph $ "\t\tsubgraph " ++ map toLower (ddcName ddc) ++ "{"
    mapM_ (hPutStrLn typeGraph) $ makeEdgesSub (ddcName ddc) (ddcContent ddc)
    hPutStrLn typeGraph $ makeNodesSub "darkviolet" $ ddcName ddc
    hPutStrLn typeGraph "\t\t}"

-- Helper to make a graph from datatypes that use the @newtype@ syntax (for larger package-wide graphs)
mkGraphNTOutputSub :: Handle -> SCRT.NewtypeDecl -> IO ()
mkGraphNTOutputSub typeGraph ntd = do
    hPutStrLn typeGraph $ "\t\tsubgraph " ++ map toLower (ntdName ntd) ++ "{"
    mapM_ (hPutStrLn typeGraph) $ makeEdgesSub (ntdName ntd) (ntdContent ntd)
    hPutStrLn typeGraph $ makeNodesSub "darkgreen" $ ntdName ntd
    hPutStrLn typeGraph "\t\t}"

-- Helper to make a graph from datatypes that use the @type@ syntax (for larger package-wide graphs)
mkGraphTOutputSub :: Handle -> SCRT.TypeDecl -> IO ()
mkGraphTOutputSub typeGraph td = do
    hPutStrLn typeGraph $ "\t\tsubgraph " ++ map toLower (tdName td) ++ "{"
    mapM_ (hPutStrLn typeGraph) $ makeEdgesSub (tdName td) (tdContent td)
    hPutStrLn typeGraph $ makeNodesSub "red2" $ tdName td
    hPutStrLn typeGraph "\t\t}"

------------------
-- Graph-related functions
------------------

-- Creates an edge between a type and its dependency
makeEdgesDi :: String -> [String] -> [String]
makeEdgesDi _ [] = []
makeEdgesDi nm (c:cs) = (nm ++ " -> " ++ c ++ ";"): makeEdgesDi nm cs

-- Creates an edge between a type and its dependency (indented for subgraphs)
makeEdgesSub :: String -> [String] -> [String]
makeEdgesSub _ [] = []
makeEdgesSub nm (c:cs) = ("\t\t" ++ nm ++ " -> " ++ c ++ ";"): makeEdgesSub nm cs

-- Creates a node based on the kind of datatype
makeNodesDi :: Colour -> String -> String
makeNodesDi c nm = nm ++ "\t[shape=oval, color=" ++ c ++ ", label=" ++ nm ++ "];"

-- Creates a node based on the kind of datatype (indented for subgraphs)
makeNodesSub :: Colour -> String -> String
makeNodesSub c nm = "\t\t" ++ nm ++ "\t[shape=oval, color=" ++ c ++ ", label=" ++ nm ++ "];"

----------
-- Organizing data functions, getting data from files
---------

-- makes Entry data instance
makeEntry :: DC.DrasilPack -> DC.FileName -> FilePath -> [DataDeclRecord] -> 
    [DataDeclConstruct] -> [NewtypeDecl] -> [TypeDecl] -> Entry
makeEntry drpk fn fp dtR dtC ntd td = Entry {drasilPack=drpk,fileName=fn,
  filePath=fp,dataTypeRecords=dtR, dataTypeConstructors=dtC, newtypes=ntd, types=td}

-- import configurations function (drasil- packages)
config :: FilePath -> IO [String]
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
