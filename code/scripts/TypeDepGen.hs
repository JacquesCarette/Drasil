#!/usr/bin/env stack
{- stack script
   --resolver lts-20.20
   --package split
   --package directory,filepath
   --package text
   --package containers
-}

-- FIXME: use real parser (Low Priority; see line 189)
-- | Creates graphs showing the dependency of one type upon another.
module TypeDepGen (main) where

import Data.List
import Data.Maybe
import System.IO
import System.Directory
import System.FilePath (takeDirectory)
import Control.Monad
import qualified Data.Map as Map
import qualified DirectoryController as DC (createFolder, createFile, finder, 
  getDirectories, DrasilPack, FileName, FolderName, File(..), Folder(..))
import SourceCodeReaderT as SCRT (extractEntryData, EntryData(..), 
  DataDeclRecord(..), DataDeclConstruct(..), NewtypeDecl(..), TypeDecl(..),
  DataTypeDeclaration(..))
import Data.List.Split (splitOn)
import Data.Char (toLower)
import DataPrinters.Dot

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
    -- Makes a dependency graph of all types in the package
    mkFullOutput fp entry
    -- Makes folders for each package and then prints out a dependency graphs of all types in a module
    let outputFilePath = fp ++ "/" ++ drasilPack entry
    mapM_ (separateDigraph outputFilePath "cyan3") $ dataTypeRecords entry
    mapM_ (separateDigraph outputFilePath "darkviolet") $ dataTypeConstructors entry
    mapM_ (separateDigraph outputFilePath "darkgreen") $ newtypes entry
    mapM_ (separateDigraph outputFilePath "red2") $ types entry
    return mempty

-------
--Output sections
-------

-- Helper to create package-wide type dependency graphs
mkFullOutput :: FilePath -> Entry -> IO ()
mkFullOutput outputFilePath entry = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (drasilPack entry ++ ".dot") AppendMode
    hPutStrLn typeGraph $ "\tsubgraph " ++ map toLower (fileName entry \\ ".hs") ++ " {"
    -- each data type gets recorded in a different colour. Cyan is for record data types,
    -- dark violet is for types using the 'data' notation (but are not records), dark green
    -- is for types using the 'newtype' notation, and red is for types that use 'type' notation.
    -- Types that are not defined in the same package but still have an edge connecting them will
    -- appear as black, since that is the default node colour. This includes Haskell-native types
    -- like String and Int since we don't define them anywhere.
    mapM_ (subgraphDTD typeGraph "cyan3") $ dataTypeRecords entry
    mapM_ (subgraphDTD typeGraph "darkviolet") $ dataTypeConstructors entry
    mapM_ (subgraphDTD typeGraph "darkgreen") $ newtypes entry
    mapM_ (subgraphDTD typeGraph "red2") $ types entry
    hPutStrLn typeGraph "\t}"
    hClose typeGraph

-- Creates a separate directional graph for every type in a package.
-- These can be found in the folders labelled with the package name.
-- The 'digraph' function comes from DataPrinters/Dot.hs.
separateDigraph :: SCRT.DataTypeDeclaration a => FilePath -> Colour -> a -> IO ()
separateDigraph outputFilePath col typeDecl = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    typeGraph <- openFile (SCRT.getTypeName typeDecl ++ ".dot") WriteMode
    digraph typeGraph (map toLower $ SCRT.getTypeName typeDecl)
      [(col, [SCRT.getTypeName typeDecl])] [(SCRT.getTypeName typeDecl, SCRT.getContents typeDecl)]

----------
-- Output for subgraphs
---------

-- Helper to create the drasil.dot graph files from an entry of different kinds of datatypes.
mkFullOutputSub :: Handle -> Entry -> IO ()
mkFullOutputSub typeGraph entry = do
    hPutStrLn typeGraph $ "\tsubgraph " ++ replaceInvalidChars (map toLower $ fileName entry \\ ".hs") ++ " {"
    mapM_ (subgraphDTD typeGraph "cyan3") $ dataTypeRecords entry
    mapM_ (subgraphDTD typeGraph "darkviolet") $ dataTypeConstructors entry
    mapM_ (subgraphDTD typeGraph "darkgreen") $ newtypes entry
    mapM_ (subgraphDTD typeGraph "red2") $ types entry
    hPutStrLn typeGraph "\t}"

-- Subgraph creation function tailored for data type declarations.
-- The title of the subgraph is the name of the type,
-- and the contents are the dependencies of that type.
subgraphDTD :: SCRT.DataTypeDeclaration a => Handle -> Colour -> a -> IO ()
subgraphDTD typeGraph col typeDecl =
  subgraph typeGraph (map toLower $ SCRT.getTypeName typeDecl)
    [(col, [SCRT.getTypeName typeDecl])] [(SCRT.getTypeName typeDecl, SCRT.getContents typeDecl)]

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
