module DataTableGen (main) where

import Data.List
import System.IO
import System.Directory
import DirectoryController as DC (finder, getDirectories, stripPath)
import SourceCodeReader as SCR (extractEntryData, EntryData)

type ClassInstance = String

type FileInstance = String
type IsInstanceOf = String

-- main controller function; update later to contain parameter for starting filepath
main :: IO ()
main = do
  -- directory variables (for scripts, code and output directories)
  scriptsDirectory <- getCurrentDirectory
  -- strips trailing "/scripts" subdirectory off code directory filepath
  codeDirectory <- DC.stripPath scriptsDirectory "/scripts"
  let outputDirectory = codeDirectory ++ "/analysis"

  -- gets names + filepaths of all drasil- packages/directories
  filtered <- DC.getDirectories codeDirectory "drasil-"
  
  -- imports configuration settings
  [packageNames,classInstanceGroups,classInstances] <- config scriptsDirectory

  -- all files + filepaths stored here; obtained from filtered list of drasil- packages using finder
  allFiles <- mapM DC.finder filtered

  -- converts list of rawFileData (filename + filepath) into list of rawEntryData (filename, 
  -- truncated filepath, data, newtype and class names, class instances)
  let rawFileData = intercalate ["new line"] allFiles
  rawEntryData <- mapM (createEntry codeDirectory classInstances . words) rawFileData

  -- entryData contains joined string with each file entries' data (intercalate)
  let entryData = intercalate "\n" rawEntryData

  -- creates and writes to output data file
  output outputDirectory entryData classInstances

-- import configurations function (drasil- package ordering, class instance group ordering)
config :: FilePath -> IO [[String]]
config configFilePath = do
  setCurrentDirectory configFilePath
  c <- readFile "DTG_Config.txt"
  let l = lines c
      [packageNames,_,classInstanceGroups,_,classInstances] = map words l
  return [packageNames,classInstanceGroups,classInstances]

-- function creates and writes output data file DataTable.csv to /code/analysis
output :: FilePath -> SCR.EntryData -> [String] -> IO ()
output outputFilePath entryData classInstances = do
  createDirectoryIfMissing False outputFilePath
  setCurrentDirectory outputFilePath
  dataTable <- openFile "DataTable.csv" WriteMode
  hPutStrLn dataTable "Package,\t,\t,\t,\t,\t,ClassInstances"
  hPutStrLn dataTable ("drasil-,File Path,File Name,Data Type,Newtype Type,Class Definitions," ++ intercalate "," classInstances)
  hPutStrLn dataTable entryData
  hClose dataTable

  -- creates an entry for each file (updated/optimized input format)
createEntry :: FilePath -> [ClassInstance] -> [FilePath] -> IO String
-- creates blank line to separate file entries by drasil- package
createEntry homeDirectory classInstances ["new","line"] = return "\t"
-- creates actual file entries
createEntry homeDirectory classInstances [name,path] = do
  [dataNames,newtypeNames,classNames,stripInstances] <- SCR.extractEntryData name path

  -- f data newtype class; joins data value/placeholders for data/newtype/class (in first data entry line)
  let f d n c = intercalate "," [drasilPack,filePath,fileName,d,n,c] 
      drasilPack = takeWhile (/= '/') . (\\ "-") $ dropWhile (/='-') path 
      filePath = (++ "/") . (\\ "/") . dropWhile (/= '/') $ (\\ homeDirectory ++ "/drasil-") path
      fileName = name

  -- guards determine how to handle first data entry line
  let entry 
        | not (null dataNames) = f (head dataNames) "\t" "\t"
        | not (null newtypeNames) = f "\t" (head newtypeNames) "\t"
        | not (null classNames) = f "\t" "\t" (head classNames)
        | otherwise = f "\t" "\t" "\t"
  
  -- creates heads of each data, newtype and class entry line
  let dataEntries = map (("\t,\t,\t,"++) . (++",\t,\t")) dataNames
      newtypeEntries = map (("\t,\t,\t,\t,"++) . (++",\t")) newtypeNames
      classEntries = map ("\t,\t,\t,\t,\t,"++) classNames

  -- creating "Y" references to class instances for data types
  let dataInstanceRefNames = map (f stripInstances) dataNames
      f b a = map (\\ (a ++ " ")) $ filter (isPrefixOf a) b
      dataInstanceRefs = zipWith (map . isInstanceOf) dataInstanceRefNames instanceSkeleton where 
          instanceSkeleton = replicate (length dataInstanceRefNames) classInstances
      dataRefLines = map (intercalate ",") dataInstanceRefs
      dataEntries2 = zipWith join' dataEntries dataRefLines

  -- creating "Y" references to class instances for newtype types
  let newtypeInstanceRefNames = map (f stripInstances) newtypeNames
      f b a = map (\\ (a ++ " ")) $ filter (isPrefixOf a) b
      newtypeInstanceRefs = zipWith (map . isInstanceOf) newtypeInstanceRefNames instanceSkeleton where 
          instanceSkeleton = replicate (length newtypeInstanceRefNames) classInstances
      newtypeRefLines = map (intercalate ",") newtypeInstanceRefs
      newtypeEntries2 = zipWith join' newtypeEntries newtypeRefLines

  -- creates file entry data by combining data, newtype and class entries
  let entryData = dataEntries2 ++ newtypeEntries2 ++ classEntries
  
  -- guards determine how to handle overall file entry output (single vs. multiple entry data)
  let output 
        | length entryData > 1 = entry ++ "\n" ++ subEntries
        | length entryData == 1 && not (null dataRefLines) = join' entry (head dataRefLines)
        | length entryData == 1 && not (null newtypeRefLines) = join' entry (head newtypeRefLines)
        | otherwise = entry
      subEntries = intercalate "\n" (drop 1 entryData)

  -- mapM_ print (lines output)
  return output

-- compares file instance in master list with file Instances List; replaces 
-- instance if in list with "Y" else "\t"
isInstanceOf :: [FileInstance] -> FileInstance -> IsInstanceOf
isInstanceOf fileInstances fileInstance = if isInstance then yes else no where
  isInstance = fileInstance `elem` fileInstances
  yes = "Y" :: IsInstanceOf
  no = "\t" :: IsInstanceOf

-- joins two strings together with ","
join' :: String -> String -> String
join' a b = a ++ "," ++ b
