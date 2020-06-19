module DataTableGen (main) where

import Data.List
import System.IO
import System.Directory
import DirectoryController as DC (iterator, iterator2, iterator3, finder, finder2)
import SourceCodeReader as SCR (extractEntryData)

type EntryData = String
type FileName = FilePath
type FolderName = FilePath

type ClassInstance = String

type FileInstance = String
type IsInstanceOf = String

-- main controller function
main :: IO ()
main = do
  all <- getDirectoryContents "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code"
  
  -- filters all drasil- packages
  let filtered = filter (isPrefixOf "drasil-") all
  
  setCurrentDirectory "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code"

  -- imports configuration settings
  [packageNames,classInstanceGroups,classInstances] <- config
  -- prints all drasil- package subdirectories
  DC.iterator filtered
  
  test2 <- mapM DC.iterator2 ["Language /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang",
                              "GOOL /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-gool"]
  print test2
  print (concat $ map (!! 0) test2)
  print (concat $ map (!! 1) test2)

  test3 <- DC.iterator2 "drasil-lang /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code"
  print test3
  print (test3 !! 0)
  print (test3 !! 1)

  test4 <- DC.finder2 "drasil-lang /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code"
  print test4
  -- mapM_ print test4
  print (length test4)

  -- list of all files (names + filepaths)
  fpsCode <- DC.finder "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code" (head filtered) 
  mapM_ print fpsCode

  -- easy + hard test for classes, medium test for data/newtypes, easy test for data types
  let rawFileData = ["Classes.hs /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang/Language/Drasil",
                     "RendererClasses.hs /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-gool/GOOL/Drasil",
                     "Core.hs /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang/Language/Drasil/Document",
                     "Date.hs /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang/Language/Drasil/Data"]
  rawEntryData <- mapM (createEntry classInstances . words) rawFileData

  let rawFileData2 = test4
  rawEntryData2 <- mapM (createEntry classInstances . words) rawFileData2

  -- entryData contains joined string with each file entries' data (intercalate)
  let entryData = intercalate "\n" rawEntryData
  -- mapM_ print (lines entryData)
  
  let entryData2 = intercalate "\n" rawEntryData2
  -- mapM_ print (lines entryData2)

  -- creates and writes to output data file
  output entryData2 classInstances

-- import configurations function (drasil- package ordering, class instance group ordering)
config :: IO [[String]]
config = do
  setCurrentDirectory "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/scripts"
  c <- readFile "DTG_Config.txt"
  let l = lines c
      [packageNames,_,classInstanceGroups,_,classInstances] = map words l
  return [packageNames,classInstanceGroups,classInstances]

-- function creates and writes output data file DataTable.csv
output :: EntryData -> [String] -> IO ()
output entryData classInstances = do
  -- adjust this later to be "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/analysis"
  setCurrentDirectory "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/scripts"
  dataTable <- openFile "DataTable.csv" WriteMode
  hPutStrLn dataTable "Package,\t,\t,\t,\t,\t,ClassInstances"
  hPutStrLn dataTable ("drasil-,File Path,File Name,Data Type,Newtype Type,Class Definitions," ++ intercalate "," classInstances)
  hPutStrLn dataTable entryData
  hClose dataTable

  -- creates an entry for each file (updated/optimized input format)
createEntry :: [ClassInstance] -> [FilePath] -> IO String
createEntry classInstances [name,path] = do
  [dataNames,newtypeNames,classNames,stripInstances] <- SCR.extractEntryData name path

  -- f data newtype class
  let f d n c = intercalate "," [drasilPack,filePath,fileName,d,n,c] 
      drasilPack = takeWhile (/= '/') . (\\ "-") $ dropWhile (/='-') path 
      filePath = (\\ "/") . dropWhile (/= '/') $ (\\ "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-") path
      fileName = name

  -- guards determine how to handle first data entry
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
      dataEntries2 = zipWith (\a b -> a ++ "," ++ b) dataEntries dataRefLines

  -- creating "Y" references to class instances for newtype types
  let newtypeInstanceRefNames = map (f stripInstances) newtypeNames
      f b a = map (\\ (a ++ " ")) $ filter (isPrefixOf a) b
      newtypeInstanceRefs = zipWith (map . isInstanceOf) newtypeInstanceRefNames instanceSkeleton where 
          instanceSkeleton = replicate (length newtypeInstanceRefNames) classInstances
      newtypeRefLines = map (intercalate ",") newtypeInstanceRefs
      newtypeEntries2 = zipWith (\a b -> a ++ "," ++ b) newtypeEntries newtypeRefLines

  -- creates file entry data by combining data, newtype and class entries
  let entryData = dataEntries2 ++ newtypeEntries2 ++ classEntries
  
  -- guards determine how to handle overall file entry output (single vs. multiple entry data)
  let output 
        | length entryData > 1 = entry ++ "\n" ++ subEntries
        | length entryData == 1 && not (null dataRefLines) = f entry (head dataRefLines)
        | length entryData == 1 && not (null newtypeRefLines) = f entry (head newtypeRefLines)
        | otherwise = entry
      subEntries = intercalate "\n" (drop 1 entryData)
      f a b = a ++ "," ++ b

  -- mapM_ print (lines output)
  return output

-- compares file instance in master list with file Instances List; replaces 
-- instance if in list with "Y" else "\t"
isInstanceOf :: [FileInstance] -> FileInstance -> IsInstanceOf
isInstanceOf fileInstances fileInstance = if isInstance then yes else no where
  isInstance = fileInstance `elem` fileInstances
  yes = "Y" :: IsInstanceOf
  no = "\t" :: IsInstanceOf
