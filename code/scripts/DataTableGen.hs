module DataTableGen (main) where

import Data.List
import System.IO
import System.Directory
import DirectoryController as DC (iterator, iterator2, finder)
import SourceCodeReader as SCR (extractEntryData)

type EntryData = String
type FileName = FilePath

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

  test1 <- DC.iterator2 "Language /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang"
  print test1

  test2 <- mapM DC.iterator2 ["Language /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang",
                              "GOOL /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-gool"]
  print test2

  -- list of all files (names + filepaths)
  fpsCode <- DC.finder "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code" (head filtered) 
  mapM_ print fpsCode

  let curDir1 = "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang/Language/Drasil"
      curDir2 = "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-gool/GOOL/Drasil"
      curDir3 = "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang/Language/Drasil/Document"
      curDir4 = "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang/Language/Drasil/Data"

  -- easy + hard test for classes, medium test for data/newtypes, easy test for data types
  entry1 <- createEntry classInstances "Classes.hs" curDir1
  entry2 <- createEntry classInstances "RendererClasses.hs" curDir2
  entry3 <- createEntry classInstances "Core.hs" curDir3
  entry4 <- createEntry classInstances "Date.hs" curDir4

  -- entryData contains joined string with each file entries' data (intercalate)
  let entryData = intercalate "\n" [entry1,entry2,entry3,entry4]
  -- mapM_ print (lines entryData)

  let rawFileData = ["Classes.hs /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang/Language/Drasil",
                     "RendererClasses.hs /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-gool/GOOL/Drasil",
                     "Core.hs /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang/Language/Drasil/Document",
                     "Date.hs /Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-lang/Language/Drasil/Data"]
  rawEntryData <- mapM (createEntry2 classInstances . words) rawFileData

  -- print [entry1,entry2,entry3,entry4]
  -- print rawEntryData

  -- creates and writes to output data file
  output entryData classInstances

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

-- creates entries for each file (original input format)
createEntry :: [ClassInstance] -> FileName -> FilePath -> IO String
createEntry classInstances name path = do
  [dataNames,newtypeNames,classNames,stripInstances] <- SCR.extractEntryData name path

  -- f data newtype class
  let f d n c = intercalate "," [drasilPack,filePath,fileName,d,n,c] 
      drasilPack = takeWhile (/= '/') . (\\ "-") $ dropWhile (/='-') path 
      filePath = (\\ "/") . dropWhile (/= '/') $ (\\ "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-") path
      fileName = name

  let entry 
        | not (null dataNames) = f (head dataNames) "\t" "\t"
        | not (null newtypeNames) = f "\t" (head newtypeNames) "\t"
        | not (null classNames) = f "\t" "\t" (head classNames)
        | otherwise = f "\t" "\t" "\t"
  
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

  let entryData = dataEntries2 ++ newtypeEntries2 ++ classEntries
      
  let output 
        | length entryData > 1 = entry ++ "\n" ++ subEntries
        | length entryData == 1 && not (null dataRefLines) = f entry (head dataRefLines)
        | length entryData == 1 && not (null newtypeRefLines) = f entry (head newtypeRefLines)
        | otherwise = entry
      subEntries = intercalate "\n" (drop 1 entryData)
      f a b = a ++ "," ++ b

  -- mapM_ print (lines output)
  return output

  -- creates entries for each file (different input format)
createEntry2 :: [ClassInstance] -> [FilePath] -> IO String
createEntry2 classInstances [name,path] = do
  [dataNames,newtypeNames,classNames,stripInstances] <- SCR.extractEntryData name path

  -- f data newtype class
  let f d n c = intercalate "," [drasilPack,filePath,fileName,d,n,c] 
      drasilPack = takeWhile (/= '/') . (\\ "-") $ dropWhile (/='-') path 
      filePath = (\\ "/") . dropWhile (/= '/') $ (\\ "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code/drasil-") path
      fileName = name

  let entry 
        | not (null dataNames) = f (head dataNames) "\t" "\t"
        | not (null newtypeNames) = f "\t" (head newtypeNames) "\t"
        | not (null classNames) = f "\t" "\t" (head classNames)
        | otherwise = f "\t" "\t" "\t"
  
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

  let entryData = dataEntries2 ++ newtypeEntries2 ++ classEntries
      
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
