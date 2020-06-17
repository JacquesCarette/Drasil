module DirectoryController (main) where

import Data.List
import System.IO
import System.Directory

type EntryData = String
type FileName = FilePath

type ClassInstance = String

-- main controller function
main :: IO ()
main = do all <- getDirectoryContents "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code"
          let filtered = filter (isPrefixOf "drasil-") all
          setCurrentDirectory "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code"
          cd <- getCurrentDirectory
          print cd
          mapM_ print filtered
          
          -- imports configuration settings
          [packageNames,classInstanceGroups,classInstances] <- config
          -- prints all drasil- package subdirectories
          iterator filtered

          -- easy + hard test for classes, medium test for data/newtypes, easy test for data types
          entry1 <- createEntry "Classes.hs" "/Users/Nathaniel_Hu/Desktop/Co-op/Issue#703" classInstances
          entry2 <- createEntry "RendererClasses.hs" "/Users/Nathaniel_Hu/Desktop/Co-op/Issue#703" classInstances
          entry3 <- createEntry "Core.hs" "/Users/Nathaniel_Hu/Desktop/Co-op/Issue#703" classInstances
          entry4 <- createEntry "Date.hs" "/Users/Nathaniel_Hu/Desktop/Co-op/Issue#703" classInstances

          -- entryData contains joined string with each file entries' data (intercalate)
          let entryData = intercalate "\n" [entry1,entry2,entry3,entry4]
          -- mapM_ print (lines entryData)
          -- creates and writes to output data file
          output entryData classInstances

-- iterates through each drasil- package and outputs subdirectories
iterator :: [FilePath] -> IO [a0]
iterator (x:xs) = do 
  setCurrentDirectory "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code"
  some <- getDirectoryContents x
  print x
  let l = filter (isInfixOf ".") some
      d = some \\ l
  print d
  if xs /= [] then iterator xs
  else return []

-- import configurations function (drasil- package ordering, class instance group ordering)
config :: IO [[String]]
config = do
  setCurrentDirectory "/Users/Nathaniel_Hu/Desktop/Co-op/Issue#703"
  c <- readFile "DCConfig.txt"
  let l = lines c
      [packageNames,_,classInstanceGroups,_,classInstances] = map words l
  return [packageNames,classInstanceGroups,classInstances]

-- function creates and writes output data file DataTable.csv
output :: EntryData -> [String] -> IO ()
output entryData classInstances = do
  setCurrentDirectory "/Users/Nathaniel_Hu/Desktop/Co-op/Issue#703"
  dataTable <- openFile "DataTable.csv" WriteMode
  hPutStrLn dataTable ("Package,\t,\t,\t,\t,\t,ClassInstances")
  hPutStrLn dataTable ("drasil-,File Path,File Name,Data Type,Newtype Type,Class Definitions," ++ intercalate "," classInstances)
  hPutStrLn dataTable (entryData)
  hClose dataTable

-- creates entries for each file FIXME pass classInsances to this function
createEntry :: FileName -> FilePath -> [ClassInstance] -> IO String
createEntry name path classInstances = do
  [dataNames,newtypeNames,classNames,stripInstances] <- extractEntryData name path

  -- f data newtype class
  let f d n c = intercalate "," [drasilPack,filePath,fileName,d,n,c] where 
      drasilPack = takeWhile (/= '/') . (\\ "-") $ dropWhile (/='-') path 
      filePath = path \\ "/Users/Nathaniel_Hu/"
      fileName = name

  let entry = if not (null dataNames) 
                then f (dataNames !! 0) "\t" "\t"
              else if not (null newtypeNames) 
                then f "\t" (newtypeNames !! 0) "\t"
              else if not (null classNames)
                then f "\t" "\t" (classNames !! 0)
              else f "\t" "\t" "\t"
  
  let dataEntries = map ("\t,\t,\t,"++) $ map (++",\t,\t") (dataNames)
      newtypeEntries = map ("\t,\t,\t,\t,"++) $ map (++",\t") (newtypeNames)
      classEntries = map ("\t,\t,\t,\t,\t,"++) (classNames)

  -- creating "Y" references to class instances for data types
  let instanceRefNames = map (f stripInstances) dataNames where 
      f b a = map (\\ (a ++ " ")) $ filter (isPrefixOf a) b
      instanceRefs = zipWith (\a b -> map (isInstanceOf a) b) instanceRefNames instanceSkeleton where 
          instanceSkeleton = replicate (length instanceRefNames) classInstances
      dataRefLines = map (intercalate ",") instanceRefs

  let dataEntries2 = zipWith (\a b -> a ++ "," ++ b) dataEntries dataRefLines
      entryData = dataEntries2 ++ newtypeEntries ++ classEntries
      
  let output = if (length entryData) > 1
                 then entry ++ "\n" ++ subEntries else f entry (dataRefLines !! 0) where
                   subEntries = intercalate "\n" (drop 1 entryData)
                   f a b = a ++ "," ++ b
  -- mapM_ print (lines output)
  return output

-- currenly extracts class names (data and newtypes to be added)
extractEntryData :: FileName -> FilePath -> IO [[String]]
extractEntryData fileName filePath = do
  setCurrentDirectory filePath
  scriptFile <- readFile fileName
  -- removes comment lines
  let scriptFileLines = (lines scriptFile) \\ filter (isInfixOf "-- ") (lines scriptFile)
      dataTypes = filter (isPrefixOf "data ") scriptFileLines
      newtypeTypes = filter (isPrefixOf "newtype ") scriptFileLines
  -- derived classes (that use '=>') vs. defined classes
      (derivClass,definClass) = partition (isInfixOf "=>") (filter (isPrefixOf "class ") scriptFileLines)
      definInstances = filter (isPrefixOf "instance ") scriptFileLines

  let dataNames = map (takeWhile (/=' ')) $ map (\\ "data ") dataTypes
      newtypeNames = map (takeWhile (/=' ')) $ map (\\ "newtype ") newtypeTypes
      stripDeriv = map (takeWhile (/=' ')) . map (\\ "> ") $ map (dropWhile (/='>')) derivClass
      stripDefin = map (takeWhile (/=' ')) $ map (\\ "class ") definClass
      classNames = stripDeriv ++ stripDefin
      stripInstances = zipWith (\a b -> b ++ " " ++ a) (map (!! 0) d) (map (!! 1) d) where
                       d = map words $ map (\\ "instance ") definInstances
  
  -- print stripInstances
  -- instanceNames = 

  return [dataNames,newtypeNames,classNames,stripInstances]

type FileInstance = String
type IsInstanceOf = String

-- (Foldable t, Eq a) => t a -> a -> [Char]
-- isInstanceOf :: FileInstance -> [FileInstance] -> [IsInstanceOf]
-- compares file instance in master list with file Instances List; replaces 
-- instance if in list with "Y" else "\t"
isInstanceOf fileInstances fileInstance = if isInstance then "Y" else "\t" where
  isInstance = fileInstance `elem` fileInstances
