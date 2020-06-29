module DataTableGen (main) where

import Data.List
import System.IO
import System.Directory
import qualified DirectoryController as DC (createFolder, createFile, finder, 
  finder2, getDirectories, getDirectories2, stripPath, DrasilPack, FileName, 
  FolderName, File(..), Folder(..))
import SourceCodeReader as SCR (extractEntryData, extractEntryData2, EntryData)

type ClassInstance = String

type FileInstance = String
type IsInstanceOf = String

type DataType = String
type Newtype = String
type DNType = String
type ClassName = String
type EntryString = String

-- Entry data type for storing entry data for each file entry
data Entry = Entry { drasilPack :: DC.DrasilPack
                   , fileName :: DC.FileName -- change to entryName?
                   , filePath :: FilePath -- change to entryPath?
                   , dataTypes :: [DataType]
                   , newtypes :: [Newtype]
                   , classes :: [Class]
                   , classInstances :: [ClassInstance2]
                   } deriving (Show)
-- ClassInstance2 data type for storing data/newtype type name + class instance name
data ClassInstance2 = ClassInstance2 {dnType :: DNType, classInstName :: ClassName} 
  deriving (Show)
-- Class data type to store class name + type
data Class = Class {className :: ClassName, classType :: ClassType} 
  deriving (Show)
-- ClassType data type for specifying class type (Haskell, Generic or GOOL)
data ClassType = Haskell | Generic | GOOL

-- Eq instance of ClassType to enable comparisons
instance Eq ClassType where
  Haskell == Haskell = True
  Generic == Generic = True
  GOOL == GOOL = True
  _ == _ = False
-- Show instance of ClassType to enable printing to console
instance Show ClassType where
  show Haskell = "Haskell-defined"
  show Generic = "Generic-defined"
  show GOOL = "GOOL-defined"

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
  -- mapM_ print filtered
  
  filtered2 <- DC.getDirectories2 codeDirectory "drasil-"
  -- mapM_ print filtered2

  -- imports configuration settings
  [packageNames,classInstanceGroups,classInstances] <- config scriptsDirectory

  -- all files + filepaths stored here; obtained from filtered list of drasil- packages using finder
  allFiles <- mapM DC.finder filtered
  -- mapM_ print allFiles

  allFiles2 <- mapM DC.finder2 filtered2
  -- mapM_ print allFiles2

  -- converts list of rawFileData (filename + filepath) into list of rawEntryData (filename, 
  -- truncated filepath, data, newtype and class names, class instances)
  let rawFileData = intercalate ["new line"] allFiles
  -- mapM print rawFileData
  rawEntryData <- mapM (createEntry codeDirectory classInstances . words) rawFileData
  -- mapM print rawEntryData

  -- entryData contains joined string with each file entries' data (intercalate)
  let entryData = intercalate "\n" rawEntryData
  
  let rawFileData2 = intercalate [DC.createFile "" "" "newline"] allFiles2
  -- mapM print rawFileData2
  rawEntryData2 <- sequence (zipWith (createEntry2 codeDirectory classInstances) rawFileData2 (map DC.fileName rawFileData2))
  -- mapM print rawEntryData2

  bakedEntryData <- sequence (zipWith (compileEntryData classInstances) rawEntryData2 (map fileName rawEntryData2))
  let entryData2 = intercalate "\n" bakedEntryData
  -- print entryData2

  -- creates and writes to output data file
  output outputDirectory entryData2 classInstances

-- makes Entry data
makeEntry :: DC.DrasilPack -> DC.FileName -> FilePath -> [DataType] -> 
  [Newtype] -> [Class] -> [ClassInstance2] -> Entry
makeEntry drpk fn fp dtl ntl cls clsint = Entry {drasilPack=drpk,fileName=fn,
  filePath=fp,dataTypes=dtl,newtypes=ntl,classes=cls,classInstances=clsint}

-- makes Class data
makeClass :: ClassType -> ClassName -> Class
makeClass clstp clsnm = Class {className=clsnm,classType=clstp}

-- makes ClassInstance2 data
makeClassInstance2 :: (DNType,ClassName) -> ClassInstance2
makeClassInstance2 (dnt,cls) = ClassInstance2 {dnType=dnt,classInstName=cls}

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
        | length entryData > 1 && not (null dataRefLines) = join' entry (head dataRefLines) ++ "\n" ++ subEntries
        | length entryData > 1 && not (null newtypeRefLines) = join' entry (head newtypeRefLines) ++ "\n" ++ subEntries
        | length entryData > 1 = entry ++ "\n" ++ subEntries
        | length entryData == 1 && not (null dataRefLines) = join' entry (head dataRefLines)
        | length entryData == 1 && not (null newtypeRefLines) = join' entry (head newtypeRefLines)
        | otherwise = entry
      subEntries = intercalate "\n" (drop 1 entryData)

  -- mapM_ print (lines output)
  return output

-- creates an entry for each file (updated/optimized Entry data format)
createEntry2 :: FilePath -> [ClassInstance] -> DC.File -> DC.FileName -> IO Entry
-- creates blank line to separate file entries by drasil- package
createEntry2 homeDirectory classInstances file "newline" = return (makeEntry "" "newline" "" [] [] [] [])
-- creates actual file entries
createEntry2 homeDirectory classInstances file filename = do
  let drpk = DC.fileDrasilPack file
      fn = filename
      fp = DC.filePath file
      -- entry file path is stripped of home directory and drasil- pack
      efp = (++ "/") $ (\\ homeDirectory ++ "/drasil-" ++ drpk ++ "/") fp

  -- stripInstances = [(dataType,classInfo)]
  (dtl,ntl,classNames,stripInstances) <- SCR.extractEntryData2 fn fp
  
  let clstp
        | drpk == "gool" = GOOL
        | otherwise = Generic

  let cls = map (makeClass clstp) classNames
      clsint = map makeClassInstance2 stripInstances

  let entry = makeEntry drpk fn efp dtl ntl cls clsint
  return entry

-- creates entrystring for each entry (contains lines for each entry data)
-- in this function, remember to sort the data/newtype/class/instance names before creating entrystrings
compileEntryData :: [ClassInstance] -> Entry -> DC.FileName -> IO EntryString
-- creates blank line entrystring to separate file entries by drasil- package
compileEntryData classInstancesI entry "newline" = return "\t"
-- creates entrystrings for actual file entries
compileEntryData classInstancesI entry filename = do

  -- f data newtype class; joins data value/placeholders for data/newtype/class (in first data entry line)
  let f d n c = intercalate "," [drpk,fp,fn,d,n,c]
      (drpk,fp,fn) = (drasilPack entry,filePath entry,fileName entry)

  -- extracts dataNames, newtypeNames, classNames and class instance names from entry
  let dataNames = dataTypes entry
      newtypeNames = newtypes entry
      classNames = map className (classes entry)
      entryClassInsts = classInstances entry

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
  let dataInstanceRefNames = map (getRefNames (entryClassInsts)) dataNames
      dataRefLines = createRefLines classInstancesI dataInstanceRefNames
      dataEntries2 = zipWith join' dataEntries dataRefLines

  -- creating "Y" references to class instances for newtype types
  let newtypeInstanceRefNames = map (getRefNames (entryClassInsts)) newtypeNames
      newtypeRefLines = createRefLines classInstancesI newtypeInstanceRefNames
      newtypeEntries2 = zipWith join' newtypeEntries newtypeRefLines

  -- creates file entry data by combining data, newtype and class entries
  let entryData = dataEntries2 ++ newtypeEntries2 ++ classEntries
  
  -- guards determine how to handle overall file entry output (single vs. multiple entry data)
  let output 
        | length entryData > 1 && not (null dataRefLines) = fdtl ++ "\n" ++ sbE
        | length entryData > 1 && not (null newtypeRefLines) = fntl ++ "\n" ++ sbE
        | length entryData > 1 = entry ++ "\n" ++ sbE
        | length entryData == 1 && not (null dataRefLines) = fdtl
        | length entryData == 1 && not (null newtypeRefLines) = fntl
        | otherwise = entry
      sbE = intercalate "\n" (drop 1 entryData)
      fdtl = join' entry (head dataRefLines)
      fntl = join' entry (head newtypeRefLines)

  -- mapM_ print (lines output)
  return output

-- compares file instance in master list with file Instances List; replaces 
-- instance if in list with "Y" else "\t"
isInstanceOf :: [FileInstance] -> FileInstance -> IsInstanceOf
isInstanceOf fileInstances fileInstance = if isInstance then yes else no where
  isInstance = fileInstance `elem` fileInstances
  yes = "Y" :: IsInstanceOf
  no = "\t" :: IsInstanceOf

-- tests if ClassInstance2 is of the data/newtype typename (DNType)
isTypeOf :: DNType -> ClassInstance2 -> Bool
isTypeOf typeName classInstance = if isType then True else False where
  isType = dnType classInstance == typeName

-- joins two strings together with ","
join' :: String -> String -> String
join' a b = a ++ "," ++ b

-- gets class instance reference names for a data/newtype typename (DNType)
getRefNames :: [ClassInstance2] -> DNType -> [ClassName]
getRefNames clsInsts tn = map classInstName $ filter (isTypeOf tn) clsInsts

-- creates class instance reference lines (EntryString fragments) for each data/newtype line
createRefLines :: [ClassInstance] -> [[ClassName]] -> [EntryString]
createRefLines classInstances instRefNames = map (intercalate ",") instRefs where
  instRefs = zipWith (map . isInstanceOf) instRefNames instanceSkeleton 
  instanceSkeleton = replicate (length instRefNames) classInstances
