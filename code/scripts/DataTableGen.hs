-- FIXME: use real parser (Low Priority; see line 119)
module DataTableGen (main) where

import Data.List
import Data.Maybe
import System.IO
import System.Directory
import System.FilePath (takeDirectory)
import Control.Monad
import qualified Data.Map as Map
import qualified DirectoryController as DC (createFolder, createFile, finder, 
  getDirectories, DrasilPack, FileName, FolderName, File(..), Folder(..))
import SourceCodeReader as SCR (extractEntryData, EntryData(..))
import Data.List.Split (splitOn)

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
                   , classInstances :: [ClassInstance]
                   } deriving (Show)
-- ClassInstance data type for storing data/newtype type + class instance names
data ClassInstance = ClassInstance {dnType :: DNType, clsInstName :: ClassName} 
  deriving (Show)
-- Class data type to store class name + type
data Class = Class {className :: ClassName, classType :: ClassType} 
  deriving (Show)
-- ClassType data type for specifying class type (Haskell, Drasil or GOOL)
data ClassType = Haskell | Drasil | GOOL deriving (Eq)

-- Show instance of ClassType to enable printing to console
instance Show ClassType where
  show Haskell = "Haskell-defined"
  show Drasil = "Drasil-defined"
  show GOOL = "GOOL-defined"

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
  (packageNames,classInstOrd) <- config scriptsDirectory
  
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
  let rawFileData = intercalate [DC.createFile "" "" "newline"] allFiles

  -- creates Entry instances (w/ file data) from File instances (File -> Entry)
  rawEntryData <- zipWithM (createEntry codeDirectory) rawFileData (map DC.fileName rawFileData)

  let ordrdClasses = ordClasses classInstOrd rawEntryData
      ordrdClassNames = map className ordrdClasses

  -- creates EntryString instances containing entry data
  bakedEntryData <- zipWithM (compileEntryData ordrdClassNames) rawEntryData (map fileName rawEntryData)
  -- contains joined string with each file EntryString ("\n" separated)
  let entryData = intercalate "\n" bakedEntryData

  -- creates and writes to output data file
  output outputDirectory entryData ordrdClassNames bakedEntryData

-- makes Entry data instance
makeEntry :: DC.DrasilPack -> DC.FileName -> FilePath -> [DataType] -> 
  [Newtype] -> [Class] -> [ClassInstance] -> Entry
makeEntry drpk fn fp dtl ntl cls clsint = Entry {drasilPack=drpk,fileName=fn,
  filePath=fp,dataTypes=dtl,newtypes=ntl,classes=cls,classInstances=clsint}

-- makes Class data instance
makeClass :: ClassType -> ClassName -> Class
makeClass clstp clsnm = Class {className=clsnm,classType=clstp}

-- makes ClassInstance data instance
makeClassInstance :: (DNType,ClassName) -> ClassInstance
makeClassInstance (dnt,cls) = ClassInstance {dnType=dnt,clsInstName=cls}

-- import configurations function (drasil- package + class instance orderings)
config :: FilePath -> IO ([String],[ClassType])
config configFilePath = do
  setCurrentDirectory configFilePath
  c <- readFile "DTG_Config.txt"
  let l = map words $ filter isInfoLine (lines c)
      -- FIXME: use real parser to extract settings from config file
      -- will improve error messages regarding config file settings
      (packageNames,classInstOrd) = (head l, map toClassType (l !! 1))
  return (packageNames,classInstOrd)

-- function creates and writes output data file DataTable.csv to /code/analysis
output :: FilePath -> EntryString -> [ClassName] -> [EntryString] -> IO ()
output outputFilePath entryData ordClassInsts bakedEntryData = do
  createDirectoryIfMissing False outputFilePath
  setCurrentDirectory outputFilePath
  dataTable <- openFile "DataTable.csv" WriteMode
  hPutStrLn dataTable "Package,\t,\t,\t,\t,\t,Class Instances"
  hPutStr dataTable "drasil-,File Path,File Name,Data Type,Newtype Type,Class Definitions,"
  hPutStrLn dataTable (intercalate "," ordClassInsts)
  hPutStrLn dataTable entryData
  hClose dataTable
  -- row length needed for lenCheck
  let rowLength = length (splitOn "," "drasil-,File Path,File Name,Data Type,Newtype Type,Class Definitions" ++ ordClassInsts)
  dataTableHTML <- openFile "DataTable.html" WriteMode
  hPutStrLn dataTableHTML "<!DOCTYPE html>\n<html>\n\t<title>Auto-Generated Data Table for Drasil</title>"
  hPutStrLn dataTableHTML "\t<table border=\"1\" cellspacing=\"0\" cellpadding=\"3\" class=\"dataframe\">"
  hPutStrLn dataTableHTML (mkhtmlTitle ("Package,\t,\t,\t,\t,\t,Class Instances" ++ mkhtmlEmptyCell (rowLength - 7)))
  hPutStrLn dataTableHTML (mkhtmlHeader (splitOn "," "drasil-,File Path,File Name,Data Type,Newtype Type,Class Definitions" ++ ordClassInsts))
  hPutStr dataTableHTML (mkhtmlRow (lenCheck (separateN bakedEntryData) rowLength))
  hPutStrLn dataTableHTML "\t\t</tbody>\n</html>"
  hClose dataTableHTML

-- | Checks the length of an 'EntryString' to see if it matchs the given length. If not, it will add empty html cells.
lenCheck :: [EntryString] -> Int -> [EntryString]
lenCheck [] _ = []
lenCheck (x:xs) len = (x ++ mkhtmlEmptyCell (len - length (splitOn "," x))) : lenCheck xs len

-- | Separate an 'EntryString' by newspace (used for separating the incoming bakedEntryData in 'output').
separateN :: [EntryString] -> [EntryString]
separateN = concatMap (splitOn "\n")

-- | Adds all of the required html syntax to generate the title files.
mkhtmlTitle :: String -> String
mkhtmlTitle xs = "\t\t<thead>\n" ++ concatMap (\y -> "\t\t\t<th>" ++ y ++ "</th>\n") (splitOn "," xs) ++ "\t\t</thead>"

-- | Similar to 'mkhtmlTitle', but the given list of strings must already be split into their respective cells.
mkhtmlHeader :: [String] -> String
mkhtmlHeader xs = "\t\t<thead>\n" ++ concatMap (\y -> "\t\t\t<th>" ++ y ++ "</th>\n") xs ++ "\t\t</thead>"

-- | Fills in the given number of html cells in a row with empty cells.
mkhtmlEmptyCell :: Int -> String
mkhtmlEmptyCell num
  | num <= 0 = ""
  | otherwise = ",\t" ++ mkhtmlEmptyCell (num-1)

-- | Adds all required html syntax for creating a normal html table row.
mkhtmlRow :: [EntryString] -> String
mkhtmlRow [] = []
mkhtmlRow (x:xs) = "\t\t\t<tr>\n" ++ concatMap (\y -> "\t\t\t\t<td>" ++ y ++ "</td>\n") (splitOn "," x) ++ "\t\t\t</tr>\n" ++ mkhtmlRow xs

-- creates an entry for each file (new Entry data-oriented format)
createEntry :: FilePath -> DC.File -> DC.FileName -> IO Entry
-- creates blank line to separate file entries by drasil- package
createEntry _ _ "newline" = return nlEntry where 
  nlEntry = makeEntry "" "newline" "" [] [] [] []
-- creates actual file entries
createEntry homeDirectory file filename = do
  let drpk = DC.fileDrasilPack file
      fn = filename
      fp = DC.filePath file
      -- entry file path is stripped of home directory and drasil- pack
      efp = (++ "/") $ (\\ homeDirectory ++ "/drasil-" ++ drpk ++ "/") fp

  -- extracts entry data from File data type
  -- stripInstances = [(dataType,classInfo)]
  rEntryData <- SCR.extractEntryData fn fp
  
  let dtl = SCR.dNs rEntryData
      ntl = SCR.ntNs rEntryData
      classNames = SCR.cNs rEntryData
      stripInstances = SCR.cITs rEntryData

  let clstp
        | drpk == "gool" = GOOL
        | otherwise = Drasil

  let cls = map (makeClass clstp) classNames
      clsint = map makeClassInstance stripInstances

  let entry = makeEntry drpk fn efp dtl ntl cls clsint
  return entry

-- creates entrystring for each entry (contains lines for each entry's data)
compileEntryData :: [ClassName] -> Entry -> DC.FileName -> IO EntryString
-- creates blank line entrystring to separate file entries by drasil- package
compileEntryData _ _ "newline" = return "\t"
-- creates entrystrings for actual file entries
compileEntryData ordClassInsts entry filename = do

  -- joins data, newtype and class values/placeholders (first data entry line)
  let f d n c = intercalate "," [drpk,fp,fn,d,n,c]
      (drpk,fp,fn) = (drasilPack entry,filePath entry,fileName entry)

  -- extracts data, newtype and class names + class instances from entry
  let dataNames = dataTypes entry
      newtypeNames = newtypes entry
      classNames = map className (classes entry)
      entryClassInsts = classInstances entry

  -- guards determine how to handle first data entry line
  let entry = hEnt dataNames newtypeNames classNames
      hEnt (x:_) _ _ = f x "\t" "\t"
      hEnt _ (x:_) _ = f "\t" x "\t"
      hEnt _ _ (x:_) = f "\t" "\t" x
      hEnt _ _ _     = f "\t" "\t" "\t"
  
  -- creates heads of each data, newtype and class entry line
  let dtEntryHds = map (("\t,\t,\t,"++) . (++",\t,\t")) dataNames
      ntEntryHds = map (("\t,\t,\t,\t,"++) . (++",\t")) newtypeNames
      clsEntries = map ("\t,\t,\t,\t,\t,"++) classNames

  -- creating "Y" references to class instances for data types
  let dtInstRefNames = map (getRefNames entryClassInsts) dataNames
      dtRefLines = createRefLines ordClassInsts dtInstRefNames
      dtEntries = zipWith join' dtEntryHds dtRefLines

  -- creating "Y" references to class instances for newtype types
  let ntInstRefNames = map (getRefNames entryClassInsts) newtypeNames
      ntRefLines = createRefLines ordClassInsts ntInstRefNames
      ntEntries = zipWith join' ntEntryHds ntRefLines

  -- creates file entry data by combining data, newtype and class entries
  let entryData = dtEntries ++ ntEntries ++ clsEntries
  
  -- guards determine how to handle overall file entry output
  -- for single line entry data vs. multi-line entry data
  let output = tEnt (length entryData) dtRefLines ntRefLines entryData
      tEnt 0 _ _ _          = entry
      tEnt 1 (x:_) _ _      = fstl x
      tEnt 1 _ (x:_) _      = fstl x
      tEnt 1 _ _ _          = entry
      tEnt _ (x:_) _ (_:xs) = fstl x ++ "\n" ++ sbE xs
      tEnt _ _ (x:_) (_:xs) = fstl x ++ "\n" ++ sbE xs
      tEnt _ _ _ (_:xs)     = entry ++ "\n" ++ sbE xs
      fstl x = join' entry x
      sbE xs = intercalate "\n" xs

  -- mapM_ print (lines output)
  return output

-- used to filter out info lines (i.e. removes comment and empty lines)
isInfoLine :: String -> Bool
isInfoLine line = (line /="") && not ("#" `isPrefixOf` line)

-- converts string to classtype (for use by config function)
toClassType :: String -> ClassType
toClassType "Haskell" = Haskell
toClassType "Drasil"  = Drasil
toClassType "GOOL"    = GOOL

-- gets folder from dictionary using folder name (iff it exists in dictionary)
getFolder :: Map.Map DC.FolderName DC.Folder -> DC.FolderName -> DC.Folder
getFolder dict name = fromJust $ Map.lookup name dict

-- converts list to dictionary list format (for use by drasil- directories only)
toDictList :: DC.Folder -> (DC.FolderName,DC.Folder)
toDictList folder = (DC.folderDrasilPack folder,folder) 

-- gets raw Entries, extracts classes and orders them with config file settings
ordClasses :: [ClassType] -> [Entry] -> [Class]
ordClasses cIO rED = concatMap (getClasses hslCls drlCls glCls) cIO where
  (hslCls,drlCls,glCls) = sortClasses dfCls instClsNms
  -- dfCls = all defined classes; instClsNms = all instanced class names
  dfCls = concatMap classes rED
  instClsNms = nub . map clsInstName $ concatMap classInstances rED

-- gets lists of all defined/instanced classes; sorts by class type
sortClasses :: [Class] -> [ClassName] -> ([Class],[Class],[Class])
sortClasses dCls iClsN = (haskellCls,drasilCls,goolCls) where
  -- partitions defined classes into GOOL and Drasil type classes
  (goolCls,drasilCls) = partition (isTypeOf GOOL) dCls
  -- gets defined classes names; used to isolate for haskell classes
  goolClsNms = map className goolCls
  drasilClsNms = map className drasilCls
  haskellClsNms = iClsN \\ (goolClsNms ++ drasilClsNms)
  -- makes new Haskell type classes
  haskellCls = map (makeClass Haskell) haskellClsNms

-- outputs class instance group; used by ordClasses to order the class instances 
-- by class type (as defined in config file settings)
getClasses :: [Class] -> [Class] -> [Class] -> ClassType -> [Class]
getClasses h _ _ Haskell = h
getClasses _ d _ Drasil  = d
getClasses _ _ g GOOL    = g

-- compares file instance in master list with file Instances List; replaces 
-- instance if in list with "Y" else "\t"
isInstanceOf :: [FileInstance] -> FileInstance -> IsInstanceOf
isInstanceOf fileInstances fileInstance = if isInstance then yes else no where
  isInstance = fileInstance `elem` fileInstances
  yes = "YYYY" :: IsInstanceOf
  no = "\t" :: IsInstanceOf

-- tests if class is of either the Haskell, Drasil or GOOL type (ClassType)
isTypeOf :: ClassType -> Class -> Bool
isTypeOf classTypeName classD = classType classD == classTypeName

-- tests if ClassInstance is of the data/newtype typename (DNType)
isTypeOf_ :: DNType -> ClassInstance -> Bool
isTypeOf_ typeName classInstance = dnType classInstance == typeName

-- joins two strings together with ","
join' :: String -> String -> String
join' a b = a ++ "," ++ b

-- gets class instance reference names for a data/newtype typename (DNType)
getRefNames :: [ClassInstance] -> DNType -> [ClassName]
getRefNames clsInsts tn = map clsInstName $ filter (isTypeOf_ tn) clsInsts

-- class instance ref lines (EntryString fragments) for each data/newtype line
createRefLines :: [ClassName] -> [[ClassName]] -> [EntryString]
createRefLines classInsts instRefNames = map (intercalate ",") instRefs where
  instRefs = zipWith (map . isInstanceOf) instRefNames instanceSkeleton 
  instanceSkeleton = replicate (length instRefNames) classInsts
