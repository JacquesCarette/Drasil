module DirectoryController (createFolder, createFile, finder, finder2, 
  getDirectories, getDirectories2, stripPath, DrasilPack, FileName, FolderName, 
  File(..), Folder(..)) where

import Data.List
import System.IO
import System.Directory

type FilterPrefix = String
type DrasilPack = String
type FileName = FilePath
type FolderName = FilePath

-- File and Folder data types for storing file drasil- package, name and filepath
data File = File { fileDrasilPack :: DrasilPack
                 , fileName :: FileName
                 , filePath :: FilePath
                 } deriving (Show)

data Folder = Folder { folderDrasilPack :: DrasilPack
                     , folderName :: FolderName
                     , folderPath :: FilePath
                     } deriving (Show)

-- used to create new folders of folder data type
createFolder :: FilePath -> DrasilPack -> FolderName -> Folder
createFolder dp drpk fn = Folder {folderDrasilPack=drpk,folderName=fn,folderPath=dp}

-- used to create new files of file data type
createFile :: FilePath -> DrasilPack -> FileName -> File
createFile fp drpk fn = File {fileDrasilPack=drpk,fileName=fn,filePath=fp}

-- iterates through each drasil- package and outputs subdirectories and haskell files
iterator :: FilePath -> IO ([FolderName],[FileName])
iterator nameFilePath = do
  let [fn,wd] = words nameFilePath
  setCurrentDirectory wd
  fc <- listDirectory fn

  let fcs = (a,b)
      a = map ((++fn) . (++"/") . (++wd) . (++" ")) $ fc \\ filter (isInfixOf ".") fc
      b = map ((++fn) . (++"/") . (++wd) . (++" ")) $ filter (isSuffixOf ".hs") fc
  return fcs

-- iterates through each drasil- package and outputs subdirectories and haskell files
iterator2 :: Folder -> IO ([Folder],[File])
iterator2 folder = do
  setCurrentDirectory (folderPath folder)
  rawContents <- listDirectory (folderName folder)

  let bakedContents = (folders,files)
      folders = map (createFolder workingDirectory currentDrasilPack) folderNames
      files = map (createFile workingDirectory currentDrasilPack) fileNames

      folderNames = rawContents \\ filter (isInfixOf ".") rawContents
      fileNames = filter (isSuffixOf ".hs") rawContents

      workingDirectory = getFolderPath folder
      currentDrasilPack = folderDrasilPack folder
  return bakedContents

-- searches for all folders and files in a directory (recursive search utilizing iterator function)
finder :: FolderName -> IO [FileName]
finder folderName = do
  rawData <- iterator folderName

  let rawFolders = fst rawData
  folders <- verifyDirectories rawFolders
  rawFiles <- mapM finder folders

  let bakedFiles
        | null rawFiles = []
        | otherwise = concat rawFiles

  let files
        | not (null folders) = snd rawData ++ bakedFiles
        | otherwise = snd rawData
  return files

-- searches for all folders and files in a directory (recursive search utilizing iterator function)
finder2 :: Folder -> IO [File]
finder2 folder = do
  rawData <- iterator2 folder

  let rawFolders = fst rawData
  folders <- verifyDirectories2 rawFolders
  rawFiles <- mapM finder2 folders

  let bakedFiles
        | null rawFiles = []
        | otherwise = concat rawFiles

  let files 
        | null folders = snd rawData
        | otherwise = snd rawData ++ bakedFiles
  return files

-- gets all drasil- packages + filepaths in a list
getDirectories :: FilePath -> FilterPrefix -> IO [FilePath]
getDirectories directoryPath filterPrefix = do
  all <- listDirectory directoryPath
  -- filters all drasil- packages
  let filtered = map ((++ directoryPath) . (++" ")) $ filter (isPrefixOf filterPrefix) all
  return filtered

-- gets all drasil- packages + filepaths in a list of folder data types
getDirectories2 :: FilePath -> FilterPrefix -> IO [Folder]
getDirectories2 directoryPath filterPrefix = do
  -- all raw directory contents
  all <- listDirectory directoryPath
  -- raw drasil- package directories + package names
  let rawPackages = filter (isPrefixOf filterPrefix) all
      packageNames = map (\\"drasil-") rawPackages
  -- convert list of directories into folder data types
      directories = zipWith (createFolder directoryPath) packageNames rawPackages
  return directories

-- verifies that each folder/directory exists
verifyDirectories :: [FilePath] -> IO [FilePath]
verifyDirectories rawFolders = do
  let rawDirectories = map (joins . words) rawFolders
  boolFolders <- mapM doesDirectoryExist rawDirectories
  let verifiedDirectories = snd $ partition null (zipWith fBool boolFolders rawFolders)
  return verifiedDirectories

verifyDirectories2 :: [Folder] -> IO [Folder]
verifyDirectories2 rawFolders = do
  let rawDirectories = map getFolderPath rawFolders
  boolFolders <- mapM doesDirectoryExist rawDirectories
  let verifiedDirectories = snd $ partition nullFolder (zipWith fBool2 boolFolders rawFolders)
  return verifiedDirectories

-- combines lists of Booleans and FilePaths (if True, FilePath exists)
fBool :: Bool -> FilePath -> FilePath
fBool b s = if b then s else ""

-- combines lists of Booleans and Folders (if True, Folder exists)
fBool2 :: Bool -> Folder -> Folder
fBool2 b f = if b then f else (createFolder "" "" "")

-- checks if a folder is null (empty)
nullFolder :: Folder -> Bool
nullFolder folder
  | folderName folder == "" && folderPath folder == "" && folderDrasilPack folder == "" = True
  | otherwise = False

-- combines folder name with filepath (for testing if directory exists)
joins :: [FilePath] -> FilePath
joins (a:b:_) = b ++ "/" ++ a

getFolderPath :: Folder -> FilePath
getFolderPath folder = folderPath folder ++ "/" ++ folderName folder

-- strips pathSuffix from filepath, returning stripped filepath (iff pathSuffix is suffix of filepath)
stripPath :: FilePath -> FilePath -> IO FilePath
stripPath filePath pathSuffix = do
  let strippedPath
        | pathSuffix `isSuffixOf` filePath = take (length filePath - length pathSuffix) filePath
        | otherwise = filePath
  return strippedPath

