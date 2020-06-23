module DirectoryController (finder, getDirectories, FileName, FolderName) where

import Data.List
import System.IO
import System.Directory

type FilterPrefix = String
type FileName = FilePath
type FolderName = FilePath

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

-- gets all drasil- packages + filepaths in a list
getDirectories :: FilePath -> FilterPrefix -> IO [FilePath]
getDirectories directoryPath filterPrefix = do
  all <- listDirectory directoryPath
  -- filters all drasil- packages
  let filtered = map ((++ directoryPath) . (++" ")) $ filter (isPrefixOf filterPrefix) all
  return filtered

-- verifies that each folder/directory exists
verifyDirectories :: [FilePath] -> IO [FilePath]
verifyDirectories rawFolders = do
  let rawDirectories = map (joins . words) rawFolders
  boolFolders <- mapM doesDirectoryExist rawDirectories
  let verifiedDirectories = snd $ partition null (zipWith fBool boolFolders rawFolders)
  return verifiedDirectories

-- combines lists of Booleans and FilePaths (if True, FilePath exists)
fBool :: Bool -> FilePath -> FilePath
fBool b s = if b then s else ""

-- combines folder name with filepath (for testing if directory exists)
joins :: [FilePath] -> FilePath
joins (a:b:_) = b ++ "/" ++ a
