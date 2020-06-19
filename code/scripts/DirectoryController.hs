module DirectoryController (iterator, iterator2, finder) where

import Data.List
import System.IO
import System.Directory

type EntryData = String
type FileName = FilePath
type FolderName = FilePath

type ClassInstance = String

type FileInstance = String

-- iterates through each drasil- package and outputs subdirectories and haskell files
iterator :: [FilePath] -> IO [a0]
iterator (x:xs) = do 
  setCurrentDirectory "/Users/Nathaniel_Hu/Documents/GitHub/Drasil/code"
  some <- listDirectory x
  wd <- getCurrentDirectory
  
  let l = filter (isInfixOf ".") some
      d = map ((++ x) . (++"/") . (++ wd) . (++" ")) $ (some \\ l)
      h = filter (isSuffixOf ".hs") some
  
  print x -- << drasil- package
  print d -- << sub-folders
  print h -- << haskell files

  if xs /= [] then iterator xs
  else return []

-- iterates through each drasil- package and outputs subdirectories and haskell files
iterator2 :: FilePath -> IO [[FileName]]
iterator2 filePath = do
  let [fn,wd] = words filePath
  setCurrentDirectory wd
  fc <- listDirectory fn

  let fcs = [a,b]
      a = map ((++fn) . (++"/") . (++wd) . (++" ")) $ fc \\ (filter (isInfixOf ".") fc)
      b = map ((++fn) . (++"/") . (++wd) . (++" ")) $ filter (isSuffixOf ".hs") fc
  return fcs

finder :: FilePath -> FolderName -> IO [FileName]
finder filePath x = do
  setCurrentDirectory filePath
  all <- listDirectory x

  let l = filter (isInfixOf ".") all
      d = map ((++ x) . (++"/") . (++ filePath) . (++" ")) $ (all \\ l)
      h = filter (isSuffixOf ".hs") all

  print x -- << drasil- package
  print d -- << sub-folders
  print h -- << haskell files

  return h
