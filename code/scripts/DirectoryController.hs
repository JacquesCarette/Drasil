module DirectoryController (iterator) where

import Data.List
import System.IO
import System.Directory

type EntryData = String
type FileName = FilePath

type ClassInstance = String

type FileInstance = String

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
