module SourceCodeReader (extractEntryData) where

import Data.List
import System.IO
import System.Directory

type EntryData = String
type FileName = FilePath

type ClassInstance = String

type FileInstance = String
type IsInstanceOf = String

-- extracts data, newtype and class names
extractEntryData :: FileName -> FilePath -> IO [[String]]
extractEntryData fileName filePath = do
  setCurrentDirectory filePath
  scriptFile <- readFile fileName
  -- removes comment lines
  let scriptFileLines = lines scriptFile \\ filter (isInfixOf "-- ") (lines scriptFile)
      dataTypes = filter (isPrefixOf "data ") scriptFileLines
      newtypeTypes = filter (isPrefixOf "newtype ") scriptFileLines
  -- derived classes (that use '=>') vs. defined classes
      (derivClass,definClass) = partition (isInfixOf "=>") (filter (isPrefixOf "class ") scriptFileLines)
      definInstances = filter (isPrefixOf "instance ") scriptFileLines

  let dataNames = map (takeWhile (/=' ') . (\\ "data ")) dataTypes
      newtypeNames = map (takeWhile (/=' ') . (\\ "newtype ")) newtypeTypes
      stripDeriv = map (takeWhile (/=' ') . (\\ "> ") . dropWhile (/='>')) derivClass
      stripDefin = map (takeWhile (/=' ') . (\\ "class ")) definClass
      classNames = stripDeriv ++ stripDefin
      stripInstances = zipWith (\a b -> b ++ " " ++ a) (map (!! 0) d) (map (!! 1) d) where
                       d = map (words . (\\ "instance ")) definInstances
  
  -- print stripInstances
  -- instanceNames = 

  return [dataNames,newtypeNames,classNames,stripInstances]