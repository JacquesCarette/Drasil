module SourceCodeReader (extractEntryData, extractEntryData2, EntryData) where

import Data.List
import System.IO
import System.Directory

import DirectoryController as DC (FileName)

type EntryData = String

-- extracts data, newtype and class names
extractEntryData :: DC.FileName -> FilePath -> IO [[EntryData]]
extractEntryData fileName filePath = do
  setCurrentDirectory filePath
  handle <- openFile fileName ReadMode
  scriptFile <- hGetContents handle
  forceRead scriptFile `seq` hClose handle
  -- removes comment lines
  let scriptFileLines = lines scriptFile \\ filter (isInfixOf "-- ") (lines scriptFile)
      dataTypes = filter (isPrefixOf "data ") scriptFileLines
      newtypeTypes = filter (isPrefixOf "newtype ") scriptFileLines
  -- derived classes (that use '=>') vs. defined classes
      (derivClass,definClass) = partition (isInfixOf " => ") (filter (isPrefixOf "class ") scriptFileLines)
      definInstances = filter (isPrefixOf "instance ") scriptFileLines

  let dataNames = map (takeWhile (/=' ') . (\\ "data ")) dataTypes
      newtypeNames = map (takeWhile (/=' ') . (\\ "newtype ")) newtypeTypes
      stripDeriv = map (takeWhile (/=' ') . (\\ "> ") . dropWhile (/='>')) derivClass
      stripDefin = map (takeWhile (/=' ') . (\\ "class ")) $ filter (isInfixOf "where") definClass
      classNames = stripDeriv ++ stripDefin
      stripInstances = zipWith (\a b -> b ++ " " ++ a) (map (!! 0) d) (map (!! 1) d) where
                       d = map (words . (\\ "instance ")) definInstances

  return [dataNames,newtypeNames,classNames,stripInstances]

-- extracts data, newtype and class names + instances (new format)
extractEntryData2 :: DC.FileName -> FilePath -> 
  IO ([EntryData],[EntryData],[EntryData],[(EntryData,EntryData)])
extractEntryData2 fileName filePath = do
  setCurrentDirectory filePath
  handle <- openFile fileName ReadMode
  scriptFile <- hGetContents handle
  forceRead scriptFile `seq` hClose handle
  -- removes comment lines
  let scriptFileLines = lines scriptFile \\ filter (isInfixOf "-- ") (lines scriptFile)
      dataTypes = filter (isPrefixOf "data ") scriptFileLines
      newtypeTypes = filter (isPrefixOf "newtype ") scriptFileLines
  -- derived classes (that use '=>') vs. defined classes
      (derivClass,definClass) = partition (isInfixOf " => ") (filter (isPrefixOf "class ") scriptFileLines)
      definInstances = filter (isPrefixOf "instance ") scriptFileLines

  let dataNames = map (takeWhile (/=' ') . (\\ "data ")) dataTypes
      newtypeNames = map (takeWhile (/=' ') . (\\ "newtype ")) newtypeTypes
      stripDeriv = map (takeWhile (/=' ') . (\\ "> ") . dropWhile (/='>')) derivClass
      stripDefin = map (takeWhile (/=' ') . (\\ "class ")) $ filter (isInfixOf "where") definClass
      classNames = stripDeriv ++ stripDefin
      stripInstances = zipWith (\a b -> (b,a)) (map (!! 0) d) (map (!! 1) d) where
                       d = map (words . (\\ "instance ")) definInstances

  return (dataNames,newtypeNames,classNames,stripInstances)

-- used to enforce strict file reading (so files can be closed, to avoid running out of memory)
forceRead :: [a0] -> ()
forceRead [] = ()
forceRead (x:xs) = forceRead xs

{- ; separate one for multi-line derived classes (ignore this block for now)
      (derivClass,definClass) = partition (isInfixOf " => ") (filter (isPrefixOf "class ") scriptFileLines)
      mLDerivClass = stripped3 \\ (filter (isInfixOf "(") stripped3)
      stripped3 = stripped2 \\ (filter (tailIsNotElemOf letters) stripped2)
      tailIsNotElemOf a b = if (last b) `notElem` a then True else False
      letters = ['a'..'z'] ++ ['A'..'Z']
      stripped2 = stripped \\ (filter (isInfixOf " -> ") stripped)
      stripped = (\\ (derivClass ++ funcList ++ classInstanceList)) $ filter (isInfixOf " => ") scriptFileLines
      funcList = union (filter (isInfixOf " -> ") funcListx) funcListx
      funcListx = filter (isInfixOf "::") $ filter (isInfixOf " => ") scriptFileLines
      classInstanceList = filter (isPrefixOf "instance ") $ filter (isInfixOf "=> ") scriptFileLines
      definInstances = filter (isPrefixOf "instance ") scriptFileLines
  -- print (fileName ++ filePath)
  print mLDerivClass
-}