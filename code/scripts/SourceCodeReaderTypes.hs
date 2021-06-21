module SourceCodeReader (extractEntryData, EntryData(..)) where

import Data.List
import System.IO
import System.Directory
import qualified Data.Text as T
import qualified Data.List as L

import DirectoryController as DC (FileName)

type DataName = String
type NewtypeName = String
type TypeName = String


-- purposefully use different type names even though they will end up in the same form
data DataDeclRecord = DDR { ddrName :: DataName
                          , ddrContent :: [DataName]} -- this can actually be any kind of type, but use DataName for simplicity
data DataDeclConstruct = DDC { ddcName :: DataName
                          , ddcContent :: [DataName]}
data NewtypeDecl = NTD { ntdName :: NewtypeName
                          , ntdContent :: NewtypeName} -- can only have one value in a constructor, but we don't care about the constructor
data TypeDecl = TD { tdName :: TypeName
                    , tdContent :: TypeName} -- should only be for a type synonym


-- new EntryData data type with strict fields to enforce strict file reading
data EntryData = EntryData { dRNs :: ![DataDeclRecord]    -- Record datatypes will be shown differently on a dot graph
                           , dCNs :: ![DataDeclConstruct] -- compared to datatypes that use constructors.
                           , ntNs :: ![NewtypeDecl]       -- Newtypes will be recorded as records (usually only wrap one other type)
                           , tNs :: ![TypeDecl]} deriving (Show) -- Types are usually synonyms of other types, so act like Record datatypes but in a different colour.

-- extracts data, newtype and class names + instances (new data-oriented format)
extractEntryData :: DC.FileName -> FilePath -> IO EntryData
extractEntryData fileName filePath = do
  setCurrentDirectory filePath
  handle <- openFile fileName ReadMode
  scriptFile <- hGetContents handle
  forceRead scriptFile `seq` hClose handle
  let rScriptFileLines = removeNewlineGuard $ removeNewlineBrace $ map stripWS $ lines scriptFile
  -- removes comment lines
      scriptFileLines = rScriptFileLines \\ filter (isPrefixOf "--") rScriptFileLines

      dataTypesRec = filter snd $ isDataRec scriptFileLines False
      dataTypesConst = filter isDataConst scriptFileLines
      newtypeTypes = filter (isPrefixOf "newtype ") scriptFileLines
      typeTypes = filter (isPrefixOf "type ") scriptFileLines


  let dataDeclRec = getDataContainedRec $ sortDataRec $ splitOn "}\n" $ unlines $ map fst dataTypesRec
      dataDeclConst = getDataContainedConst $ sortDataConst $ dataTypesConst 
      newtypeDecl = getNewtypes newtypeTypes
      typeDecl = getTypes typeTypes

  return EntryData {dRNs=dataDeclRec,dCNs=dataDeclConst,ntNs=newtypeDecl,tNs=typeDecl}

-- strips leading and trailing whitespace from strings
stripWS :: String -> String
stripWS = T.unpack . T.strip . T.pack

---TODO: use map for most of these, I just need to visualize what is happening for now.

getNewtypes :: [String] -> [NewtypeDecl]
getNewtypes [] = []
getNewtypes (l:ls) = NTD {ntdName = fst typeContents, ntdContent = checkContents} : getNewtypes ls
    where typeContents = map stripWS $ L.splitOn "=" $ l \\ "newtype "
          checkContents =  if length $ words $ snd typeContents > 1 then snd $ words $ snd typeContents else removeListType $ snd typeContents
          removeListType = reverse . tail . reverse . tail

getTypes :: [String] -> [TypeDecl]
getTypes [] = []
getTypes (l:ls) = TD {tdName = fst typeContents, tdContent = snd typeContents} : getTypes ls
    where typeContents = map stripWS $ L.splitOn "=" $ l \\ "type " 

-- Helper that takes a list of Strings and arranges it so that a list of the data and the datatype constructor values is made.
sortDataConst :: [String] -> [(String, [String])]
sortDataConst [] = [[]]
sortDataConst (l:ls) = (fst $ map stripWS $ L.splitOneOf "|=" $ l \\ "data ", concatMap tail $ words $ tail $ map stripWS $ L.splitOneOf "|=" $ l \\ "data "): sortDataConst ls

-- Creates a data declaration for constructs.
getDataContainedConst :: [(String, [String])] -> [DataDeclConstruct]
getDataContainedConst [] = []
getDataContainedConst (l:ls) = DDC {ddcName fst l, ddcContent snd l} : getDataContainedConst

--for those few cases of data declarations that use constructors with guards on a newline.
removeNewlineGuard :: [String] -> [String]
removeNewlineGuard [] = []
removeNewlineGuard (l1:l2:ls) = if "|" `isPrefixOf` l2 then removeNewlineGuard (concat l1 (" ":l2): ls) else l1 : removeNewlineGuard (l2:ls)
removeNewlineGuard ls = ls

-- for those few cases of data declarations that are record types where the "{" is on a newline.
removeNewlineBrace :: [String] -> [String]
removeNewlineBrace [] = []
removeNewlineBrace (l1:l2:ls) = if "data " `isPrefixOf` l1 && "{" `isPrefixOf` l2 then concat l1 l2 : removeNewline ls else l1:l2:removeNewline ls
removeNewlineBrace ls = ls

-- make a data declaration record out of the name of the type and the names of the contained types
getDataContainedRec :: [(String, [String])] -> [DataDeclRecord]
getDataContainedRec [] = [] 
getDataContainedRec (l:ls) = DDR {ddrName = fst l, ddrContent = snd l} : getDataConatinedRec ls

-- take a list of data declarations for records and get the name of the datatype and all datatypes within the record
sortDataRec :: [String] -> [(String, [String])]
sortDataRec [] = []
sortDataRec (l:ls) = sortDataRec' ((splitOn "=" (fst (unlines l))) : tail (unlines l)) : sortDataRec ls
    where
        sortDataRec' :: [String] -> (String, [String])
        sortDataRec' ms = (stripWS (fst ms \\ "data "), getContainedTypes $ tail ms)
        getContainedTypes :: [String] -> [String]
        getContainedTypes [] = []
        getContainedTypes (n:ns) = stripWS . snd $ L.splitOn "::" n : getContainedTypes ns

-- Attach booleans to see if a line is a part of a data type declaration (for records)
isDataRec :: [String] -> Bool -> [(String, Bool)]
isDataRec [] _ = [] 
isDataRec (l:ls) dtStill
    | "data " `isPrefixOf` l && "{" `elem` l = (l, True): isDataRec ls True -- if start with "data " and contains "{", it is start of record datatype
    | dtStill && "}" `elem` l = (l, True): isDataRec ls False -- if still part of record datatype and we see "}", dataType ends
    | dtStill = (l, True) : isDataRec ls True -- if still part of record datatype and none of the above happen, keep going with datatype
    | otherwise = (l, False) : isDataRec ls False -- otherwise, it is not a datatype and keep going as if the next line is not a datatype

-- for checking data types that are not records
isDataConst :: String -> Bool
isDataConst dt = if (isPrefixOf "data " && not ("{" `elem` dt)) then True else False

-- enforces strict file reading; files can be closed to avoid memory exhaustion
forceRead :: [a0] -> ()
forceRead [] = ()
forceRead (x:xs) = forceRead xs

-- index number, class + script lines, indexes list (for multi-line classes)
getIndexes :: Int -> [String] -> [String] -> [Int]
getIndexes _ _ [] = []
getIndexes idx clsLines (x:xs) = if isClassLine then addIdx else nextIdx where
  isClassLine = x `elem` clsLines
  addIdx = idx : nextIdx
  nextIdx = getIndexes (idx + 1) clsLines xs

-- used to extract the class name from a raw script line
getClassName :: String -> ClassName
getClassName rsl = if derived then stripDv else stripDf where
  derived = "=>" `isInfixOf` rsl
  -- operates on derived classes
  stripDv = takeWhile (/=' ') . (\\ "> ") $ dropWhile (/='>') rsl
  -- operates on defined classes
  stripDf = takeWhile (/=' ') $ (\\ "class ") rsl

-- used to extract data/newtype name + class instance name
getStripInstance :: String -> (DtNtName,ClassName)
getStripInstance rsl = if derived then stripDv else stripDf where
  derived = "=>" `isInfixOf` rsl
  -- operates on derived class instances
  stripDv
    | "(" `isInfixOf` rsl = (stripDvLmdn,stripDvLmc)
    | otherwise = (stripDvLs !! 1,head stripDvLs)
  stripDvLs = words . (\\ "> ") $ dropWhile (/='>') rsl
  stripDvLm = (\\ "> ") $ dropWhile (/='>') rsl
  stripDvLmdn = takeWhile (/=')') . (\\ "(") $ dropWhile (/='(') stripDvLm
  stripDvLmc = takeWhile (/=' ') stripDvLm
  -- operates on defined class instances
  stripDf = (stripDfL !! 1,head stripDfL)
  stripDfL = words $ (\\ "instance ") rsl
