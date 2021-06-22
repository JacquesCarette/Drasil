module SourceCodeReaderTypes (extractEntryData, EntryData(..), DataDeclRecord(..), DataDeclConstruct(..), NewtypeDecl(..), TypeDecl(..)) where

import Data.List
import System.IO
import System.Directory
import qualified Data.Text as T
import qualified Data.List.Split as L
import Data.Maybe (fromJust)

import DirectoryController as DC (FileName)

type DataName = String
type NewtypeName = String
type TypeName = String

{--Copied from Data.List.Index. How to import?
insertAt :: Int -> a -> [a] -> [a]
insertAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 xs     = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []-}


-- purposefully use different type names even though they will end up in the same form
data DataDeclRecord = DDR { ddrName :: DataName
                          , ddrContent :: [DataName]} deriving (Show)-- this can actually be any kind of type, but use DataName for simplicity
data DataDeclConstruct = DDC { ddcName :: DataName
                          , ddcContent :: [DataName]} deriving (Show)
data NewtypeDecl = NTD { ntdName :: NewtypeName
                          , ntdContent :: [NewtypeName]} deriving (Show) -- can only have one value in a constructor, but we don't care about the constructor
data TypeDecl = TD { tdName :: TypeName
                    , tdContent :: [TypeName]} deriving (Show)-- should only be for a type synonym


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
  let scriptFileLines = removeDeriving $ removeNewlineGuard $ removeNewlineBrace $ removeNewlineEqual $ filterEmptyS $ filterComments $ map stripWS $ lines scriptFile
  -- removes comment lines
      --scriptFileLines = rScriptFileLines \\ filter (isPrefixOf "--") rScriptFileLines
      -- Recorded types as a String. Contains multiple, so it is a list of strings.
      dataTypesRec = removeNewlineComma $ removeComments $ map fst $ filter snd $ isDataRec scriptFileLines False
      dataTypesConst = removeComments $ filter isDataConst $ useGuardForm scriptFileLines
      newtypeRec = removeComments $ isNewtypeRec $ filter (isPrefixOf "newtype ") scriptFileLines
      newtypeConst = removeComments $ isNewtypeConst $ filter (isPrefixOf "newtype ") scriptFileLines
      typeTypes = removeComments $ filter (not . isInfixOf "=") $ filter (isPrefixOf "type ") scriptFileLines


  let dataDeclRec = getDataContainedRec $ sortDataRec $ filterEmptyS $ L.splitOn "}\n" $ unlines $ dataTypesRec
      dataDeclConst = getDataContainedConst $ sortDataConst dataTypesConst
      newtypeDeclR = getNewtypesR newtypeRec
      newtypeDeclC = getNewtypesC newtypeConst
      newtypeDecl = newtypeDeclR ++ newtypeDeclC
      typeDecl = getTypes typeTypes

  return EntryData {dRNs=dataDeclRec,dCNs=dataDeclConst,ntNs=newtypeDecl,tNs=typeDecl}

-- strips leading and trailing whitespace from strings
stripWS :: String -> String
stripWS = T.unpack . T.strip . T.pack

isNewtypeRec :: [String] -> [String]
isNewtypeRec = filter (isInfixOf "{")

isNewtypeConst :: [String] -> [String]
isNewtypeConst = filter (not . isInfixOf "{")

{-fromRecordForm :: [String] -> [String]
fromRecordForm [] = []
fromRecordForm (l:ls)                                                   -- FIXME: super hacky but using just to get it to work for now
  | "newtype " `isPrefixOf` l && "{" `isInfixOf` l && "=" `isInfixOf` l = unwords (head (L.splitOn "=" l) : (error $ show $ concatMap tail $ map (L.splitOn "::") $ tail $ L.splitOn "=" l)) : fromRecordForm ls -- (unwords $ insertAt 2 "=" $ insertAt 2 "Constructor" $ take (fromJust (elemIndex "=" (words l))) $ error $ show $ words l) : fromRecordForm ls
  | otherwise = l : fromRecordForm ls-}

---TODO: use map for most of these, I just need to visualize what is happening for now.

filterInvalidChars :: String -> String
filterInvalidChars = filterInvalidChars' invalidChars
  where
    filterInvalidChars' [] x = x
    filterInvalidChars' (l:ls) x = filterInvalidChars' ls $ filter (/= l) x
    invalidChars = "[]!}"

removeNewlineComma :: [String] -> [String]
removeNewlineComma [] = []
removeNewlineComma (l1:l2:ls)
  | "," `isPrefixOf` l2 = removeNewlineComma ((l1 ++ " " ++ l2) : ls)
  | otherwise = l1 : removeNewlineComma (l2:ls)
removeNewlineComma ls = ls

removeComments :: [String] -> [String]
removeComments [] = []
removeComments (l:ls)
  | "--" `isInfixOf` l = (unwords $ take (fromJust (findIndex  (isInfixOf "--") (words l))) $ words l): removeComments ls
  | otherwise = l : removeComments ls

removeDeriving :: [String] -> [String]
removeDeriving [] = []
removeDeriving (l:ls) 
  | "deriving " `isInfixOf` l = (unwords $ take (fromJust (elemIndex "deriving" (words l))) $ words l): removeDeriving ls
  | otherwise = l : removeDeriving ls

filterComments :: [String] -> [String]
filterComments ls = ls \\ filter (isPrefixOf "--") ls

filterEmptyS :: [String] -> [String]
filterEmptyS ls = filter (/= "") ls

useGuardForm :: [String] -> [String]
useGuardForm [] = []
useGuardForm (l1:l2:ls) = if "::" `isInfixOf` l2 then useGuardForm ((l1 ++ l2Guard) : ls) else l1: useGuardForm (l2:ls)
    where l2Guard = " |" ++ (T.unpack $ T.replace (T.pack "::") mempty $ T.replace (T.pack "->") mempty $ T.pack l2)
useGuardForm ls = ls

getNewtypesR :: [String] -> [NewtypeDecl]
getNewtypesR [] = []
getNewtypesR (l:ls) = NTD {ntdName = head typeContents, ntdContent = nub $ map filterInvalidChars checkContents} : getNewtypesR ls
  where typeContents = map stripWS $ L.splitOn "=" $ l \\ "newtype "
        checkContents = map stripWS $ concatMap (tail) $ map (L.splitOn "::") $ L.splitOn "," $ concat $ tail typeContents

getNewtypesC :: [String] -> [NewtypeDecl]
getNewtypesC [] = []
getNewtypesC (l:ls) = NTD {ntdName = head typeContents, ntdContent = nub $ map filterInvalidChars checkContents} : getNewtypesC ls
    where typeContents = map stripWS $ L.splitOn "=" $ l \\ "newtype "
          checkContents = concatMap (tail.words) $ tail typeContents

getTypes :: [String] -> [TypeDecl]
getTypes [] = []
getTypes (l:ls) = TD {tdName = head typeContents, tdContent = nub $ map filterInvalidChars $ tail typeContents} : getTypes ls
    where typeContents = map stripWS $ L.splitOn "=" $ l \\ "type "
          --checkContents = if "->" `isInfixOf` (concat $ tail typeContents) then error $ show typeContents else tail typeContents

-- Helper that takes a list of Strings and arranges it so that a list of the data and the datatype constructor values is made.
sortDataConst :: [String] -> [(String, [String])]
sortDataConst [] = []
sortDataConst (l:ls) = (dataCName, dataCContents): sortDataConst ls
    where dataCName = head $ map stripWS $ L.splitOneOf "|=" $ l \\ "data "
          dataCContents = concatMap tail $ map words $ tail $ map stripWS $ L.splitOneOf "|=" $ l \\ "data " 

{-filterDeriv :: [String] -> [String]
filterDeriv [] = []
filterDeriv (l:ls) 
  | "deriving" `isInfixOf` l = []
  | otherwise = l: filterDeriv ls-}

-- Creates a data declaration for constructs.
getDataContainedConst :: [(String, [String])] -> [DataDeclConstruct]
getDataContainedConst [] = []
getDataContainedConst (l:ls) = DDC {ddcName = fst l, ddcContent = nub $ map filterInvalidChars $ snd l} : getDataContainedConst ls

removeNewlineEqual :: [String] -> [String]
removeNewlineEqual [] = []
removeNewlineEqual (l1:l2:ls) = if ("data " `isPrefixOf` l1 || "type " `isPrefixOf` l1 || "newtype " `isPrefixOf` l1) && "=" `isSuffixOf` l1 then removeNewlineEqual ((l1 ++ " " ++ l2): ls) else l1 : removeNewlineEqual (l2:ls)
removeNewlineEqual ls = ls

--for those few cases of data declarations that use constructors with guards on a newline.
removeNewlineGuard :: [String] -> [String]
removeNewlineGuard [] = []
removeNewlineGuard (l1:l2:ls) = if "|" `isPrefixOf` l2 then removeNewlineGuard ((l1 ++ " " ++ l2): ls) else l1 : removeNewlineGuard (l2:ls)
removeNewlineGuard ls = ls

-- for those few cases of data declarations that are record types where the "{" is on a newline.
removeNewlineBrace :: [String] -> [String]
removeNewlineBrace [] = []
removeNewlineBrace (l1:l2:ls) = if ("data " `isPrefixOf` l1 || "newtype " `isPrefixOf` l1)  && "{" `isInfixOf` l2 then (l1 ++ " " ++ l2) : removeNewlineBrace ls else l1:removeNewlineBrace (l2:ls)
removeNewlineBrace ls = ls

-- make a data declaration record out of the name of the type and the names of the contained types
getDataContainedRec :: [(String, [String])] -> [DataDeclRecord]
getDataContainedRec [] = [] 
getDataContainedRec (l:ls) = DDR {ddrName = fst l, ddrContent = nub $ map filterInvalidChars $ snd l} : getDataContainedRec ls

--for testing, will be removed after
mytail :: [a] -> [a]
mytail (x:xs) = xs
mytail [] = error "here"

{-sortDataConst (l:ls) = (dataCName, dataCContents): sortDataConst ls
    where dataCName = head $ map stripWS $ L.splitOneOf "|=" $ l \\ "data "
          dataCContents = concatMap tail $ map words $ tail $ map stripWS $ L.splitOneOf "|=" $ l \\ "data "-}

-- take a list of data declarations for records and get the name of the datatype and all datatypes within the record
sortDataRec :: [String] -> [(String, [String])]
sortDataRec [] = []
--sortDataRec [""] = [] --hack?
sortDataRec (l:ls) = (head typeContents, checkContents): sortDataRec ls -- sortDataRec' (head (L.splitOn "=" l) : tail (L.splitOn "=" l)) : sortDataRec ls
    where
        typeContents = map stripWS $ L.splitOn "=" $ l \\ "data "
        checkContents = map stripWS $ concatMap (tail) $ map (L.splitOn "::") $ L.splitOn "," $ concat $ tail typeContents
        {-sortDataRec' :: [String] -> (String, [String])
        --sortDataRec' [] = ("", [])
        sortDataRec' ms = ("", getContainedTypes $ tail ms)
        getContainedTypes :: [String] -> [String]
        getContainedTypes [n] = []
        getContainedTypes [] = [] --for those files which do not contain any record types
        getContainedTypes ns = error $ show (l ++ "\nSomething isn't right here.")-} --"Should only have one string filled with the different needed types at this stage."

-- Attach booleans to see if a line is a part of a data type declaration (for records)
isDataRec :: [String] -> Bool -> [(String, Bool)]
isDataRec [] _ = [] 
isDataRec (l:ls) dtStill
    | "data " `isPrefixOf` l && "{" `isInfixOf` l = (l, True): isDataRec ls True -- if start with "data " and contains "{", it is start of record datatype
    | dtStill && "}" `isInfixOf` l = (l, True): isDataRec ls False -- if still part of record datatype and we see "}", dataType ends
    | dtStill = (l, True) : isDataRec ls True -- if still part of record datatype and none of the above happen, keep going with datatype
    | otherwise = (l, False) : isDataRec ls False -- otherwise, it is not a datatype and keep going as if the next line is not a datatype

-- for checking data types that are not records
isDataConst :: String -> Bool
isDataConst dt = if (isPrefixOf "data " dt && not ("{" `isInfixOf` dt)) then True else False

-- enforces strict file reading; files can be closed to avoid memory exhaustion
forceRead :: [a0] -> ()
forceRead [] = ()
forceRead (x:xs) = forceRead xs

{-- index number, class + script lines, indexes list (for multi-line classes)
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
  stripDfL = words $ (\\ "instance ") rsl-}
