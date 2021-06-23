module SourceCodeReaderTypes (extractEntryData, EntryData(..), DataDeclRecord(..), DataDeclConstruct(..), NewtypeDecl(..), TypeDecl(..)) where

import Data.List
import System.IO
import System.Directory
import qualified Data.Text as T
import qualified Data.List.Split as L
import Data.Maybe (fromJust)

import DirectoryController as DC (FileName)

-- Synonyms for clarity
type DataName = String
type NewtypeName = String
type TypeName = String

-- purposefully use different type names even though they will end up in the same form (for now)
data DataDeclRecord = DDR { ddrName :: DataName                       -- this can actually be any kind of type, but use DataName for clarity (same with below)
                          , ddrContent :: [DataName]} deriving (Show) -- same with this
data DataDeclConstruct = DDC { ddcName :: DataName
                          , ddcContent :: [DataName]} deriving (Show)
data NewtypeDecl = NTD { ntdName :: NewtypeName
                          , ntdContent :: [NewtypeName]} deriving (Show)
data TypeDecl = TD { tdName :: TypeName
                    , tdContent :: [TypeName]} deriving (Show)


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

      -- light cleanup of the files before sorting by datatype
  let scriptFileLines = removeDeriving $ removeNewlineGuard $ removeNewlineBrace $ removeNewlineEqual $ filterEmptyS $ filterComments $ map stripWS $ lines scriptFile

      -- convert the raw files into nicer strings by picking only the information we want
      dataTypesRec = removeNewlineComma $ useConstructFormRec False $ removeComments $ map fst $ filter snd $ isDataRec scriptFileLines False
      dataTypesConst = removeComments $ filter isDataConst $ map useEqForm $ useGuardForm scriptFileLines
      newtypeRec = removeComments $ isNewtypeRec $ filter (isPrefixOf "newtype ") scriptFileLines
      newtypeConst = removeComments $ isNewtypeConst $ filter (isPrefixOf "newtype ") scriptFileLines
      typeTypes = removeComments $ filter (isInfixOf "=") $ filter (isPrefixOf "type ") scriptFileLines

      -- organize the data from a nice string into their respective Decl formats
  let dataDeclRec = getDataContainedRec $ sortDataRec $ filterEmptyS $ L.splitOn "}\n" $ unlines $ dataTypesRec
      dataDeclConst = getDataContainedConst $ sortDataConst dataTypesConst
      newtypeDeclR = getNewtypesR newtypeRec
      newtypeDeclC = getNewtypesC newtypeConst
      newtypeDecl = newtypeDeclR ++ newtypeDeclC
      typeDecl = getTypes typeTypes

  -- returns all types within a file
  return EntryData {dRNs=dataDeclRec,dCNs=dataDeclConst,ntNs=newtypeDecl,tNs=typeDecl}

-- strips leading and trailing whitespace from strings
stripWS :: String -> String
stripWS = T.unpack . T.strip . T.pack

---TODO: use map for most of these, I just need to visualize what is happening for now.
-- TODO: organize and comment functions.

useConstructFormRec :: Bool -> [String]  -> [String]
useConstructFormRec _ [] = []
useConstructFormRec isSameRec (l1:l2:ls) 
  | "data " `isPrefixOf` l1 && "where " `isInfixOf` l1 = useConstructFormRec True ((l1Construct ++ l2) : ls)
  | "}" `isInfixOf` l2 && "->" `isInfixOf` l2 = useConstructFormRec False ((l1 ++ "}") : ls) -- may need to be changed to account for other possible variance, but for now it should work
  | isSameRec = useConstructFormRec True ((l1 ++ l2):ls)
  | otherwise = l1 : useConstructFormRec False (l2:ls)
  where
      l1Construct 
        | "=>" `isInfixOf` l1 = (T.unpack $ T.replace (T.pack "where") (T.pack "=") $ T.pack $ unwords $ take (fromJust (findIndex  (isInfixOf "::") (words l1))) $ words l1) ++ " {" --has a class constraint, but we can just ignore for now
        | otherwise = (T.unpack $ T.replace (T.pack "where") (T.pack "=") $ T.pack l1) ++ " {" -- no class constraint in data type
useConstructFormRec _ ls = ls

useEqForm :: String -> String
useEqForm = T.unpack . T.replace (T.pack "where") (T.pack "=") . T.pack

isNewtypeRec :: [String] -> [String]
isNewtypeRec = filter (isInfixOf "{")

isNewtypeConst :: [String] -> [String]
isNewtypeConst = filter (not . isInfixOf "{")

filterAll :: String -> String
filterAll = filterPrimeTypes . filterQualifiedTypes . filterInvalidChars

filterInvalidChars :: String -> String
filterInvalidChars = filterInvalidChars' invalidChars
  where
    filterInvalidChars' [] x = x
    filterInvalidChars' (l:ls) x = filterInvalidChars' ls $ filter (/= l) x
    invalidChars = "[]!} (){->,$" --this space is kind of a hack, but here to get it to work for now

filterPrimeTypes :: String -> String
filterPrimeTypes = T.unpack . T.replace (T.pack "\'") (T.pack "_") . T.pack 

filterQualifiedTypes :: String -> String
filterQualifiedTypes l = if '.' `elem` l then drop (fromJust (elemIndex '.' l)+1) l else l -- get rid of types that are made from qualified imports

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
getNewtypesR (l:ls) = NTD {ntdName = filterAll $ head typeContents, ntdContent = nub $ filter (not . null) $ map filterAll checkContents} : getNewtypesR ls
  where typeContents = map stripWS $ L.splitOn "=" $ l \\ "newtype "
        checkContents = map stripWS $ concatMap (tail) $ map (L.splitOn "::") $ L.splitOn "," $ concat $ tail typeContents

getNewtypesC :: [String] -> [NewtypeDecl]
getNewtypesC [] = []
getNewtypesC (l:ls) = NTD {ntdName = filterAll $ head typeContents, ntdContent = nub $ filter (not . null) $ map filterAll checkContents} : getNewtypesC ls
    where typeContents = map stripWS $ L.splitOn "=" $ l \\ "newtype "
          checkContents = concatMap (tail.words) $ tail typeContents

getTypes :: [String] -> [TypeDecl]
getTypes [] = []
getTypes (l:ls) = TD {tdName = filterAll $ head typeContents, tdContent = nub $ filter (not . null) $ map filterAll $ tail typeContents} : getTypes ls
    where typeContents = map stripWS $ L.splitOn "=" $ l \\ "type "
          --checkContents = if "->" `isInfixOf` (concat $ tail typeContents) then error $ show typeContents else tail typeContents

-- Helper that takes a list of Strings and arranges it so that a list of the data and the datatype constructor values is made.
sortDataConst :: [String] -> [(String, [String])]
sortDataConst [] = []
sortDataConst (l:ls) = (dataCName, dataCContents): sortDataConst ls
    where dataCName = head $ map stripWS $ L.splitOneOf "|=" $ l \\ "data "
          dataCContents = concatMap tail $ map words $ tail $ filter (not . null) $ map stripWS $ L.splitOneOf "|=" $ l \\ "data " 

-- Creates a data declaration for constructs.
getDataContainedConst :: [(String, [String])] -> [DataDeclConstruct]
getDataContainedConst [] = []
getDataContainedConst (l:ls) = DDC {ddcName = filterAll $ fst l, ddcContent = nub $ filter (not . null) $ map filterAll $ snd l} : getDataContainedConst ls

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
getDataContainedRec (l:ls) = DDR {ddrName = filterAll $ fst l, ddrContent = nub $ filter (not . null) $ map filterAll $ snd l} : getDataContainedRec ls

--for testing, will be removed after
mytail :: [a] -> [a]
mytail (x:xs) = xs
mytail [] = error "here"

-- take a list of data declarations for records and get the name of the datatype and all datatypes within the record
sortDataRec :: [String] -> [(String, [String])]
sortDataRec [] = []
--sortDataRec [""] = [] --hack?
sortDataRec (l:ls) = (head typeContents, checkContents): sortDataRec ls -- sortDataRec' (head (L.splitOn "=" l) : tail (L.splitOn "=" l)) : sortDataRec ls
    where
        typeContents = map stripWS $ L.splitOn "=" $ l \\ "data "
        checkContents = map stripWS $ concatMap (tail) $ map (L.splitOn "::") $ L.splitOn "," $ concat $ tail typeContents

-- Attach booleans to see if a line is a part of a data type declaration (for records)
isDataRec :: [String] -> Bool -> [(String, Bool)]
isDataRec [] _ = [] 
isDataRec (l:ls) dtStill
    | "data " `isPrefixOf` l && "{" `isInfixOf` l && "}" `isInfixOf` l = (l, True): isDataRec ls False 
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

