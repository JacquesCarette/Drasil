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
  let dataDeclRec = getDataContainedRec $ sortDataRec $ filterEmptyS $ L.splitOn "}\n" $ unlines dataTypesRec
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

--------
-- Initial Filters (In order of use)
-------

-- get rid of lines that start with a comment.
filterComments :: [String] -> [String]
filterComments ls = ls \\ filter (isPrefixOf "--") ls

-- get rid of lines with nothing in them.
filterEmptyS :: [String] -> [String]
filterEmptyS = filter (/= "")

-- for those few cases of data declarations that start the actual data declaration on a new line after the "=" sign.
removeNewlineEqual :: [String] -> [String]
removeNewlineEqual [] = []
removeNewlineEqual (l1:l2:ls) = if ("data " `isPrefixOf` l1 || "type " `isPrefixOf` l1 || "newtype " `isPrefixOf` l1) && "=" `isSuffixOf` l1 then removeNewlineEqual ((l1 ++ " " ++ l2): ls) else l1 : removeNewlineEqual (l2:ls)
removeNewlineEqual ls = ls

-- for those few cases of data declarations that are record types where the "{" is on a newline.
removeNewlineBrace :: [String] -> [String]
removeNewlineBrace [] = []
removeNewlineBrace (l1:l2:ls) = if ("data " `isPrefixOf` l1 || "newtype " `isPrefixOf` l1)  && "{" `isInfixOf` l2 then (l1 ++ " " ++ l2) : removeNewlineBrace ls else l1:removeNewlineBrace (l2:ls)
removeNewlineBrace ls = ls

-- for those few cases of data declarations that use constructors with guards on a newline.
removeNewlineGuard :: [String] -> [String]
removeNewlineGuard [] = []
removeNewlineGuard (l1:l2:ls) = if "|" `isPrefixOf` l2 then removeNewlineGuard ((l1 ++ " " ++ l2): ls) else l1 : removeNewlineGuard (l2:ls)
removeNewlineGuard ls = ls

-- Gets rid of automatically derived instances since we only care about type dependencies.
removeDeriving :: [String] -> [String]
removeDeriving [] = []
removeDeriving (l:ls) 
  | "deriving " `isInfixOf` l = unwords (take (fromJust (elemIndex "deriving" (words l))) $ words l): removeDeriving ls
  | otherwise = l : removeDeriving ls

-- Removes comments that are a part of datatype lines (drops everything after the comment symbol).
removeComments :: [String] -> [String]
removeComments [] = []
removeComments (l:ls)
  | "--" `isInfixOf` l = unwords (take (fromJust (findIndex  (isInfixOf "--") (words l))) $ words l): removeComments ls
  | otherwise = l : removeComments ls

-----------------
-- Sorting and filtering for functions that use @data@ syntax (for record types)
----------------

-- Attach booleans to see if a line is a part of a record data type declaration. 
-- This is needed to organize and arrage data types.
isDataRec :: [String] -> Bool -> [(String, Bool)]
isDataRec [] _ = [] 
isDataRec (l:ls) dtStill
    | "data " `isPrefixOf` l && "{" `isInfixOf` l && "}" `isInfixOf` l = (l, True): isDataRec ls False -- if a line starts with "data " and contains "{" and "}", it is the start and end of a record datatype.
    | "data " `isPrefixOf` l && "{" `isInfixOf` l = (l, True): isDataRec ls True -- if a line starts with "data " and contains "{", it is the start of a record datatype.
    | dtStill && "}" `isInfixOf` l = (l, True): isDataRec ls False               -- if a line is still part of record datatype and we see "}", the datatype declaration has ended.
    | dtStill = (l, True) : isDataRec ls True                                    -- if a line is still part of record datatype and none of the above happens, keep going with the datatype.
    | otherwise = (l, False) : isDataRec ls False                                -- otherwise, it is not a datatype and keep going as if the next line is not a datatype.
  
-- Record types may be defined in the form:
-- data Type where
--    Constructor :: (ClassConstraint a) => { record1 :: Type1
--        , record2 = [a]
--        , record3 = Type3
--        } -> Type 
-- This is used in the CodeSpec and SystemInformation types.
useConstructFormRec :: Bool -> [String]  -> [String]
useConstructFormRec _ [] = []
useConstructFormRec isSameRec (l1:l2:ls) 
  | "data " `isPrefixOf` l1 && "where " `isInfixOf` l1 = useConstructFormRec True ((l1Construct ++ l2) : ls)
  | "}" `isInfixOf` l2 && "->" `isInfixOf` l2 = useConstructFormRec False ((l1 ++ "}") : ls) -- may need to be changed to account for other possible variance in type declaratios, but for now it should work
  | isSameRec = useConstructFormRec True ((l1 ++ l2):ls)
  | otherwise = l1 : useConstructFormRec False (l2:ls)
  where
      l1Construct 
        | "=>" `isInfixOf` l1 = T.unpack (T.replace (T.pack "where") (T.pack "=") $ T.pack $ unwords $ take (fromJust (findIndex  (isInfixOf "::") (words l1))) $ words l1) ++ " {" --has a class constraint, but we can just ignore that for now
        | otherwise = T.unpack (T.replace (T.pack "where") (T.pack "=") $ T.pack l1) ++ " {" -- no class constraint in data type
useConstructFormRec _ ls = ls

-- Instead of separating records by newline (which are not guarenteed),
-- use commas (which will always be there for a record with more than one field).
removeNewlineComma :: [String] -> [String]
removeNewlineComma [] = []
removeNewlineComma (l1:l2:ls)
  | "," `isPrefixOf` l2 = removeNewlineComma ((l1 ++ " " ++ l2) : ls)
  | otherwise = l1 : removeNewlineComma (l2:ls)
removeNewlineComma ls = ls

-- Take a list of data declarations for records and get the name of the datatype itself and all dependencies of that datatype.
sortDataRec :: [String] -> [(String, [String])]
sortDataRec [] = []
sortDataRec (l:ls) = (head typeContents, checkContents): sortDataRec ls 
    where
        typeContents = map stripWS $ L.splitOn "=" $ l \\ "data "
        checkContents = map stripWS $ concatMap (tail . L.splitOn "::") $ L.splitOn "," $ concat $ tail typeContents

-- Record a datatype and its dependencies. For record types using the @data@ declaration syntax.
getDataContainedRec :: [(String, [String])] -> [DataDeclRecord]
getDataContainedRec = map (\l -> DDR {ddrName = filterAll $ fst l, ddrContent = nub $ filter (not . null) $ map filterAll $ snd l})

-----------------
-- Sorting and filtering for functions that use @data@ syntax (for non-record types)
----------------

-- Some datatypes use a style similar to defining functions. This will change it over to constructors for use in other functions.
useGuardForm :: [String] -> [String]
useGuardForm [] = []
useGuardForm (l1:l2:ls) = if "::" `isInfixOf` l2 then useGuardForm ((l1 ++ l2Guard) : ls) else l1: useGuardForm (l2:ls)
    where l2Guard = " |" ++ T.unpack (T.replace (T.pack "::") mempty $ T.replace (T.pack "->") mempty $ T.pack l2)
useGuardForm ls = ls

-- Some datatypes may be defined using the @where@ syntax, so this converts them to use @=@.
useEqForm :: String -> String
useEqForm = T.unpack . T.replace (T.pack "where") (T.pack "=") . T.pack

-- For filtering out data types that are records.
isDataConst :: String -> Bool
isDataConst dt = isPrefixOf "data " dt && not ("{" `isInfixOf` dt)

-- Helper that takes a list of datatype declarations (not record type) and sorts them so that a list of the datatype name and the datatype constructor values is made.
sortDataConst :: [String] -> [(String, [String])]
sortDataConst [] = []
sortDataConst (l:ls) = (dataCName, dataCContents): sortDataConst ls
    where dataCName = head $ map stripWS $ L.splitOneOf "|=" $ l \\ "data "
          dataCContents = concatMap (tail . words) $ tail $ filter (not . null) $ map stripWS $ L.splitOneOf "|=" $ l \\ "data " 

-- Record a datatype and its dependencies. For non-record types using the @data@ declaration syntax.
getDataContainedConst :: [(String, [String])] -> [DataDeclConstruct]
getDataContainedConst = map (\l -> DDC {ddcName = filterAll $ fst l, ddcContent = nub $ filter (not . null) $ map filterAll $ snd l})

-----------------
-- Sorting and filtering for functions that use @newtype@ syntax
----------------

-- Newtypes can be defined similar to records. Only includes those record-style declarations.
isNewtypeRec :: [String] -> [String]
isNewtypeRec = filter (isInfixOf "{")

-- Newtypes are often defined using constructors (they don't use @{@).
isNewtypeConst :: [String] -> [String]
isNewtypeConst = filter (not . isInfixOf "{")

-- Record a datatype and its dependencies. For record-style @newtype@ declaration syntax.
getNewtypesR :: [String] -> [NewtypeDecl]
getNewtypesR [] = []
getNewtypesR (l:ls) = NTD {ntdName = filterAll $ head typeContents, ntdContent = nub $ filter (not . null) $ map filterAll checkContents} : getNewtypesR ls
  where typeContents = map stripWS $ L.splitOn "=" $ l \\ "newtype "
        checkContents = map stripWS $ concatMap (tail . L.splitOn "::") $ L.splitOn "," $ concat $ tail typeContents

-- Record a datatype and its dependencies. For constructor-style @newtype@ declaration syntax.
getNewtypesC :: [String] -> [NewtypeDecl]
getNewtypesC [] = []
getNewtypesC (l:ls) = NTD {ntdName = filterAll $ head typeContents, ntdContent = nub $ filter (not . null) $ map filterAll checkContents} : getNewtypesC ls
    where typeContents = map stripWS $ L.splitOn "=" $ l \\ "newtype "
          checkContents = concatMap (tail.words) $ tail typeContents

-----------------
-- Sorting and filtering for functions that use @type@ syntax
----------------

-- Record a datatype and its dependencies. For @type@ declaration syntax.
getTypes :: [String] -> [TypeDecl]
getTypes [] = []
getTypes (l:ls) = TD {tdName = filterAll $ head typeContents, tdContent = nub $ filter (not . null) $ map filterAll $ tail typeContents} : getTypes ls
    where typeContents = map stripWS $ L.splitOn "=" $ l \\ "type "

-----------------
-- Ending filter functions
---------------

-- Combines the below three filter functions
filterAll :: String -> String
filterAll = filterPrimeTypes . filterQualifiedTypes . filterInvalidChars

-- These characters should either not appear in a .dot file (eg. gets rid of list types), or does some extra cleanup that the general sort functions did not catch.
filterInvalidChars :: String -> String
filterInvalidChars = filterInvalidChars' invalidChars
  where
    filterInvalidChars' [] x = x
    filterInvalidChars' (l:ls) x = filterInvalidChars' ls $ filter (/= l) x
    invalidChars = "[]!} (){->,$" --the space being here is kind of a hack (as a result of uncommon type syntax not caught by the above sorters), but it allows the .dot files to compile for now

-- Primes are not legal syntax in .dot files, so replace @'@ with @_@ instead.
filterPrimeTypes :: String -> String
filterPrimeTypes = T.unpack . T.replace (T.pack "\'") (T.pack "_") . T.pack 

-- We don't need to know which types are qualified, so just get rid of the extra information.
filterQualifiedTypes :: String -> String
filterQualifiedTypes l = if '.' `elem` l then drop (fromJust (elemIndex '.' l)+1) l else l -- get rid of types that are made from qualified imports

---------
-- Other functions
---------

-- enforces strict file reading; files can be closed to avoid memory exhaustion
forceRead :: [a0] -> ()
forceRead [] = ()
forceRead (x:xs) = forceRead xs

