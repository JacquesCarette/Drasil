-- | Source code reader for type dependency graphs of all Drasil types.
module SourceCodeReaderT (extractEntryData, EntryData(..), DataDeclRecord(..),
 DataDeclConstruct(..), NewtypeDecl(..), TypeDecl(..), DataTypeDeclaration(..)) where

import Data.List
import System.IO
import System.Directory
import qualified Data.Text as T
import qualified Data.List.Split as L
import Data.Maybe (fromJust)
import Data.Char (isUpper)

import DirectoryController as DC (FileName)

import Data.Containers.ListUtils (nubOrd)

-- Synonyms for clarity
type DataName = String
type NewtypeName = String
type TypeName = String

------------
-- Main function and datatypes for creating a .dot graph of the type dependencies in Drasil
-----------

-- purposefully use different type names even though they will end up in the same form (for now)
data DataDeclRecord = DDR { ddrName :: DataName                       -- this can actually be any kind of type, but use DataName for clarity (same with below)
                          , ddrContent :: [DataName]} deriving (Show) -- same with this
data DataDeclConstruct = DDC { ddcName :: DataName
                          , ddcContent :: [DataName]} deriving (Show)
data NewtypeDecl = NTD { ntdName :: NewtypeName
                          , ntdContent :: [NewtypeName]} deriving (Show)
data TypeDecl = TD { tdName :: TypeName
                    , tdContent :: [TypeName]} deriving (Show)

-- The above types all have a name and other types they build upon (contents).
class DataTypeDeclaration a where
  getTypeName :: a -> String
  getContents :: a -> [String]

instance DataTypeDeclaration DataDeclRecord where
  getTypeName = ddrName
  getContents = ddrContent
instance DataTypeDeclaration DataDeclConstruct where
  getTypeName = ddcName
  getContents = ddcContent
instance DataTypeDeclaration NewtypeDecl where
  getTypeName = ntdName
  getContents = ntdContent
instance DataTypeDeclaration TypeDecl where
  getTypeName = tdName
  getContents = tdContent


-- new EntryData data type with strict fields to enforce strict file reading
data EntryData = EntryData { dRNs :: ![DataDeclRecord]    -- Record datatypes will be shown differently on a dot graph
                           , dCNs :: ![DataDeclConstruct] -- compared to these datatypes that use constructors.
                           , ntNs :: ![NewtypeDecl]       -- Newtypes will be recorded as records (usually only wrap one other type)
                           , tNs :: ![TypeDecl]} deriving (Show) -- Types are usually synonyms of other types, so act like Record datatypes but in a different colour.

-- extracts data, newtype and class names + instances (new data-oriented format)
extractEntryData :: DC.FileName -> FilePath -> IO EntryData
extractEntryData fileName filePath = do
  setCurrentDirectory filePath
  handle <- openFile fileName ReadMode
  scriptFile <- hGetContents handle
  forceRead scriptFile `seq` hClose handle

      -- general light cleanup of the files before sorting by datatype
  let scriptFileLines = scriptFilter scriptFile
      -- organize the data from the script file into their respective data Decl formats
      dataDeclRec =  formatDataRec scriptFileLines
      dataDeclConst = formatDataCon scriptFileLines
      newtypeDecl = formatNewtype scriptFileLines
      typeDecl = formatType scriptFileLines

  -- returns all types within a file
  return EntryData {dRNs=dataDeclRec,dCNs=dataDeclConst,ntNs=newtypeDecl,tNs=typeDecl}

---TODO: use map/filter/fold for most of these, I just need to visualize what is happening for now.

--------
-- Initial Filters (In order of use)
-------

-- combine all initial filters (except for removing in-line comments, which is done later)
scriptFilter :: String -> [String]
scriptFilter = removeDeriving . removeNewlineGuard . removeNewlineBrace . removeNewlineEqual . filterEmptyS . filterMultilineComments . filterComments . map stripWS . lines

-- get rid of lines that start with a comment.
filterComments :: [String] -> [String]
filterComments ls = ls \\ filter (isPrefixOf "--") ls

-- gets rid of lines that start or end with a multiline comment
filterMultilineComments :: [String] -> [String]
filterMultilineComments ls = ls \\ filter (\l -> isPrefixOf "{-" l || isSuffixOf "-}" l) ls

-- get rid of lines with nothing in them.
filterEmptyS :: [String] -> [String]
filterEmptyS = filter (/= "")

-- for those few cases of data declarations that start the actual data declaration on a new line after the "=" sign.
removeNewlineEqual :: [String] -> [String]
removeNewlineEqual = filterNewline prefixCheck (const True)
  where
    prefixCheck l = ("data " `isPrefixOf` l || "type " `isPrefixOf` l || "newtype " `isPrefixOf` l) && "=" `isSuffixOf` l

-- for those few cases of data declarations that are record types where the "{" is on a newline.
removeNewlineBrace :: [String] -> [String]
removeNewlineBrace = filterNewline prefixCheck (isInfixOf "{")
  where
    prefixCheck l = "data " `isPrefixOf` l || "newtype " `isPrefixOf` l

-- for those few cases of data declarations that use constructors with guards on a newline.
removeNewlineGuard :: [String] -> [String]
removeNewlineGuard = filterNewline (const True) (isPrefixOf "|") 

-- Gets rid of automatically derived instances since we only care about type dependencies.
removeDeriving :: [String] -> [String]
removeDeriving = mapIf (isInfixOf "deriving ") $ \l -> unwords (take (fromJust (elemIndex "deriving" (words l))) $ words l)

-- Removes comments that are a part of datatype lines (drops everything after the comment symbol).
removeComments :: [String] -> [String]
removeComments = mapIf (isInfixOf "--") $ \l -> unwords $ take (fromJust $ findIndex  (isInfixOf "--") $ words l) $ words l

-- helper map functions to reduce code clutter --

-- map a function to a list if a predicate passes, otherwise, just keep element the same.
mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf p f = map (\x -> if p x then f x else x)

-- helper to remove newlines if the contents pass a predicate, otherwise just skip over the element.
-- Maps over two elements at a time. First predicate is for first line, second is for the second line.
filterNewline :: (String -> Bool) -> (String -> Bool) -> [String] -> [String]
filterNewline _ _ [] = []
filterNewline p1 p2 (l1:l2:ls) = if p1 l1 && p2 l2 then filterNewline p1 p2 ((l1 ++ " " ++ l2):ls) else l1 : filterNewline p1 p2 (l2:ls)
filterNewline _ _ ls = ls

-------------------------------
-- Main formatting functions (combine many filtering and sorting functions for cleaner code)
-- Takes in the raw file data, formats it into nice usable strings, and then sorts them into their respective type declarations
------------------------------

-- combine record data sorting & cleanup functions for cleaner code in extractEntryData
formatDataRec :: [String] -> [DataDeclRecord]
                -- sorting functions                                                           -- cleanup functions
formatDataRec = getDataContainedRec . sortDataRec . filterEmptyS . L.splitOn "}\n" . unlines . removeNewlineComma . useConstructFormRec False . removeComments . isDataRec

-- combine constructor data sorting & cleanup functions for cleaner code in extractEntryData
formatDataCon :: [String] -> [DataDeclConstruct]
                -- sorting functions                    -- cleanup functions
formatDataCon = getDataContainedConst . sortDataConst . removeComments . isDataConst . map useEqForm . useGuardForm 

-- combine newtype sorting & cleanup functions for cleaner code in extractEntryData
formatNewtype :: [String] -> [NewtypeDecl]
                -- sorting functions
formatNewtype sfLines = getNewtypes $ sortNewtypesR newtypeRec ++ sortNewtypesC newtypeConst
  where
    -- cleanup functions
    newtypeRec = removeComments $ isNewtypeRec sfLines
    newtypeConst = removeComments $ isNewtypeConst sfLines

-- combine type sorting & cleanup functions for cleaner code in extractEntryData
formatType :: [String] -> [TypeDecl]
             -- sorting functions   -- cleanup functions
formatType = getTypes . sortTypes . removeComments . isType


-----------------
-- Sorting and filtering for functions that use @data@ syntax (for record types)
----------------

-- Attach booleans to see if a line is a part of a record data type declaration. 
-- This is needed to organize and arrage data types.
isDataRec :: [String] -> [String]
isDataRec dataRecs = isDataRecAux dataRecs False
  where
    isDataRecAux [] _ = []
    isDataRecAux (l:ls) dtStill
      | "data " `isPrefixOf` l && "{" `isInfixOf` l && "}" `isInfixOf` l = l: isDataRecAux ls False -- if a line starts with "data " and contains "{" and "}", it is the start and end of a record datatype.
      | "data " `isPrefixOf` l && "{" `isInfixOf` l = l: isDataRecAux ls True -- if a line starts with "data " and contains "{", it is the start of a record datatype.
      | dtStill && "}" `isInfixOf` l = l: isDataRecAux ls False               -- if a line is still part of record datatype and we see "}", the datatype declaration has ended.
      | dtStill = l: isDataRecAux ls True                                    -- if a line is still part of record datatype and none of the above happens, keep going with the datatype.
      | otherwise = isDataRecAux ls False                                    -- otherwise, it is not a datatype and keep going as if the next line is not a datatype.
    -- Trying fold style, gave me different .dot graphs so probably is not right
    {-output = map fst $ filter snd $ zip dataRecs doit 
    doit = foldl (\x y -> ((++) x (someFunc (myHead (reverse x)) y))) [] dataRecs
    someFunc :: Bool -> String -> [Bool]
    someFunc dtStill l
      | "data " `isPrefixOf` l && "{" `isInfixOf` l && "}" `isInfixOf` l = [False]
      | "data " `isPrefixOf` l && "{" `isInfixOf` l = [True]
      | dtStill && "}" `isInfixOf` l = [False]              
      | dtStill = [True]                                    
      | otherwise = [False]                                 
    myHead [] = False
    myHead (x:xs) = x-}
  
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
removeNewlineComma = filterNewline (const True) (isPrefixOf ",")

-- filter through records of the form rec :: Type1 -> f Type2. Only accepts the first type though, so this will eventually need to be changed
filterFuncForm :: String -> String
filterFuncForm l 
  | "->" `isInfixOf` l = unwords $ filter (isUpper . head) $ nub $ words l
  | otherwise = l

-- Take a list of data declarations for records and get the name of the datatype itself and all dependencies of that datatype.
sortDataRec :: [String] -> [(String, [String])]
sortDataRec = map (\l -> (head $ typeContents l, typeDependencies l))
  where
    typeContents l = map stripWS $ L.splitOn "=" $ l \\ "data "
    typeDependencies l = map (filterFuncForm . stripWS) $ concatMap (tail . L.splitOn "::") $ L.splitOn "," $ concat $ tail $ typeContents l

-- Record a datatype and its dependencies. For record types using the @data@ declaration syntax.
getDataContainedRec :: [(String, [String])] -> [DataDeclRecord]
getDataContainedRec = map (\l -> DDR {ddrName = filterName $ fst l, ddrContent = filterContents $ snd l})

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
isDataConst :: [String] -> [String]
isDataConst = filter (\dt -> isPrefixOf "data " dt && not ("{" `isInfixOf` dt))

-- Helper that takes a list of datatype declarations (not record type) and sorts them so that a list of the datatype name and the datatype constructor values is made.
sortDataConst :: [String] -> [(String, [String])]
sortDataConst = map (\l -> (head $ typeContents l, typeDependencies l))
  where 
    typeContents l = map stripWS $ L.splitOneOf "|=" $ l \\ "data "
    typeDependencies l = concatMap (tail . words) $ tail $ filter (not . null) $ typeContents l

-- Record a datatype and its dependencies. For non-record types using the @data@ declaration syntax.
getDataContainedConst :: [(String, [String])] -> [DataDeclConstruct]
getDataContainedConst = map (\l -> DDC {ddcName = filterName $ fst l, ddcContent = filterContents $ snd l})

-----------------
-- Sorting and filtering for functions that use @newtype@ syntax
----------------

-- Newtypes can be defined similar to records. Only includes those record-style declarations.
isNewtypeRec :: [String] -> [String]
isNewtypeRec = filter (\x -> "{" `isInfixOf` x && "newtype " `isPrefixOf` x)

-- Newtypes are often defined using constructors (they don't use @{@).
isNewtypeConst :: [String] -> [String]
isNewtypeConst = filter (\x -> not ("{" `isInfixOf` x) && "newtype " `isPrefixOf` x)

-- Sorts a datatype and its dependencies. For record-style @newtype@ declaration syntax.
sortNewtypesR :: [String] -> [(String, [String])]
sortNewtypesR = map (\l -> (head $ typeContents l, typeDependencies l))
  where typeContents l = map stripWS $ L.splitOn "=" $ l \\ "newtype "
        typeDependencies l = map stripWS $ concatMap (tail . L.splitOn "::") $ L.splitOn "," $ concat $ tail $ typeContents l

-- Sorts a datatype and its dependencies. For constructor style @newtype@ declaration syntax.
sortNewtypesC :: [String] -> [(String, [String])]
sortNewtypesC = map (\l -> (head $ typeContents l, typeDependencies l))
  where typeContents l = map stripWS $ L.splitOn "=" $ l \\ "newtype "
        typeDependencies l = concatMap (tail.words) $ tail $ typeContents l

-- Record a datatype and its dependencies. For @newtype@ declaration syntax.
getNewtypes :: [(String, [String])] -> [NewtypeDecl]
getNewtypes = map (\l -> NTD {ntdName = filterName $ fst l, ntdContent = filterContents $ snd l})

-----------------
-- Sorting and filtering for functions that use @type@ syntax
----------------

-- Only accept types defined with "=" syntax (for now).
isType :: [String] -> [String]
isType = filter (\x -> isInfixOf "=" x && isPrefixOf "type " x)

-- Sorts a datatype and its dependencies. For @type@ declaration syntax.
sortTypes :: [String] -> [(String, [String])]
sortTypes = map (\l -> (head $ typeContents l, tail $ typeContents l))
  where typeContents l = map stripWS $ L.splitOn "=" $ l \\ "type "

-- Record a datatype and its dependencies. For @type@ declaration syntax.
getTypes :: [(String, [String])] -> [TypeDecl]
getTypes = map (\l -> TD {tdName = filterName $ fst l, tdContent = filterContents $ snd l})

-----------------
-- Ending filter functions
---------------

-- Combines multiple filter functions. For type contents.
filterContents :: [String] -> [String]
filterContents = nubOrd . filter (not . null) . map filterName

-- Combines the below three filter functions. For type names.
filterName :: String -> String
filterName = filterPrimeTypes . filterQualifiedTypes . filterInvalidChars

-- These characters should either not appear in a .dot file (eg. gets rid of list types), or does some extra cleanup that the general sort functions did not catch.
filterInvalidChars :: String -> String
filterInvalidChars = filter (`notElem` invalidChars)
  where
    invalidChars = "[]!} (){->,$" --the space being here is kind of a hack (as a result of uncommon type syntax not caught by the above sorters), but it allows the .dot files to compile for now

-- Primes are not legal syntax in .dot files, so replace @'@ with @_@ instead.
filterPrimeTypes :: String -> String
filterPrimeTypes = T.unpack . T.replace (T.pack "\'") (T.pack "_") . T.pack 

-- We don't need to know which types are qualified, so just get rid of the extra information.
filterQualifiedTypes :: String -> String
filterQualifiedTypes l = if '.' `elem` l then drop (fromJust (elemIndex '.' l)+1) l else l -- get rid of types that are made from qualified imports

---------
-- Other helper functions
---------

-- strips leading and trailing whitespace from strings
stripWS :: String -> String
stripWS = T.unpack . T.strip . T.pack

-- enforces strict file reading; files can be closed to avoid memory exhaustion
forceRead :: [a0] -> ()
forceRead [] = ()
forceRead (x:xs) = forceRead xs

