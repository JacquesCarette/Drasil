{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Drasil.Shared.State (
  -- Types
  GS, GOOLState(..), FS, CS, MS, VS, 
  -- Lenses
  lensFStoGS, lensGStoFS, lensMStoGS, lensFStoCS, lensFStoMS, lensFStoVS, 
  lensCStoMS, lensMStoCS, lensCStoVS, lensMStoFS, lensMStoVS, lensVStoFS, 
  lensVStoMS, lensCStoFS, headers, sources, mainMod, currMain, currFileType, currParameters,
  -- Initial states
  initialState, initialFS, 
  -- State helpers
  modifyReturn, modifyReturnFunc, modifyReturnList, 
  -- State modifiers
  revFiles, addFile, addCombinedHeaderSource, addHeader, addSource, 
  addProgNameToPaths, setMainMod, addLangImport, addLangImportVS, 
  addExceptionImports, getLangImports, addLibImport, addLibImportVS, 
  addLibImports, getLibImports, addModuleImport, addModuleImportVS, 
  getModuleImports, addHeaderLangImport, getHeaderLangImports, 
  addHeaderLibImport, getHeaderLibImports, addHeaderModImport, 
  getHeaderModImports, addDefine, getDefines, addHeaderDefine, 
  getHeaderDefines, addUsing, getUsing, addHeaderUsing, getHeaderUsing, 
  setFileType, setModuleName, getModuleName, setClassName, getClassName, 
  setCurrMain, getCurrMain, addClass, getClasses, updateClassMap, getClassMap, 
  updateMethodExcMap, getMethodExcMap, updateCallMap, callMapTransClosure, 
  updateMEMWithCalls, addParameter, getParameters, setOutputsDeclared, 
  isOutputsDeclared, addException, addExceptions, getExceptions, addCall, 
  setMainDoc, getMainDoc, setVisibility, getVisibility, setCurrMainFunc,
  getCurrMainFunc, setThrowUsed, getThrowUsed, setErrorDefined, getErrorDefined,
  incrementLine, incrementWord, getLineIndex,
  getWordIndex,  resetIndices, useVarName, genVarName, genLoopIndex,
  genVarNameIf, varNameAvailable, setVarScope, getVarScope
) where

import Drasil.Shared.AST (FileType(..), VisibilityTag(..), ScopeTag(..),
  ScopeData(..), sd, QualifiedName, qualName)
import Drasil.Shared.CodeAnalysis (Exception, ExceptionType, printExc, hasLoc)
import Drasil.Shared.CodeType (ClassName)

import Utils.Drasil (nubSort)

import Control.Lens (Lens', (^.), lens, makeLenses, over, set, _1, _2, both, at)
import Control.Monad.State (State, modify, gets)
import Data.Char (isDigit)
import Data.List (nub)
import Data.Foldable (foldl')
import Data.Maybe (isNothing, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple (swap)
import Text.PrettyPrint.HughesPJ (Doc, empty)
import Text.Read (readMaybe)

data GOOLState = GS {
  _headers :: [FilePath], -- Used by Drasil for doxygen config gen
  _sources :: [FilePath], -- Used by Drasil for doxygen config and Makefile gen
  _mainMod :: Maybe FilePath, -- Used by Drasil generator to access main 
                              -- mod file path (needed in Makefile generation)
  _classMap :: Map String ClassName, -- Used to determine whether an import is 
                                     -- needed when using extClassVar and obj

  -- Only used in Java and Swift, to generate correct "throws Exception" declarations
  _methodExceptionMap :: Map QualifiedName [ExceptionType], -- Method to exceptions thrown
  _callMap :: Map QualifiedName [QualifiedName], -- Method to other methods it calls

  -- Only used for Swift
  _throwUsed :: Bool, -- to add code so Strings can be used as Errors
  _errorDefined :: Bool -- to avoid duplicating that code
} 
makeLenses ''GOOLState

data FileState = FS {
  _goolState :: GOOLState,
  _currModName :: String, -- Used by fileDoc to insert the module name in the 
                          -- file path, and by CodeInfo/Java when building
                          -- method exception map and call map
  _currFileType :: FileType, -- Used when populating headers and sources in GOOLState
  _currMain :: Bool, -- Used to set mainMod in GOOLState, 
                     -- and in C++ to put documentation for the main 
                     -- module in the source file instead of header
  _currClasses :: [ClassName], -- Used to update classMap
  _langImports :: [String],
  _libImports :: [String],
  _moduleImports :: [String],
  
  -- Only used for Python and Swift
  _mainDoc :: Doc, -- To print Python/Swift's "main" last

  -- C++ only
  _headerLangImports :: [String],
  _headerLibImports :: [String],
  _headerModImports :: [String],
  _defines :: [String],
  _headerDefines :: [String],
  _using :: [String],
  _headerUsing :: [String]
}
makeLenses ''FileState

data ClassState = CS {
  _fileState :: FileState,
  _currClassName :: ClassName -- So class name is accessible when generating 
                              -- constructor or self 
}
makeLenses ''ClassState

type Index = Integer

data MethodState = MS {
  _classState :: ClassState,
  _currParameters :: [String], -- Used to get parameter names when generating 
                               -- function documentation
  _varNames :: Map String Int, -- Used to generate fresh variable names
  
  -- Only used for Julia
  _varScopes :: Map String ScopeData, -- Used to keep track of a variable's scope from its declaration

  -- Only used for Java
  _outputsDeclared :: Bool, -- So Java doesn't redeclare outputs variable when using inOutCall
  _exceptions :: [ExceptionType], -- Used to build methodExceptionMap
  _calls :: [QualifiedName], -- Used to build CallMap
  
  -- Only used for C++
  _currVisibility :: VisibilityTag, -- Used to maintain correct visibility when adding 
                          -- documentation to function in C++
  _currMainFunc :: Bool, -- Used by C++ to put documentation for the main
                        -- function in source instead of header file

  -- Only used for Swift
  _contentsIndices :: (Index, Index) -- Used to keep track of the current place
                                     -- in a file being read. First Int is the 
                                     -- line number, second is the word number.
}
makeLenses ''MethodState

-- This was once used, but now is not. However it would be a pain to revert all 
-- of the types back to MS from VS, and it is likely that this level of state 
-- will be useful in the future, so I'm just putting in a placeholder.
newtype ValueState = VS {
  _methodState :: MethodState
}
makeLenses ''ValueState

type GS = State GOOLState
type FS = State FileState
type CS = State ClassState
type MS = State MethodState
type VS = State ValueState

-------------------------------
---- Lenses between States ----
-------------------------------

-- GS - FS --

lensGStoFS :: Lens' GOOLState FileState
lensGStoFS = lens (\gs -> set goolState gs initialFS) (const (^. goolState))

lensFStoGS :: Lens' FileState GOOLState
lensFStoGS = goolState

-- GS - MS --

lensMStoGS :: Lens' MethodState GOOLState
lensMStoGS = lensMStoFS . lensFStoGS

-- FS - CS --

lensFStoCS :: Lens' FileState ClassState
lensFStoCS = lens (\fs -> set fileState fs initialCS) (const (^. fileState))

lensCStoFS :: Lens' ClassState FileState
lensCStoFS = fileState

-- FS - MS --

lensFStoMS :: Lens' FileState MethodState
lensFStoMS = lens (\fs -> set lensMStoFS fs initialMS) (const (^. lensMStoFS))

lensMStoFS :: Lens' MethodState FileState 
lensMStoFS = classState . fileState

-- CS - MS --

lensCStoMS :: Lens' ClassState MethodState
lensCStoMS = lens (\cs -> set classState cs initialMS) (const (^. classState))

lensMStoCS :: Lens' MethodState ClassState
lensMStoCS = classState

-- FS - VS --

lensFStoVS :: Lens' FileState ValueState
lensFStoVS = lens (\fs -> set lensVStoFS fs initialVS) (const (^. lensVStoFS))

lensVStoFS :: Lens' ValueState FileState
lensVStoFS = methodState . lensMStoFS

-- CS - VS --

lensCStoVS :: Lens' ClassState ValueState
lensCStoVS = lens (\cs -> set (methodState . classState) cs initialVS) 
  (const (^. (methodState . classState)))

-- MS - VS --

lensMStoVS :: Lens' MethodState ValueState
lensMStoVS = lens (\ms -> set methodState ms initialVS) (const (^. methodState))

lensVStoMS :: Lens' ValueState MethodState
lensVStoMS = methodState

-------------------------------
------- Initial States -------
-------------------------------

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = [],
  _mainMod = Nothing,
  _classMap = Map.empty,

  _methodExceptionMap = Map.empty,
  _callMap = Map.empty,

  _throwUsed = False,
  _errorDefined = False
}

initialFS :: FileState
initialFS = FS {
  _goolState = initialState,
  _currModName = "",
  _currFileType = Combined,
  _currMain = False,
  _currClasses = [],
  _langImports = [],
  _libImports = [],
  _moduleImports = [],
  
  _mainDoc = empty,

  _headerLangImports = [],
  _headerLibImports = [],
  _headerModImports = [],
  _defines = [],
  _headerDefines = [],
  _using = [],
  _headerUsing = []
}

initialCS :: ClassState
initialCS = CS {
  _fileState = initialFS,
  _currClassName = ""
}

initialMS :: MethodState
initialMS = MS {
  _classState = initialCS,
  _currParameters = [],
  _varNames = Map.empty,

  _varScopes = Map.fromList [("", sd Local)], -- Hack for multiAssign

  _outputsDeclared = False,
  _exceptions = [],
  _calls = [],

  _currVisibility  = Priv,
  _currMainFunc = False,

  _contentsIndices = (0,0)
}

initialVS :: ValueState
initialVS = VS {
  _methodState = initialMS
}

-------------------------------
------- State Patterns -------
-------------------------------

modifyReturn :: (s -> s) -> a -> State s a
modifyReturn sf v = do
  modify sf
  return v

modifyReturnFunc :: (b -> s -> s) -> (b -> a) -> State s b -> State s a
modifyReturnFunc sf vf st = do
  v <- st
  modify $ sf v
  return $ vf v

modifyReturnList :: [State s b] -> (s -> s) -> 
  ([b] -> a) -> State s a
modifyReturnList l sf vf = do
  v <- sequence l
  modify sf
  return $ vf v

-------------------------------
------- State Modifiers -------
-------------------------------

revFiles :: GOOLState -> GOOLState
revFiles = over headers reverse . over sources reverse

addFile :: FileType -> FilePath -> GOOLState -> GOOLState
addFile Combined = addCombinedHeaderSource
addFile Source = addSource
addFile Header = addHeader

addHeader :: FilePath -> GOOLState -> GOOLState
addHeader fp = over headers (\h -> ifElemError fp h $
  "Multiple files with same name encountered: " ++ fp)

addSource :: FilePath -> GOOLState -> GOOLState
addSource fp = over sources (\s -> ifElemError fp s $
  "Multiple files with same name encountered: " ++ fp)

addCombinedHeaderSource :: FilePath -> GOOLState -> GOOLState
addCombinedHeaderSource fp = addSource fp . addHeader fp 

addProgNameToPaths :: String -> GOOLState -> GOOLState
addProgNameToPaths n = over mainMod (fmap f) . over sources (map f) . 
  over headers (map f)
  where f = ((n++"/")++)

setMainMod :: String -> GOOLState -> GOOLState
setMainMod n = over mainMod (\m -> if isNothing m then Just n else error 
  "Multiple modules with main methods encountered")

addLangImport :: String -> MethodState -> MethodState
addLangImport i = over (lensMStoFS . langImports) (\is -> nubSort $ i:is)
  
addLangImportVS :: String -> ValueState -> ValueState
addLangImportVS i = over methodState (addLangImport i)

addExceptionImports :: [Exception] -> MethodState -> MethodState
addExceptionImports es = over (lensMStoFS . langImports) 
  (\is -> nubSort $ is ++ imps)
  where imps = map printExc $ filter hasLoc es

getLangImports :: FS [String]
getLangImports = gets (^. langImports)

addLibImport :: String -> MethodState -> MethodState
addLibImport i = over (lensMStoFS . libImports) (\is -> nubSort $ i:is)

addLibImportVS :: String -> ValueState -> ValueState
addLibImportVS i = over (lensVStoFS . libImports) (\is -> nubSort $ i:is)

addLibImports :: [String] -> MethodState -> MethodState
addLibImports is s = foldl' (flip addLibImport) s is

getLibImports :: FS [String]
getLibImports = gets (^. libImports)

addModuleImport :: String -> MethodState -> MethodState
addModuleImport i = over (lensMStoFS . moduleImports) (\is -> nubSort $ i:is)

addModuleImportVS :: String -> ValueState -> ValueState
addModuleImportVS i = over methodState (addModuleImport i)

getModuleImports :: FS [String]
getModuleImports = gets (^. moduleImports)

addHeaderLangImport :: String -> ValueState -> ValueState
addHeaderLangImport i = over (lensVStoFS . headerLangImports) 
  (\is -> nubSort $ i:is)

getHeaderLangImports :: FS [String]
getHeaderLangImports = gets (^. headerLangImports)

addHeaderLibImport :: String -> MethodState -> MethodState
addHeaderLibImport i = over (lensMStoFS . headerLibImports)
  (\is -> nubSort $ i:is)

getHeaderLibImports :: FS [String]
getHeaderLibImports = gets (^. headerLibImports)

addHeaderModImport :: String -> ValueState -> ValueState
addHeaderModImport i = over (lensVStoFS . headerModImports)
  (\is -> nubSort $ i:is)

getHeaderModImports :: FS [String]
getHeaderModImports = gets (^. headerModImports)

addDefine :: String -> ValueState -> ValueState
addDefine d = over (lensVStoFS . defines) (\ds -> nubSort $ d:ds)

getDefines :: FS [String]
getDefines = gets (^. defines)
  
addHeaderDefine :: String -> ValueState -> ValueState
addHeaderDefine d = over (lensVStoFS . headerDefines) (\ds -> nubSort $ d:ds)

getHeaderDefines :: FS [String]
getHeaderDefines = gets (^. headerDefines)

addUsing :: String -> ValueState -> ValueState
addUsing u = over (lensVStoFS . using) (\us -> nubSort $ u:us)

getUsing :: FS [String]
getUsing = gets (^. using)

addHeaderUsing :: String -> ValueState -> ValueState
addHeaderUsing u = over (lensVStoFS . headerUsing) (\us -> nubSort $ u:us)

getHeaderUsing :: FS [String]
getHeaderUsing = gets (^. headerUsing)

setMainDoc :: Doc -> MethodState -> MethodState
setMainDoc d = over lensMStoFS $ set mainDoc d

getMainDoc :: FS Doc
getMainDoc = gets (^. mainDoc)

setFileType :: FileType -> FileState -> FileState
setFileType = set currFileType

setModuleName :: String -> FileState -> FileState
setModuleName = set currModName

getModuleName :: FS String
getModuleName = gets (^. currModName)

setClassName :: String -> ClassState -> ClassState
setClassName = set currClassName

getClassName :: MS ClassName
getClassName = gets (^. (classState . currClassName))

setCurrMain :: MethodState -> MethodState
setCurrMain = over (lensMStoFS . currMain) (\b -> if b then 
  error "Multiple main functions defined" else not b)

getCurrMain :: FS Bool
getCurrMain = gets (^. currMain)

addClass :: String -> ClassState -> ClassState
addClass c = over (fileState . currClasses) (\cs -> ifElemError c cs 
  "Multiple classes with same name in same file")

getClasses :: FS [String]
getClasses = gets (^. currClasses)

updateClassMap :: String -> FileState -> FileState
updateClassMap n fs = over (goolState . classMap) (Map.union (Map.fromList $
  map (n,) (fs ^. currClasses))) fs

getClassMap :: VS (Map String String)
getClassMap = gets (^. (lensVStoFS . goolState . classMap))

updateMethodExcMap :: String -> MethodState -> MethodState
updateMethodExcMap n ms = over (lensMStoFS . goolState . methodExceptionMap) 
  (Map.insert (qualName mn n) (ms ^. exceptions)) ms
  where mn = ms ^. (lensMStoFS . currModName)

getMethodExcMap :: VS (Map QualifiedName [ExceptionType])
getMethodExcMap = gets (^. (lensVStoFS . goolState . methodExceptionMap))

updateCallMap :: String -> MethodState -> MethodState
updateCallMap n ms = over (lensMStoFS . goolState . callMap) 
  (Map.insert (qualName mn n) (ms ^. calls)) ms
  where mn = ms ^. (lensMStoFS . currModName)

callMapTransClosure :: GOOLState -> GOOLState
callMapTransClosure = over callMap tClosure
  where tClosure m = Map.map (traceCalls m) m
        traceCalls :: Map QualifiedName [QualifiedName] -> [QualifiedName] -> 
          [QualifiedName]
        traceCalls _ [] = []
        traceCalls cm (c:cs) = c : traceCalls cm (cs ++ 
          Map.findWithDefault [] c cm)

updateMEMWithCalls :: GOOLState -> GOOLState
updateMEMWithCalls s = over methodExceptionMap (\mem -> Map.mapWithKey
  (addCallExcs mem (s ^. callMap)) mem) s
  where addCallExcs :: Map QualifiedName [ExceptionType] -> 
          Map QualifiedName [QualifiedName] -> QualifiedName -> [ExceptionType] 
          -> [ExceptionType]
        addCallExcs mem cm f es = nub $ es ++ concatMap (\fn -> Map.findWithDefault
          [] fn mem) (Map.findWithDefault [] f cm)

addParameter :: String -> MethodState -> MethodState
addParameter p = over currParameters (\ps -> ifElemError p ps $ 
  "Function has duplicate parameter: " ++ p)

getParameters :: MS [String]
getParameters = gets (reverse . (^. currParameters))

setOutputsDeclared :: MethodState -> MethodState
setOutputsDeclared = set outputsDeclared True

isOutputsDeclared :: MS Bool
isOutputsDeclared = gets (^. outputsDeclared)

addException :: ExceptionType -> MethodState -> MethodState
addException e = over exceptions (\es -> nub $ e : es)

addExceptions :: [ExceptionType] -> ValueState -> ValueState
addExceptions es = over (methodState . exceptions) (\exs -> nub $ es ++ exs)

getExceptions :: MS [ExceptionType]
getExceptions = gets (^. exceptions)

addCall :: QualifiedName -> ValueState -> ValueState
addCall f = over (methodState . calls) (f:)

setVisibility :: VisibilityTag -> MethodState -> MethodState
setVisibility = set currVisibility

getVisibility :: MS VisibilityTag
getVisibility = gets (^. currVisibility)

setCurrMainFunc :: Bool -> MethodState -> MethodState
setCurrMainFunc = set currMainFunc

getCurrMainFunc :: MS Bool
getCurrMainFunc = gets (^. currMainFunc)

setThrowUsed :: MethodState -> MethodState
setThrowUsed = set (lensMStoGS . throwUsed) True

getThrowUsed :: MS Bool
getThrowUsed = gets (^. (lensMStoGS . throwUsed))

setErrorDefined :: MethodState -> MethodState
setErrorDefined = set (lensMStoGS . errorDefined) True

getErrorDefined :: MS Bool
getErrorDefined = gets (^. (lensMStoGS . errorDefined))

incrementLine :: MethodState -> MethodState
incrementLine = over (contentsIndices . _1) (+1)  . set (contentsIndices . _2) 0

incrementWord :: MethodState -> MethodState
incrementWord = over (contentsIndices . _2) (+1)

getLineIndex :: MS Index
getLineIndex = gets (^. (contentsIndices . _1))

getWordIndex :: MS Index
getWordIndex = gets (^. (contentsIndices . _2))

resetIndices :: MethodState -> MethodState
resetIndices = set contentsIndices (0,0)

useVarName :: String -> MethodState -> MethodState
useVarName v = over (varNames . at prefix) (Just . max nextSuffix . fromMaybe 0)
  where (prefix, nextSuffix) = over _2 (maybe 0 (+1)) $ splitVarName v

genVarName :: [String] -> String -> MS String
genVarName candidates backup = do
  used <- gets (^. varNames)
  let
    isAvailable (n,c) = maybe True (maybe (const False) (>=) c) $ Map.lookup n used
    choice = foldr const (splitVarName backup) $ filter isAvailable $ map splitVarName candidates
  bumpVarName choice

genLoopIndex :: MS String
genLoopIndex = genVarName ["i", "j", "k"] "i"

genVarNameIf :: Bool -> String -> MS String
genVarNameIf True n = genVarName [] n
genVarNameIf False _ = do
  return ""

varNameAvailable :: String -> MS Bool
varNameAvailable n = do
  used <- gets (^. varNames)
  return $ isNothing $ Map.lookup n used

-- Helpers

ifElemError :: (Eq a) => a -> [a] -> String -> [a]
ifElemError e es err = if e `elem` es then error err else e : es

-- Split the longest numerical (0-9) suffix from the rest of the string
splitVarName :: String -> (String, Maybe Int)
splitVarName = over _2 readMaybe . over both reverse . swap . span isDigit . reverse

bumpVarName :: (String, Maybe Int) -> MS String
bumpVarName (n,c) = do
  count <- gets (^. (varNames . at n))
  let suffix = maybe count (flip fmap count . max) c
  modify $ set (varNames . at n) $ Just $ maybe 0 (+1) suffix
  return $ maybe n ((n ++) . show) count

setVarScope :: String -> ScopeData -> MethodState -> MethodState
setVarScope n s = over varScopes (Map.insert n s)

getVarScope :: String -> MS ScopeData
getVarScope n = do
  sMap <- gets (^. varScopes)
  return $ case Map.lookup n sMap of
    Nothing -> error $ "Variable with no declared scope: " ++ n
    (Just scp) -> scp
