{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GS, GOOLState(..), FS, CS, MS, VS, lensFStoGS, lensGStoFS, lensFStoCS, 
  lensFStoMS, lensFStoVS, lensCStoMS, lensMStoCS, lensCStoVS, lensMStoFS, 
  lensMStoVS, lensVStoFS, lensVStoMS, headers, sources, mainMod, goolState, currMain, 
  currFileType, initialState, initialFS, modifyReturn, modifyReturnFunc, 
  modifyReturnFunc2, modifyReturnList, addODEFilePaths, addFile, 
  addCombinedHeaderSource, addHeader, addSource, addProgNameToPaths, setMainMod,
  addODEFiles, getODEFiles, addLangImport, addLangImportVS, addExceptionImports,
  getLangImports, addLibImport, addLibImportVS, addLibImports, getLibImports, 
  addModuleImport, addModuleImportVS, getModuleImports, addHeaderLangImport, 
  getHeaderLangImports, addHeaderLibImport, getHeaderLibImports, 
  addHeaderModImport, getHeaderModImports, addDefine, getDefines, 
  addHeaderDefine, getHeaderDefines, addUsing, getUsing, addHeaderUsing, 
  getHeaderUsing, setFileType, setModuleName, getModuleName, setClassName, 
  getClassName, setCurrMain, getCurrMain, addClass, getClasses, updateClassMap, 
  getClassMap, updateMethodExcMap, getMethodExcMap, updateCallMap, 
  callMapTransClosure, updateMEMWithCalls, addParameter, getParameters, 
  setOutputsDeclared, isOutputsDeclared, addException, addExceptions, 
  getExceptions, addCall, setMainDoc, getMainDoc, setScope, getScope, 
  setCurrMainFunc, getCurrMainFunc, setODEDepVars, getODEDepVars, 
  setODEOthVars, getODEOthVars
) where

import GOOL.Drasil.AST (FileType(..), ScopeTag(..), FileData)
import GOOL.Drasil.CodeAnalysis (Exception, printExc, hasLoc)
import GOOL.Drasil.CodeType (ClassName)

import Control.Lens (Lens', (^.), lens, makeLenses, over, set)
import Control.Monad.State (State, modify, gets)
import Data.List (sort, nub)
import Data.Maybe (isNothing)
import Data.Map (Map, fromList, insert, union, findWithDefault, mapWithKey)
import qualified Data.Map as Map (empty, map)
import Text.PrettyPrint.HughesPJ (Doc, empty)

data GOOLState = GS {
  _headers :: [FilePath], -- Used by Drasil for doxygen config gen
  _sources :: [FilePath], -- Used by Drasil for doxygen config and Makefile gen
  _mainMod :: Maybe FilePath, -- Used by Drasil generator to access main 
                              -- mod file path (needed in Makefile generation)
  _classMap :: Map String ClassName, -- Used to determine whether an import is 
                                     -- needed when using extClassVar and obj
  _odeFiles :: [FileData],

  -- Only used for Java, to generate correct "throws Exception" declarations
  -- Key format in both maps is ModuleName.MethodName
  _methodExceptionMap :: Map String [Exception], -- Method to exceptions thrown
  _callMap :: Map String [String] -- Method to other methods it calls
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
  
  -- Only used for Python
  _mainDoc :: Doc, -- To print Python's "main" last

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

data MethodState = MS {
  _classState :: ClassState,
  _currParameters :: [String], -- Used to get parameter names when generating 
                               -- function documentation

  -- Only used for Java
  _outputsDeclared :: Bool, -- So Java doesn't redeclare outputs variable when using inOutCall
  _exceptions :: [Exception], -- Used to build methodExceptionMap
  _calls :: [String], -- Used to build CallMap
  
  -- Only used for C++
  _currScope :: ScopeTag, -- Used to maintain correct scope when adding 
                          -- documentation to function in C++
  _currMainFunc :: Bool -- Used by C++ to put documentation for the main
                        -- function in source instead of header file
}
makeLenses ''MethodState

data ValueState = VS {
  _methodState :: MethodState,
  _currODEDepVars :: [String],
  _currODEOthVars :: [String]
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
lensGStoFS = lens (\gs -> initialFS {_goolState = gs}) (const (^. goolState))

lensFStoGS :: Lens' FileState GOOLState
lensFStoGS = goolState

-- FS - CS --

lensFStoCS :: Lens' FileState ClassState
lensFStoCS = lens (\fs -> initialCS {_fileState = fs}) (const (^. fileState))

-- FS - MS --

lensFStoMS :: Lens' FileState MethodState
lensFStoMS = lens (\fs -> initialMS {_classState = initialCS {_fileState = fs}})
  (const (^. lensMStoFS))

lensMStoFS :: Lens' MethodState FileState 
lensMStoFS = classState . fileState

-- CS - MS --

lensCStoMS :: Lens' ClassState 
  MethodState
lensCStoMS = lens (\cs -> initialMS {_classState = cs}) (const (^. classState))

lensMStoCS :: Lens' MethodState
  ClassState
lensMStoCS = classState

-- FS - VS --

lensFStoVS :: Lens' FileState
  ValueState
lensFStoVS = lens 
  (\fs -> initialVS {_methodState = initialMS {_classState = initialCS {
    _fileState = fs}}}) 
  (const (^. lensVStoFS))

lensVStoFS :: Lens' ValueState FileState
lensVStoFS = methodState . lensMStoFS

-- CS - VS --

lensCStoVS :: Lens' ClassState
  ValueState
lensCStoVS = lens 
  (\cs -> initialVS {_methodState = initialMS {_classState = cs}}) 
  (const (^. (methodState . classState)))

-- MS - VS --

lensMStoVS :: Lens' MethodState
  ValueState
lensMStoVS = lens (\ms -> initialVS {_methodState = ms}) 
  (const (^. methodState))

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
  _odeFiles = [],

  _methodExceptionMap = Map.empty,
  _callMap = Map.empty
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

  _outputsDeclared = False,
  _exceptions = [],
  _calls = [],

  _currScope = Priv,
  _currMainFunc = False
}

initialVS :: ValueState
initialVS = VS {
  _methodState = initialMS,
  _currODEDepVars = [],
  _currODEOthVars = []
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

modifyReturnFunc2 :: (c -> b -> s -> s) -> (c -> b -> a) -> State s c -> 
  State s b -> State s a
modifyReturnFunc2 sf vf st1 st2 = do
  v1 <- st1
  v2 <- st2
  modify $ sf v1 v2
  return $ vf v1 v2

modifyReturnList :: [State s b] -> (s -> s) -> 
  ([b] -> a) -> State s a
modifyReturnList l sf vf = do
  v <- sequence l
  modify sf
  return $ vf v

-------------------------------
------- State Modifiers -------
-------------------------------

addODEFilePaths :: GOOLState -> 
  MethodState -> 
  MethodState
addODEFilePaths s = over (lensMStoFS . goolState . headers) 
  (s ^. headers ++)
  . over (lensMStoFS . goolState . sources) (s ^. sources ++)

addFile :: FileType -> FilePath -> GOOLState -> GOOLState
addFile Combined = addCombinedHeaderSource
addFile Source = addSource
addFile Header = addHeader

addHeader :: FilePath -> GOOLState -> GOOLState
addHeader fp = over headers (\h -> if fp `elem` h then 
  error $ "Multiple files with same name encountered: " ++ fp else h ++ [fp])

addSource :: FilePath -> GOOLState -> GOOLState
addSource fp = over sources (\s -> if fp `elem` s then 
  error $ "Multiple files with same name encountered: " ++ fp else s ++ [fp])

addCombinedHeaderSource :: FilePath -> GOOLState -> GOOLState
addCombinedHeaderSource fp = addSource fp . addHeader fp 

addProgNameToPaths :: String -> GOOLState -> GOOLState
addProgNameToPaths n = over mainMod (fmap f) . over sources (map f) . 
  over headers (map f)
  where f = ((n++"/")++)

setMainMod :: String -> GOOLState -> GOOLState
setMainMod n = over mainMod (\m -> if isNothing m then Just n else error 
  "Multiple modules with main methods encountered")

addODEFiles :: [FileData] -> MethodState
  -> MethodState
addODEFiles f = over (lensMStoFS . goolState . odeFiles) (f++)

getODEFiles :: GS [FileData]
getODEFiles = gets (^. odeFiles)

addLangImport :: String -> MethodState 
  -> MethodState
addLangImport i = over (lensMStoFS . langImports) (\is -> 
  if i `elem` is then is else sort $ i:is)
  
addLangImportVS :: String -> 
  ValueState 
  -> ValueState
addLangImportVS i = over methodState (addLangImport i)

addExceptionImports :: [Exception] -> 
  MethodState -> 
  MethodState
addExceptionImports es = over (lensMStoFS . langImports) (\is -> sort $ nub $ 
  is ++ imps)
  where imps = map printExc $ filter hasLoc es

getLangImports :: FS [String]
getLangImports = gets (^. langImports)

addLibImport :: String -> MethodState
  -> MethodState
addLibImport i = over (lensMStoFS . libImports) (\is -> 
  if i `elem` is then is else sort $ i:is)

addLibImportVS :: String -> 
  ValueState -> 
  ValueState
addLibImportVS i = over (lensVStoFS . libImports) 
  (\is -> if i `elem` is then is else sort $ i:is)

addLibImports :: [String] -> MethodState
  -> MethodState
addLibImports is s = foldl (flip addLibImport) s is

getLibImports :: FS [String]
getLibImports = gets (^. libImports)

addModuleImport :: String -> MethodState
  -> MethodState
addModuleImport i = over (lensMStoFS . moduleImports) (\is -> 
  if i `elem` is then is else sort $ i:is)

addModuleImportVS :: String -> 
  ValueState
  -> ValueState
addModuleImportVS i = over methodState (addModuleImport i)

getModuleImports :: FS [String]
getModuleImports = gets (^. moduleImports)

addHeaderLangImport :: String -> 
  ValueState -> 
  ValueState
addHeaderLangImport i = over (lensVStoFS . headerLangImports) 
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderLangImports :: FS [String]
getHeaderLangImports = gets (^. headerLangImports)

addHeaderLibImport :: String -> 
  MethodState -> 
  MethodState
addHeaderLibImport i = over (lensMStoFS . headerLibImports)
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderLibImports :: FS [String]
getHeaderLibImports = gets (^. headerLibImports)

addHeaderModImport :: String -> 
  ValueState -> 
  ValueState
addHeaderModImport i = over (lensVStoFS . headerModImports) 
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderModImports :: FS [String]
getHeaderModImports = gets (^. headerModImports)

addDefine :: String -> 
  ValueState -> 
  ValueState
addDefine d = over (lensVStoFS . defines) (\ds -> if d `elem` ds 
  then ds else sort $ d:ds)

getDefines :: FS [String]
getDefines = gets (^. defines)
  
addHeaderDefine :: String -> 
  ValueState ->
  ValueState
addHeaderDefine d = over (lensVStoFS . headerDefines) (\ds -> 
  if d `elem` ds then ds else sort $ d:ds)

getHeaderDefines :: FS [String]
getHeaderDefines = gets (^. headerDefines)

addUsing :: String -> 
  ValueState -> 
  ValueState
addUsing u = over (lensVStoFS . using) (\us -> if u `elem` us 
  then us else sort $ u:us)

getUsing :: FS [String]
getUsing = gets (^. using)

addHeaderUsing :: String -> 
  ValueState -> 
  ValueState
addHeaderUsing u = over (lensVStoFS . headerUsing) (\us -> 
  if u `elem` us then us else sort $ u:us)

getHeaderUsing :: FS [String]
getHeaderUsing = gets (^. headerUsing)

setMainDoc :: Doc -> MethodState -> 
  MethodState
setMainDoc d = over (lensMStoFS) $ set mainDoc d

getMainDoc :: FS Doc
getMainDoc = gets (^. mainDoc)

setFileType :: FileType -> FileState -> FileState
setFileType = set currFileType

setModuleName :: String -> FileState -> FileState
setModuleName = set currModName

getModuleName :: FS String
getModuleName = gets (^. currModName)

setClassName :: String -> ClassState -> 
  ClassState
setClassName = set currClassName

getClassName :: MS ClassName
getClassName = gets (^. (classState . currClassName))

setCurrMain :: MethodState -> 
  MethodState
setCurrMain = over (lensMStoFS . currMain) (\b -> if b then 
  error "Multiple main functions defined" else not b)

getCurrMain :: FS Bool
getCurrMain = gets (^. currMain)

addClass :: String -> ClassState -> ClassState
addClass c = over (fileState . currClasses) (\cs -> if c `elem` cs then 
  error "Multiple classes with same name in same file" else c:cs)

getClasses :: FS [String]
getClasses = gets (^. currClasses)

updateClassMap :: String -> FileState -> FileState
updateClassMap n fs = over (goolState . classMap) (union (fromList $ 
  zip (repeat n) (fs ^. currClasses))) fs

getClassMap :: VS (Map String String)
getClassMap = gets (^. (lensVStoFS . goolState . classMap))

updateMethodExcMap :: String ->
  MethodState 
  -> MethodState
updateMethodExcMap n ms = over (lensMStoFS . goolState . 
  methodExceptionMap) (insert (mn ++ "." ++ n) (ms ^. exceptions)) ms
  where mn = ms ^. (lensMStoFS . currModName)

getMethodExcMap :: VS (Map String [Exception])
getMethodExcMap = gets (^. (lensVStoFS . goolState . 
  methodExceptionMap))

updateCallMap :: String -> MethodState 
  -> MethodState
updateCallMap n ms = over (lensMStoFS . goolState . callMap) 
  (insert (mn ++ "." ++ n) (ms ^. calls)) ms
  where mn = ms ^. (lensMStoFS . currModName)

callMapTransClosure :: GOOLState -> GOOLState
callMapTransClosure = over callMap tClosure
  where tClosure m = Map.map (traceCalls m) m
        traceCalls :: Map String [String] -> [String] -> [String]
        traceCalls _ [] = []
        traceCalls cm (c:cs) = nub $ c : traceCalls cm (nub $ cs ++ 
          findWithDefault [] c cm)

updateMEMWithCalls :: GOOLState -> GOOLState
updateMEMWithCalls s = over methodExceptionMap (\mem -> mapWithKey 
  (addCallExcs mem (s ^. callMap)) mem) s
  where addCallExcs :: Map String [Exception] -> Map String [String] -> String 
          -> [Exception] -> [Exception]
        addCallExcs mem cm f es = nub $ es ++ concatMap (\fn -> findWithDefault 
          [] fn mem) (findWithDefault [] f cm)

addParameter :: String -> MethodState 
  -> MethodState
addParameter p = over currParameters (\ps -> if p `elem` ps then 
  error $ "Function has duplicate parameter: " ++ p else ps ++ [p])

getParameters :: MS [String]
getParameters = gets (^. currParameters)

setODEDepVars :: [String] -> 
  ValueState -> 
  ValueState
setODEDepVars = set currODEDepVars

getODEDepVars :: VS [String]
getODEDepVars = gets (^. currODEDepVars)

setODEOthVars :: [String] -> 
  ValueState -> 
  ValueState
setODEOthVars = set currODEOthVars

getODEOthVars :: VS [String]
getODEOthVars = gets (^. currODEOthVars)

setOutputsDeclared :: MethodState -> 
  MethodState
setOutputsDeclared = set outputsDeclared True

isOutputsDeclared :: MS Bool
isOutputsDeclared = gets (^. outputsDeclared)

addException :: Exception -> MethodState
  -> MethodState
addException e = over exceptions (\es -> if e `elem` es then es else 
  es ++ [e])

addExceptions :: [Exception] -> 
  ValueState -> 
  ValueState
addExceptions es = over (methodState . exceptions) (\exs -> nub $ exs ++ es)

getExceptions :: MS [Exception]
getExceptions = gets (^. exceptions)

addCall :: String -> 
  ValueState -> 
  ValueState
addCall f = over (methodState . calls) (f:)

setScope :: ScopeTag -> MethodState -> 
  MethodState
setScope = set currScope

getScope :: MS ScopeTag
getScope = gets (^. currScope)

setCurrMainFunc :: Bool -> MethodState 
  -> MethodState
setCurrMainFunc = set currMainFunc

getCurrMainFunc :: MS Bool
getCurrMainFunc = gets (^. currMainFunc)