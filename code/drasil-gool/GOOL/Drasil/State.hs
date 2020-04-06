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
import Control.Lens.Tuple (_1, _2)
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
  _currODEDepVars :: [String],
  _currODEOthVars :: [String]
}
makeLenses ''ValueState

type GS = State GOOLState
type FS = State FileState
type CS = State ClassState
type MS = State (ClassState, MethodState)
type VS = State ((ClassState, MethodState), 
  ValueState)

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

getMSfromFS :: FileState -> 
  (ClassState, MethodState)
getMSfromFS fs = ((initialCS {_fileState = fs}), initialMS)

setMSfromFS :: FileState -> 
  (ClassState, MethodState) -> FileState
setMSfromFS _ (cs, _) = cs ^. fileState

lensFStoMS :: Lens' FileState (ClassState, MethodState)
lensFStoMS = lens getMSfromFS setMSfromFS

lensMStoFS :: Lens' (ClassState, MethodState) FileState 
lensMStoFS = _1 . fileState

-- CS - MS --

getMSfromCS :: ClassState -> 
  (ClassState, MethodState)
getMSfromCS cs = (cs, initialMS)

setMSfromCS :: ClassState -> 
  (ClassState, MethodState) -> 
  ClassState
setMSfromCS _ (cs, _) = cs

lensCStoMS :: Lens' ClassState 
  (ClassState, MethodState)
lensCStoMS = lens getMSfromCS setMSfromCS

lensMStoCS :: Lens' (ClassState, MethodState)
  ClassState
lensMStoCS = _1

-- FS - VS --

getVSfromFS :: FileState ->
  ((ClassState, MethodState), ValueState)
getVSfromFS fs = (((initialCS {_fileState = fs}), initialMS), initialVS)

setVSfromFS :: FileState ->
  ((ClassState, MethodState), ValueState) -> 
  FileState
setVSfromFS _ ((cs, _), _) = cs ^. fileState

lensFStoVS :: Lens' FileState
  ((ClassState, MethodState), ValueState)
lensFStoVS = lens getVSfromFS setVSfromFS

lensVStoFS :: Lens' ((ClassState, MethodState), 
  ValueState) FileState
lensVStoFS = _1 . _1 . fileState

-- CS - VS --

getVSfromCS :: ClassState ->
  ((ClassState, MethodState), ValueState)
getVSfromCS cs = ((cs, initialMS), initialVS)

setVSfromCS :: ClassState ->
  ((ClassState, MethodState), ValueState) -> 
  ClassState
setVSfromCS _ ((cs, _), _) = cs

lensCStoVS :: Lens' ClassState
  ((ClassState, MethodState), ValueState)
lensCStoVS = lens getVSfromCS setVSfromCS

-- MS - VS --

getVSfromMS :: (ClassState, MethodState) ->
  ((ClassState, MethodState), ValueState)
getVSfromMS ms = (ms, initialVS)

setVSfromMS :: (ClassState, MethodState) ->
  ((ClassState, MethodState), ValueState) -> 
  (ClassState, MethodState)
setVSfromMS _ (ms, _) = ms

lensMStoVS :: Lens' (ClassState, MethodState)
  ((ClassState, MethodState), ValueState)
lensMStoVS = lens getVSfromMS setVSfromMS

lensVStoMS :: Lens' ((ClassState, MethodState), 
  ValueState) (ClassState, MethodState)
lensVStoMS = _1

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
  _currParameters = [],

  _outputsDeclared = False,
  _exceptions = [],
  _calls = [],

  _currScope = Priv,
  _currMainFunc = False
}

initialVS :: ValueState
initialVS = VS {
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
  (ClassState, MethodState) -> 
  (ClassState, MethodState)
addODEFilePaths s = over (_1 . fileState . goolState . headers) (s ^. headers ++)
  . over (_1 . fileState . goolState . sources) (s ^. sources ++)

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

addODEFiles :: [FileData] -> (ClassState, MethodState)
  -> (ClassState, MethodState)
addODEFiles f = over _1 $ over fileState $ over goolState $ over odeFiles (f++)

getODEFiles :: GS [FileData]
getODEFiles = gets (^. odeFiles)

addLangImport :: String -> (ClassState, MethodState) 
  -> (ClassState, MethodState)
addLangImport i = over (_1 . fileState . langImports) (\is -> 
  if i `elem` is then is else sort $ i:is)
  
addLangImportVS :: String -> 
  ((ClassState, MethodState), ValueState) 
  -> ((ClassState, MethodState), ValueState)
addLangImportVS i = over _1 (addLangImport i)

addExceptionImports :: [Exception] -> 
  (ClassState, MethodState) -> 
  (ClassState, MethodState)
addExceptionImports es = over (_1 . fileState . langImports) (\is -> sort $ nub $ 
  is ++ imps)
  where imps = map printExc $ filter hasLoc es

getLangImports :: FS [String]
getLangImports = gets (^. langImports)

addLibImport :: String -> (ClassState, MethodState)
  -> (ClassState, MethodState)
addLibImport i = over _1 $ over fileState $ over libImports (\is -> 
  if i `elem` is then is else sort $ i:is)

addLibImportVS :: String -> 
  ((ClassState, MethodState), ValueState) -> 
  ((ClassState, MethodState), ValueState)
addLibImportVS i = over _1 $ over _1 $ over fileState $ over libImports 
  (\is -> if i `elem` is then is else sort $ i:is)

addLibImports :: [String] -> (ClassState, MethodState)
  -> (ClassState, MethodState)
addLibImports is s = foldl (flip addLibImport) s is

getLibImports :: FS [String]
getLibImports = gets (^. libImports)

addModuleImport :: String -> (ClassState, MethodState)
  -> (ClassState, MethodState)
addModuleImport i = over (_1 . fileState . moduleImports) (\is -> 
  if i `elem` is then is else sort $ i:is)

addModuleImportVS :: String -> 
  ((ClassState, MethodState), ValueState)
  -> ((ClassState, MethodState), ValueState)
addModuleImportVS i = over _1 (addModuleImport i)

getModuleImports :: FS [String]
getModuleImports = gets (^. moduleImports)

addHeaderLangImport :: String -> 
  ((ClassState, MethodState), ValueState) -> 
  ((ClassState, MethodState), ValueState)
addHeaderLangImport i = over (_1 . _1 . fileState . headerLangImports) 
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderLangImports :: FS [String]
getHeaderLangImports = gets (^. headerLangImports)

addHeaderLibImport :: String -> 
  (ClassState, MethodState) -> 
  (ClassState, MethodState)
addHeaderLibImport i = over _1 $ over fileState $ over headerLibImports 
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderLibImports :: FS [String]
getHeaderLibImports = gets (^. headerLibImports)

addHeaderModImport :: String -> 
  ((ClassState, MethodState), ValueState) -> 
  ((ClassState, MethodState), ValueState)
addHeaderModImport i = over (_1 . _1 . fileState . headerModImports) 
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderModImports :: FS [String]
getHeaderModImports = gets (^. headerModImports)

addDefine :: String -> 
  ((ClassState, MethodState), ValueState) -> 
  ((ClassState, MethodState), ValueState)
addDefine d = over (_1 . _1 . fileState . defines) (\ds -> if d `elem` ds 
  then ds else sort $ d:ds)

getDefines :: FS [String]
getDefines = gets (^. defines)
  
addHeaderDefine :: String -> 
  ((ClassState, MethodState), ValueState) ->
  ((ClassState, MethodState), ValueState)
addHeaderDefine d = over (_1 . _1 . fileState . headerDefines) (\ds -> 
  if d `elem` ds then ds else sort $ d:ds)

getHeaderDefines :: FS [String]
getHeaderDefines = gets (^. headerDefines)

addUsing :: String -> 
  ((ClassState, MethodState), ValueState) -> 
  ((ClassState, MethodState), ValueState)
addUsing u = over (_1 . _1 . fileState . using) (\us -> if u `elem` us 
  then us else sort $ u:us)

getUsing :: FS [String]
getUsing = gets (^. using)

addHeaderUsing :: String -> 
  ((ClassState, MethodState), ValueState) -> 
  ((ClassState, MethodState), ValueState)
addHeaderUsing u = over (_1 . _1 . fileState . headerUsing) (\us -> 
  if u `elem` us then us else sort $ u:us)

getHeaderUsing :: FS [String]
getHeaderUsing = gets (^. headerUsing)

setMainDoc :: Doc -> (ClassState, MethodState) -> 
  (ClassState, MethodState)
setMainDoc d = over (_1 . fileState) $ set mainDoc d

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
getClassName = gets ((^. currClassName) . fst)

setCurrMain :: (ClassState, MethodState) -> 
  (ClassState, MethodState)
setCurrMain = over _1 $ over fileState $ over currMain (\b -> if b then 
  error "Multiple main functions defined" else not b)

getCurrMain :: FS Bool
getCurrMain = gets (^. currMain)

addClass :: String -> ClassState -> ClassState
addClass c = over fileState $ over currClasses (\cs -> if c `elem` cs then 
  error "Multiple classes with same name in same file" else c:cs)

getClasses :: FS [String]
getClasses = gets (^. currClasses)

updateClassMap :: String -> FileState -> FileState
updateClassMap n fs = over goolState (over classMap (union (fromList $ 
  zip (repeat n) (fs ^. currClasses)))) fs

getClassMap :: VS (Map String String)
getClassMap = gets ((^. (fileState . goolState . classMap)) . fst . fst)

updateMethodExcMap :: String ->
  (ClassState, MethodState) 
  -> (ClassState, MethodState)
updateMethodExcMap n (cs, ms) = over (_1 . fileState . goolState . 
  methodExceptionMap) (insert (mn ++ "." ++ n) (ms ^. exceptions)) 
  (cs, ms)
  where mn = cs ^. (fileState . currModName)

getMethodExcMap :: VS (Map String [Exception])
getMethodExcMap = gets ((^. (fileState . goolState . methodExceptionMap)) . fst . fst)

updateCallMap :: String -> (ClassState, MethodState) 
  -> (ClassState, MethodState)
updateCallMap n (cs, ms) = over (_1 . fileState . goolState . callMap) 
  (insert (mn ++ "." ++ n) (ms ^. calls)) (cs, ms)
  where mn = cs ^. (fileState . currModName)

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

addParameter :: String -> (ClassState, MethodState) 
  -> (ClassState, MethodState)
addParameter p = over _2 $ over currParameters (\ps -> if p `elem` ps then 
  error $ "Function has duplicate parameter: " ++ p else ps ++ [p])

getParameters :: MS [String]
getParameters = gets ((^. currParameters) . snd)

setODEDepVars :: [String] -> 
  ((ClassState, MethodState), ValueState) -> 
  ((ClassState, MethodState), ValueState)
setODEDepVars vs = over _2 $ set currODEDepVars vs

getODEDepVars :: VS [String]
getODEDepVars = gets ((^. currODEDepVars) . snd)

setODEOthVars :: [String] -> 
  ((ClassState, MethodState), ValueState) -> 
  ((ClassState, MethodState), ValueState)
setODEOthVars vs = over _2 $ set currODEOthVars vs

getODEOthVars :: VS [String]
getODEOthVars = gets ((^. currODEOthVars) . snd)

setOutputsDeclared :: (ClassState, MethodState) -> 
  (ClassState, MethodState)
setOutputsDeclared = over _2 $ set outputsDeclared True

isOutputsDeclared :: MS Bool
isOutputsDeclared = gets ((^. outputsDeclared) . snd)

addException :: Exception -> (ClassState, MethodState)
  -> (ClassState, MethodState)
addException e = over (_2 . exceptions) (\es -> if e `elem` es then es else 
  es ++ [e])

addExceptions :: [Exception] -> 
  ((ClassState, MethodState), ValueState) -> 
  ((ClassState, MethodState), ValueState)
addExceptions es = over (_1 . _2 . exceptions) (\exs -> nub $ exs ++ es)

getExceptions :: MS [Exception]
getExceptions = gets ((^. exceptions) . snd)

addCall :: String -> 
  ((ClassState, MethodState), ValueState) -> 
  ((ClassState, MethodState), ValueState)
addCall f = over (_1 . _2 . calls) (f:)

setScope :: ScopeTag -> (ClassState, MethodState) -> 
  (ClassState, MethodState)
setScope scp = over _2 $ set currScope scp

getScope :: MS ScopeTag
getScope = gets ((^. currScope) . snd)

setCurrMainFunc :: Bool -> (ClassState, MethodState) 
  -> (ClassState, MethodState)
setCurrMainFunc m = over _2 $ set currMainFunc m

getCurrMainFunc :: MS Bool
getCurrMainFunc = gets ((^. currMainFunc) . snd)