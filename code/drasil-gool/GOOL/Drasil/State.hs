{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GS, GOOLState(..), FS, CS, MS, VS, lensFStoGS, lensGStoFS, lensFStoCS, 
  lensFStoMS, lensFStoVS, lensCStoMS, lensMStoCS, lensCStoVS, lensMStoFS, 
  lensMStoVS, lensVStoFS, lensVStoMS, headers, sources, mainMod, currMain, 
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

import GOOL.Drasil.Data (FileType(..), ScopeTag(..), Exception(..), FileData)

import Control.Lens (Lens', (^.), lens, makeLenses, over, set)
import Control.Lens.Tuple (_1, _2)
import Control.Monad.State (State, modify, gets)
import Data.List (sort, nub)
import Data.Maybe (isNothing)
import Data.Map (Map, fromList, insert, union, findWithDefault, mapWithKey)
import qualified Data.Map as Map (empty, map)
import Text.PrettyPrint.HughesPJ (Doc, empty)

data GOOLState = GS {
  _headers :: [FilePath],
  _sources :: [FilePath],
  _mainMod :: Maybe FilePath,
  _classMap :: Map String String,
  _odeFiles :: [FileData],

  -- Only used for Java
  _methodExceptionMap :: Map String [Exception],
  _callMap :: Map String [String]
} 
makeLenses ''GOOLState

data MethodState = MS {
  _currParameters :: [String],

  -- Only used for Java
  _outputsDeclared :: Bool,
  _exceptions :: [Exception],
  _calls :: [String],
  
  -- Only used for C++
  _currScope :: ScopeTag,
  _currMainFunc :: Bool
}
makeLenses ''MethodState

newtype ClassState = CS {
  _currClassName :: String
}
makeLenses ''ClassState

data FileState = FS {
  _currModName :: String,
  _currFileType :: FileType,
  _currMain :: Bool,
  _currClasses :: [String],
  _langImports :: [String],
  _libImports :: [String],
  _moduleImports :: [String],
  
  -- Only used for Python
  _mainDoc :: Doc,

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

data ValueState = VS {
  _currODEDepVars :: [String],
  _currODEOthVars :: [String]
}
makeLenses ''ValueState

type GS = State GOOLState
type FS = State (GOOLState, FileState)
type CS = State ((GOOLState, FileState), ClassState)
type MS = State (((GOOLState, FileState), ClassState), MethodState)
type VS = State ((((GOOLState, FileState), ClassState), MethodState), 
  ValueState)

-------------------------------
---- Lenses between States ----
-------------------------------

-- GS - FS --

getFSfromGS :: GOOLState -> (GOOLState, FileState)
getFSfromGS gs = (gs, initialFS)

setFSfromGS :: GOOLState -> (GOOLState, FileState) -> GOOLState
setFSfromGS _ (gs, _) = gs

lensGStoFS :: Lens' GOOLState (GOOLState, FileState)
lensGStoFS = lens getFSfromGS setFSfromGS

lensFStoGS :: Lens' (GOOLState, FileState) GOOLState
lensFStoGS = _1

-- FS - CS --

getCSfromFS :: (GOOLState, FileState) -> ((GOOLState, FileState), ClassState)
getCSfromFS fs = (fs, initialCS)

setCSfromFS :: (GOOLState, FileState) -> ((GOOLState, FileState), ClassState) 
  -> (GOOLState, FileState)
setCSfromFS _ (fs, _) = fs 

lensFStoCS :: Lens' (GOOLState, FileState) ((GOOLState, FileState), ClassState)
lensFStoCS = lens getCSfromFS setCSfromFS

-- FS - MS --

getMSfromFS :: (GOOLState, FileState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
getMSfromFS (gs, fs) = (((gs, fs), initialCS), initialMS)

setMSfromFS :: (GOOLState, FileState) -> 
  (((GOOLState, FileState), ClassState), MethodState) -> (GOOLState, FileState)
setMSfromFS _ ((fs, _), _) = fs

lensFStoMS :: Lens' (GOOLState, FileState) 
  (((GOOLState, FileState), ClassState), MethodState)
lensFStoMS = lens getMSfromFS setMSfromFS

lensMStoFS :: Lens' (((GOOLState, FileState), ClassState), MethodState) 
  (GOOLState, FileState) 
lensMStoFS = _1 . _1

-- CS - MS --

getMSfromCS :: ((GOOLState, FileState), ClassState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
getMSfromCS cs = (cs, initialMS)

setMSfromCS :: ((GOOLState, FileState), ClassState) -> 
  (((GOOLState, FileState), ClassState), MethodState) -> 
  ((GOOLState, FileState), ClassState)
setMSfromCS _ (cs, _) = cs

lensCStoMS :: Lens' ((GOOLState, FileState), ClassState) 
  (((GOOLState, FileState), ClassState), MethodState)
lensCStoMS = lens getMSfromCS setMSfromCS

lensMStoCS :: Lens' (((GOOLState, FileState), ClassState), MethodState)
  ((GOOLState, FileState), ClassState)
lensMStoCS = _1

-- FS - VS --

getVSfromFS :: (GOOLState, FileState) ->
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
getVSfromFS fs = (((fs, initialCS), initialMS), initialVS)

setVSfromFS :: (GOOLState, FileState) ->
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  (GOOLState, FileState)
setVSfromFS _ (((fs, _), _), _) = fs

lensFStoVS :: Lens' (GOOLState, FileState)
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
lensFStoVS = lens getVSfromFS setVSfromFS

lensVStoFS :: Lens' ((((GOOLState, FileState), ClassState), MethodState), 
  ValueState) (GOOLState, FileState)
lensVStoFS = _1 . _1 . _1

-- CS - VS --

getVSfromCS :: ((GOOLState, FileState), ClassState) ->
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
getVSfromCS cs = ((cs, initialMS), initialVS)

setVSfromCS :: ((GOOLState, FileState), ClassState) ->
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  ((GOOLState, FileState), ClassState)
setVSfromCS _ ((cs, _), _) = cs

lensCStoVS :: Lens' ((GOOLState, FileState), ClassState)
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
lensCStoVS = lens getVSfromCS setVSfromCS

-- MS - VS --

getVSfromMS :: (((GOOLState, FileState), ClassState), MethodState) ->
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
getVSfromMS ms = (ms, initialVS)

setVSfromMS :: (((GOOLState, FileState), ClassState), MethodState) ->
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
setVSfromMS _ (ms, _) = ms

lensMStoVS :: Lens' (((GOOLState, FileState), ClassState), MethodState)
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
lensMStoVS = lens getVSfromMS setVSfromMS

lensVStoMS :: Lens' ((((GOOLState, FileState), ClassState), MethodState), 
  ValueState) (((GOOLState, FileState), ClassState), MethodState)
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
  (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
addODEFilePaths s = over (_1 . _1 . _1 . headers) (s ^. headers ++)
  . over (_1 . _1 . _1 . sources) (s ^. sources ++)

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

addODEFiles :: [FileData] -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
addODEFiles f = over _1 $ over _1 $ over _1 $ over odeFiles (f++)

getODEFiles :: GS [FileData]
getODEFiles = gets (^. odeFiles)

addLangImport :: String -> (((GOOLState, FileState), ClassState), MethodState) 
  -> (((GOOLState, FileState), ClassState), MethodState)
addLangImport i = over (_1 . _1 . _2 . langImports) (\is -> 
  if i `elem` is then is else sort $ i:is)
  
addLangImportVS :: String -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) 
  -> ((((GOOLState, FileState), ClassState), MethodState), ValueState)
addLangImportVS i = over _1 (addLangImport i)

addExceptionImports :: [Exception] -> 
  (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
addExceptionImports es = over (_1 . _1 . _2 . langImports) (\is -> sort $ nub $ 
  is ++ imps)
  where mkImport l e = if null l then "" else l ++ "." ++ e
        imps = filter (not . null) $ zipWith mkImport (map loc es) (map exc es)

getLangImports :: FS [String]
getLangImports = gets ((^. langImports) . snd)

addLibImport :: String -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
addLibImport i = over _1 $ over _1 $ over _2 $ over libImports (\is -> 
  if i `elem` is then is else sort $ i:is)

addLibImportVS :: String -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
addLibImportVS i = over _1 $ over _1 $ over _1 $ over _2 $ over libImports 
  (\is -> if i `elem` is then is else sort $ i:is)

addLibImports :: [String] -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
addLibImports is s = foldl (flip addLibImport) s is

getLibImports :: FS [String]
getLibImports = gets ((^. libImports) . snd)

addModuleImport :: String -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
addModuleImport i = over (_1 . _1 . _2 . moduleImports) (\is -> 
  if i `elem` is then is else sort $ i:is)

addModuleImportVS :: String -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
  -> ((((GOOLState, FileState), ClassState), MethodState), ValueState)
addModuleImportVS i = over _1 (addModuleImport i)

getModuleImports :: FS [String]
getModuleImports = gets ((^. moduleImports) . snd)

addHeaderLangImport :: String -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
addHeaderLangImport i = over (_1 . _1 . _1 . _2 . headerLangImports) 
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderLangImports :: FS [String]
getHeaderLangImports = gets ((^. headerLangImports) . snd)

addHeaderLibImport :: String -> 
  (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
addHeaderLibImport i = over _1 $ over _1 $ over _2 $ over headerLibImports 
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderLibImports :: FS [String]
getHeaderLibImports = gets ((^. headerLibImports) . snd)

addHeaderModImport :: String -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
addHeaderModImport i = over (_1 . _1 . _1 . _2 . headerModImports) 
  (\is -> if i `elem` is then is else sort $ i:is)

getHeaderModImports :: FS [String]
getHeaderModImports = gets ((^. headerModImports) . snd)

addDefine :: String -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
addDefine d = over (_1 . _1 . _1 . _2 . defines) (\ds -> if d `elem` ds 
  then ds else sort $ d:ds)

getDefines :: FS [String]
getDefines = gets ((^. defines) . snd)
  
addHeaderDefine :: String -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) ->
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
addHeaderDefine d = over (_1 . _1 . _1 . _2 . headerDefines) (\ds -> 
  if d `elem` ds then ds else sort $ d:ds)

getHeaderDefines :: FS [String]
getHeaderDefines = gets ((^. headerDefines) . snd)

addUsing :: String -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
addUsing u = over (_1 . _1 . _1 . _2 . using) (\us -> if u `elem` us 
  then us else sort $ u:us)

getUsing :: FS [String]
getUsing = gets ((^. using) . snd)

addHeaderUsing :: String -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
addHeaderUsing u = over (_1 . _1 . _1 . _2 . headerUsing) (\us -> 
  if u `elem` us then us else sort $ u:us)

getHeaderUsing :: FS [String]
getHeaderUsing = gets ((^. headerUsing) . snd)

setMainDoc :: Doc -> (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
setMainDoc d = over (_1 . _1 . _2) $ set mainDoc d

getMainDoc :: FS Doc
getMainDoc = gets ((^. mainDoc) . snd)

setFileType :: FileType -> (GOOLState, FileState) -> (GOOLState, FileState)
setFileType ft = over _2 (set currFileType ft)

setModuleName :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
setModuleName n = over _2 (set currModName n)

getModuleName :: FS String
getModuleName = gets ((^. currModName) . snd)

setClassName :: String -> ((GOOLState, FileState), ClassState) -> 
  ((GOOLState, FileState), ClassState)
setClassName n = over _2 (set currClassName n)

getClassName :: MS String
getClassName = gets ((^. currClassName) . snd . fst)

setCurrMain :: (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
setCurrMain = over _1 $ over _1 $ over _2 $ over currMain (\b -> if b then 
  error "Multiple main functions defined" else not b)

getCurrMain :: FS Bool
getCurrMain = gets ((^. currMain) . snd)

addClass :: String -> ((GOOLState, FileState), ClassState) -> (
  (GOOLState, FileState), ClassState)
addClass c = over _1 $ over _2 $ over currClasses (\cs -> if c `elem` cs then 
  error "Multiple classes with same name in same file" else c:cs)

getClasses :: FS [String]
getClasses = gets ((^. currClasses) . snd)

updateClassMap :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
updateClassMap n (gs, fs) = over _1 (over classMap (union (fromList $ 
  zip (repeat n) (fs ^. currClasses)))) (gs, fs)

getClassMap :: VS (Map String String)
getClassMap = gets ((^. classMap) . fst . fst . fst . fst)

updateMethodExcMap :: String ->
  (((GOOLState, FileState), ClassState), MethodState) 
  -> (((GOOLState, FileState), ClassState), MethodState)
updateMethodExcMap n (((gs, fs), cs), ms) = over (_1 . _1 . _1 . 
  methodExceptionMap) (insert (mn ++ "." ++ n) (ms ^. exceptions)) 
  (((gs, fs), cs), ms)
  where mn = fs ^. currModName

getMethodExcMap :: VS (Map String [Exception])
getMethodExcMap = gets ((^. methodExceptionMap) . fst . fst . fst . fst)

updateCallMap :: String -> (((GOOLState, FileState), ClassState), MethodState) 
  -> (((GOOLState, FileState), ClassState), MethodState)
updateCallMap n (((gs, fs), cs), ms) = over (_1 . _1 . _1 . callMap) 
  (insert (mn ++ "." ++ n) (ms ^. calls)) (((gs, fs), cs), ms)
  where mn = fs ^. currModName

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

addParameter :: String -> (((GOOLState, FileState), ClassState), MethodState) 
  -> (((GOOLState, FileState), ClassState), MethodState)
addParameter p = over _2 $ over currParameters (\ps -> if p `elem` ps then 
  error $ "Function has duplicate parameter: " ++ p else ps ++ [p])

getParameters :: MS [String]
getParameters = gets ((^. currParameters) . snd)

setODEDepVars :: [String] -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
setODEDepVars vs = over _2 $ set currODEDepVars vs

getODEDepVars :: VS [String]
getODEDepVars = gets ((^. currODEDepVars) . snd)

setODEOthVars :: [String] -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
setODEOthVars vs = over _2 $ set currODEOthVars vs

getODEOthVars :: VS [String]
getODEOthVars = gets ((^. currODEOthVars) . snd)

setOutputsDeclared :: (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
setOutputsDeclared = over _2 $ set outputsDeclared True

isOutputsDeclared :: MS Bool
isOutputsDeclared = gets ((^. outputsDeclared) . snd)

addException :: Exception -> (((GOOLState, FileState), ClassState), MethodState)
  -> (((GOOLState, FileState), ClassState), MethodState)
addException e = over (_2 . exceptions) (\es -> if e `elem` es then es else 
  es ++ [e])

addExceptions :: [Exception] -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
addExceptions es = over (_1 . _2 . exceptions) (\exs -> nub $ exs ++ es)

getExceptions :: MS [Exception]
getExceptions = gets ((^. exceptions) . snd)

addCall :: String -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState) -> 
  ((((GOOLState, FileState), ClassState), MethodState), ValueState)
addCall f = over (_1 . _2 . calls) (f:)

setScope :: ScopeTag -> (((GOOLState, FileState), ClassState), MethodState) -> 
  (((GOOLState, FileState), ClassState), MethodState)
setScope scp = over _2 $ set currScope scp

getScope :: MS ScopeTag
getScope = gets ((^. currScope) . snd)

setCurrMainFunc :: Bool -> (((GOOLState, FileState), ClassState), MethodState) 
  -> (((GOOLState, FileState), ClassState), MethodState)
setCurrMainFunc m = over _2 $ set currMainFunc m

getCurrMainFunc :: MS Bool
getCurrMainFunc = gets ((^. currMainFunc) . snd)