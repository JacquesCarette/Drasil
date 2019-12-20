{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GS, GOOLState(..), FS, MS, lensFStoGS, lensGStoFS, lensFStoMS, lensMStoFS, 
  headers, sources, mainMod, currMain, initialState, initialFS, putAfter, 
  getPutReturn, getPutReturnFunc, getPutReturnFunc2, getPutReturnList, addFile, 
  addCombinedHeaderSource, addHeader, addSource, addProgNameToPaths, setMainMod,
  addLangImport, getLangImports, addModuleImport, getModuleImports, 
  addHeaderLangImport, getHeaderLangImports, addHeaderModImport, 
  getHeaderModImports, addDefine, getDefines, addHeaderDefine, getHeaderDefines,
  addUsing, getUsing, addHeaderUsing, getHeaderUsing, setFilePath, getFilePath, 
  setModuleName, getModuleName, setCurrMain, getCurrMain, addClass, getClasses, 
  updateClassMap, getClassMap, addParameter, getParameters, setScope, getScope, 
  setCurrMainFunc, getCurrMainFunc
) where

import GOOL.Drasil.Data (FileType(..), ScopeTag(..))

import Control.Lens (Lens', (^.), lens, makeLenses, over, set)
import Control.Lens.Tuple (_1, _2)
import Control.Monad.State (State, get, put, gets)
import Data.Maybe (isNothing)
import Data.Map (Map, fromList, empty, union)

data GOOLState = GS {
  _headers :: [FilePath],
  _sources :: [FilePath],
  _mainMod :: Maybe FilePath,
  _classMap :: Map String String
} 
makeLenses ''GOOLState

data MethodState = MS {
  _currParameters :: [String],
  
  -- Only used for C++
  _currScope :: ScopeTag,
  _currMainFunc :: Bool
}
makeLenses ''MethodState

data FileState = FS {
  _currModName :: String,
  _currFilePath :: FilePath,
  _currMain :: Bool,
  _currClasses :: [String],
  _langImports :: [String],
  _moduleImports :: [String],

  -- C++ only
  _headerLangImports :: [String],
  _headerModImports :: [String],
  _defines :: [String],
  _headerDefines :: [String],
  _using :: [String],
  _headerUsing :: [String]
}
makeLenses ''FileState

type GS = State GOOLState
type FS = State (GOOLState, FileState)
type MS = State ((GOOLState, FileState), MethodState)

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

-- FS - MS --

getMSfromFS :: (GOOLState, FileState) -> ((GOOLState, FileState), MethodState)
getMSfromFS (gs, fs) = ((gs, fs), initialMS)

setMSfromFS :: (GOOLState, FileState) -> ((GOOLState, FileState), MethodState)
  -> (GOOLState, FileState)
setMSfromFS _ (fs, _) = fs

lensFStoMS :: Lens' (GOOLState, FileState) ((GOOLState, FileState), MethodState)
lensFStoMS = lens getMSfromFS setMSfromFS

lensMStoFS :: Lens' ((GOOLState, FileState), MethodState) (GOOLState, FileState)
lensMStoFS = _1

-------------------------------
------- Initial States -------
-------------------------------

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = [],
  _mainMod = Nothing,
  _classMap = empty
}

initialFS :: FileState
initialFS = FS {
  _currModName = "",
  _currFilePath = "",
  _currMain = False,
  _currClasses = [],
  _langImports = [],
  _moduleImports = [],

  _headerLangImports = [],
  _headerModImports = [],
  _defines = [],
  _headerDefines = [],
  _using = [],
  _headerUsing = []
}

initialMS :: MethodState
initialMS = MS {
  _currParameters = [],

  _currScope = Priv,
  _currMainFunc = False
}

-------------------------------
------- State Patterns -------
-------------------------------

putAfter :: (s -> s) -> State s a -> State s a
putAfter sf sv = do
  v <- sv
  getPutReturn sf v

getPutReturn :: (s -> s) -> a -> State s a
getPutReturn sf v = do
  s <- get
  put $ sf s
  return v

getPutReturnFunc :: (s -> b -> s) -> (b -> a) -> State s b -> State s a
getPutReturnFunc sf vf st = do
  v <- st
  s <- get
  put $ sf s v
  return $ vf v

getPutReturnFunc2 :: (s -> c -> b -> s) -> (c -> b -> a) -> State s c -> 
  State s b -> State s a
getPutReturnFunc2 sf vf st1 st2 = do
  v1 <- st1
  v2 <- st2
  s <- get
  put $ sf s v1 v2
  return $ vf v1 v2

getPutReturnList :: [State s b] -> (s -> s) -> 
  ([b] -> a) -> State s a
getPutReturnList l sf vf = do
  v <- sequence l
  s <- get
  put $ sf s
  return $ vf v

-------------------------------
------- State Modifiers -------
-------------------------------

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

addLangImport :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
addLangImport i = over _2 $ over langImports (\is -> if i `elem` is then is 
  else i:is)

getLangImports :: FS [String]
getLangImports = gets ((^. langImports) . snd)

addModuleImport :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
addModuleImport i = over _2 $ over moduleImports (\is -> if i `elem` is then is 
  else i:is)

getModuleImports :: FS [String]
getModuleImports = gets ((^. moduleImports) . snd)

addHeaderLangImport :: String -> (GOOLState, FileState) -> (GOOLState, 
  FileState)
addHeaderLangImport i = over _2 $ over headerLangImports (\is -> if i `elem` is 
  then is else i:is)

getHeaderLangImports :: FS [String]
getHeaderLangImports = gets ((^. headerLangImports) . snd)

addHeaderModImport :: String -> (GOOLState, FileState) -> (GOOLState, 
  FileState)
addHeaderModImport i = over _2 $ over headerModImports (\is -> if i `elem` is 
  then is else i:is)

getHeaderModImports :: FS [String]
getHeaderModImports = gets ((^. headerModImports) . snd)

addDefine :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
addDefine d = over _2 $ over defines (\ds -> if d `elem` ds then ds else d:ds)

getDefines :: FS [String]
getDefines = gets ((^. defines) . snd)
  
addHeaderDefine :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
addHeaderDefine d = over _2 $ over headerDefines (\ds -> if d `elem` ds then ds 
  else d:ds)

getHeaderDefines :: FS [String]
getHeaderDefines = gets ((^. headerDefines) . snd)

addUsing :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
addUsing u = over _2 $ over using (\us -> if u `elem` us then us else u:us)

getUsing :: FS [String]
getUsing = gets ((^. using) . snd)

addHeaderUsing :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
addHeaderUsing u = over _2 $ over headerUsing (\us -> if u `elem` us then us 
  else u:us)

getHeaderUsing :: FS [String]
getHeaderUsing = gets ((^. headerUsing) . snd)

setFilePath :: FilePath -> (GOOLState, FileState) -> (GOOLState, FileState)
setFilePath fp = over _2 (set currFilePath fp)

getFilePath :: FS FilePath
getFilePath = gets ((^. currFilePath) . snd)

setModuleName :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
setModuleName n = over _2 (set currModName n)

getModuleName :: FS String
getModuleName = gets ((^. currModName) . snd)

setCurrMain :: ((GOOLState, FileState), MethodState) -> 
  ((GOOLState, FileState), MethodState)
setCurrMain = over _1 (over _2 (over currMain (\b -> if b then error 
  "Multiple main functions defined" else not b)))

getCurrMain :: FS Bool
getCurrMain = gets ((^. currMain) . snd)

addClass :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
addClass c = over _2 (over currClasses (\cs -> if c `elem` cs then error
  "Multiple classes with same name in same file" else c:cs))

getClasses :: FS [String]
getClasses = gets ((^. currClasses) . snd)

updateClassMap :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
updateClassMap n (gs, fs) = over _1 (over classMap (union (fromList $ 
  zip (repeat n) (fs ^. currClasses)))) (gs, fs)

getClassMap :: GS (Map String String)
getClassMap = gets (^. classMap)

addParameter :: String -> ((GOOLState, FileState), MethodState) -> 
  ((GOOLState, FileState), MethodState)
addParameter p = over _2 $ over currParameters (\ps -> if p `elem` ps then 
  error $ "Function has duplicate parameter: " ++ p else ps ++ [p])

getParameters :: MS [String]
getParameters = gets ((^. currParameters) . snd)

setScope :: ScopeTag -> ((GOOLState, FileState), MethodState) -> 
  ((GOOLState, FileState), MethodState)
setScope scp = over _2 $ set currScope scp

getScope :: MS ScopeTag
getScope = gets ((^. currScope) . snd)

setCurrMainFunc :: Bool -> ((GOOLState, FileState), MethodState) -> 
  ((GOOLState, FileState), MethodState)
setCurrMainFunc m = over _2 $ set currMainFunc m

getCurrMainFunc :: MS Bool
getCurrMainFunc = gets ((^. currMainFunc) . snd)