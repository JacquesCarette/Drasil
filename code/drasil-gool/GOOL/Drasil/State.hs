{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GS, GOOLState(..), FS, MS, lensFStoGS, lensGStoFS, lensFStoMS, lensMStoGS,  
  headers, sources, mainMod, currMain, initialState, initialFS, putAfter, 
  getPutReturn, getPutReturnFunc, getPutReturnFunc2, getPutReturnList, addFile, 
  addCombinedHeaderSource, addHeader, addSource, addProgNameToPaths, setMainMod,
  addLangImport, setFilePath, getFilePath, setModuleName, getModuleName, 
  setCurrMain, getCurrMain, addParameter, getParameters, setScope, getScope, 
  setCurrMainFunc, getCurrMainFunc
) where

import GOOL.Drasil.Data (FileType(..), ScopeTag(..))

import Control.Lens (Lens', (^.), lens, makeLenses, over, set)
import Control.Lens.Tuple (_1, _2)
import Control.Monad.State (State, get, put, gets)
import Data.Maybe (isNothing)

data GOOLState = GS {
  _headers :: [FilePath],
  _sources :: [FilePath],
  _mainMod :: Maybe FilePath,
  _langImports :: [String]
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
  _currMain :: Bool
}
makeLenses ''FileState

type GS = State GOOLState
type FS = State (GOOLState, FileState)
type MS = State (GOOLState, (FileState, MethodState))

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

getMSfromFS :: (GOOLState, FileState) -> (GOOLState, (FileState, MethodState))
getMSfromFS (gs, fs) = (gs, (fs, initialMS))

setMSfromFS :: (GOOLState, FileState) -> (GOOLState, (FileState, MethodState))
  -> (GOOLState, FileState)
setMSfromFS _ (gs, (fs, _)) = (gs, fs)

lensFStoMS :: Lens' (GOOLState, FileState) (GOOLState, (FileState, MethodState))
lensFStoMS = lens getMSfromFS setMSfromFS

-- MS - GS --

lensMStoGS :: Lens' (GOOLState, (FileState, MethodState)) GOOLState
lensMStoGS = _1

-------------------------------
------- Initial States -------
-------------------------------

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = [],
  _mainMod = Nothing,
  _langImports = []
}

initialFS :: FileState
initialFS = FS {
  _currModName = "",
  _currFilePath = "",
  _currMain = False
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

getPutReturnFunc :: State s b -> (s -> b -> s) -> (b -> a) -> State s a
getPutReturnFunc st sf vf = do
  v <- st
  s <- get
  put $ sf s v
  return $ vf v

getPutReturnFunc2 :: State s c -> State s b -> 
  (s -> c -> b -> s) -> (c -> b -> a) -> State s a
getPutReturnFunc2 st1 st2 sf vf = do
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

addLangImport :: String -> GOOLState -> GOOLState
addLangImport i = over langImports (\is -> if i `elem` is then i:is else is)

setFilePath :: FilePath -> (GOOLState, FileState) -> (GOOLState, FileState)
setFilePath fp = over _2 (set currFilePath fp)

getFilePath :: FS FilePath
getFilePath = gets ((^. currFilePath) . snd)

setModuleName :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
setModuleName n = over _2 (set currModName n)

getModuleName :: FS String
getModuleName = gets ((^. currModName) . snd)

setCurrMain :: (GOOLState, (FileState, MethodState)) -> 
  (GOOLState, (FileState, MethodState))
setCurrMain = over _2 (over _1 (over currMain (\b -> if b then error 
  "Multiple main functions defined" else not b)))

getCurrMain :: FS Bool
getCurrMain = gets ((^. currMain) . snd)

addParameter :: String -> (GOOLState, (FileState, MethodState)) -> 
  (GOOLState, (FileState, MethodState))
addParameter p = over _2 $ over _2 $ over currParameters (\ps -> if p `elem` 
  ps then error $ "Function has duplicate parameter: " ++ p else ps ++ [p])

getParameters :: MS [String]
getParameters = gets ((^. currParameters) . snd . snd)

setScope :: ScopeTag -> (GOOLState, (FileState, MethodState)) -> 
  (GOOLState, (FileState, MethodState))
setScope scp = over _2 $ over _2 $ set currScope scp

getScope :: MS ScopeTag
getScope = gets ((^. currScope) . snd . snd)

setCurrMainFunc :: Bool -> (GOOLState, (FileState, MethodState)) -> 
  (GOOLState, (FileState, MethodState))
setCurrMainFunc m = over _2 $ over _2 $ set currMainFunc m

getCurrMainFunc :: MS Bool
getCurrMainFunc = gets ((^. currMainFunc) . snd . snd)