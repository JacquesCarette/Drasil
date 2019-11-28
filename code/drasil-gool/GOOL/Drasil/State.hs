{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GS, GOOLState(..), FS, MS, lensFStoGS, lensGStoFS, lensFStoMS, lensGStoMS, 
  lensMStoGS,  headers, sources, hasMain, mainMod, initialState, initialFS, 
  putAfter, getPutReturn, getPutReturnFunc, getPutReturnFunc2, getPutReturnList,
  addFile, addCombinedHeaderSource, addHeader, addSource, addProgNameToPaths, 
  setMain, setMainMod, setFilePath, getFilePath, setModuleName, getModuleName,
  setCurrMain, getCurrMain, setParameters, getParameters, setScope, getScope, 
  setCurrMainFunc, getCurrMainFunc
) where

import GOOL.Drasil.Data (FileType(..), ParamData, ScopeTag(..))

import Control.Lens (Lens', (^.), lens, makeLenses, over, set)
import Control.Lens.Tuple (_1, _2)
import Control.Monad.State (State, get, put, gets)

data GOOLState = GS {
  _headers :: [FilePath],
  _sources :: [FilePath],
  _hasMain :: Bool,
  _mainMod :: Maybe FilePath,

  _currMain :: Bool
} 
makeLenses ''GOOLState

data MethodState = MS {
  _currParameters :: [ParamData],
  
  -- Only used for C++
  _currScope :: ScopeTag,
  _currMainFunc :: Bool
}
makeLenses ''MethodState

data FileState = FS {
  _currModName :: String,
  _currFilePath :: FilePath
}
makeLenses ''FileState

type GS = State GOOLState
type FS = State (GOOLState, FileState)
type MS = State (GOOLState, MethodState)

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

getMSfromFS :: (GOOLState, FileState) -> (GOOLState, MethodState)
getMSfromFS (gs, _) = (gs, initialMS)

setMSfromFS :: (GOOLState, FileState) -> (GOOLState, MethodState) -> 
  (GOOLState, FileState)
setMSfromFS (_, ms) (gs, _) = (gs, ms)

lensFStoMS :: Lens' (GOOLState, FileState) (GOOLState, MethodState)
lensFStoMS = lens getMSfromFS setMSfromFS

-- GS - MS --

getMSfromGS :: GOOLState -> (GOOLState, MethodState)
getMSfromGS gs = (gs, initialMS)

setMSfromGS :: GOOLState -> (GOOLState, MethodState) -> GOOLState
setMSfromGS _ (gs, _) = gs

lensGStoMS :: Lens' GOOLState (GOOLState, MethodState)
lensGStoMS = lens getMSfromGS setMSfromGS

lensMStoGS :: Lens' (GOOLState, MethodState) GOOLState
lensMStoGS = _1

-------------------------------
------- Initial States -------
-------------------------------

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = [],
  _hasMain = False,
  _mainMod = Nothing,

  _currMain = False
}

initialFS :: FileState
initialFS = FS {
  _currModName = "",
  _currFilePath = ""
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

setMain :: (GOOLState, MethodState) -> (GOOLState, MethodState)
setMain = over _1 (over hasMain (\b -> if b then error "Multiple main functions defined"
  else not b)) 

setMainMod :: String -> GOOLState -> GOOLState
setMainMod n = set mainMod (Just n)

setFilePath :: FilePath -> (GOOLState, FileState) -> (GOOLState, FileState)
setFilePath fp = over _2 (set currFilePath fp)

getFilePath :: FS FilePath
getFilePath = gets ((^. currFilePath) . snd)

setModuleName :: String -> (GOOLState, FileState) -> (GOOLState, FileState)
setModuleName n = over _2 (set currModName n)

getModuleName :: FS String
getModuleName = gets ((^. currModName) . snd)

setCurrMain :: Bool -> GOOLState -> GOOLState
setCurrMain = set currMain

getCurrMain :: GS Bool
getCurrMain = gets (^. currMain)

setParameters :: [ParamData] -> (GOOLState, MethodState) -> 
  (GOOLState, MethodState)
setParameters ps = over _2 (set currParameters ps) 

getParameters :: MS [ParamData]
getParameters = gets ((^. currParameters) . snd)

setScope :: ScopeTag -> (GOOLState, MethodState) -> (GOOLState, MethodState)
setScope scp = over _2 (set currScope scp)

getScope :: MS ScopeTag
getScope = gets ((^. currScope) . snd)

setCurrMainFunc :: Bool -> (GOOLState, MethodState) -> (GOOLState, MethodState)
setCurrMainFunc m = over _2 (set currMainFunc m)

getCurrMainFunc :: MS Bool
getCurrMainFunc = gets ((^. currMainFunc) . snd)