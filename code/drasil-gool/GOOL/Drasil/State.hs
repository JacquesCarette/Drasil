{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (MS, lensGStoMS, lensMStoGS,
  GS, GOOLState(..), headers, sources, hasMain, mainMod, initialState, 
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

  _currFilePath :: FilePath,
  _currModName :: String,
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

type GS = State GOOLState
type MS = State (GOOLState, MethodState)

getMSfromGS :: GOOLState -> (GOOLState, MethodState)
getMSfromGS gs = (gs, initialMS)

setMSfromGS :: GOOLState -> (GOOLState, MethodState) -> GOOLState
setMSfromGS _ (gs, _) = gs

lensGStoMS :: Lens' GOOLState (GOOLState, MethodState)
lensGStoMS = lens getMSfromGS setMSfromGS

lensMStoGS :: Lens' (GOOLState, MethodState) GOOLState
lensMStoGS = _1

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = [],
  _hasMain = False,
  _mainMod = Nothing,

  _currFilePath = "",
  _currModName = "",
  _currMain = False
}

initialMS :: MethodState
initialMS = MS {
  _currParameters = [],

  _currScope = Priv,
  _currMainFunc = False
}

putAfter :: (s -> s) -> State s a -> State s a
putAfter sf sv = do
  v <- sv
  getPutReturn sf v

getPutReturn :: (s -> s) -> a -> State s a
getPutReturn sf v = do
  s <- get
  put $ sf s
  return v

getPutReturnFunc :: GS b -> (GOOLState -> b -> GOOLState) -> 
  (b -> a) -> GS a
getPutReturnFunc st sf vf = do
  v <- st
  s <- get
  put $ sf s v
  return $ vf v

getPutReturnFunc2 :: GS c -> GS b -> 
  (GOOLState -> c -> b -> GOOLState) -> (c -> b -> a) -> GS a
getPutReturnFunc2 st1 st2 sf vf = do
  v1 <- st1
  v2 <- st2
  s <- get
  put $ sf s v1 v2
  return $ vf v1 v2

getPutReturnList :: [GS b] -> (GOOLState -> GOOLState) -> 
  ([b] -> a) -> GS a
getPutReturnList l sf vf = do
  v <- sequence l
  s <- get
  put $ sf s
  return $ vf v

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

setFilePath :: FilePath -> GOOLState -> GOOLState
setFilePath = set currFilePath

getFilePath :: GS FilePath
getFilePath = gets (^. currFilePath)

setModuleName :: String -> GOOLState -> GOOLState
setModuleName = set currModName

getModuleName :: GS String
getModuleName = gets (^. currModName)

setCurrMain :: Bool -> GOOLState -> GOOLState
setCurrMain = set currMain

getCurrMain :: GS Bool
getCurrMain = gets (^. currMain)

setParameters :: [ParamData] -> (GOOLState, MethodState) -> (GOOLState, MethodState)
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