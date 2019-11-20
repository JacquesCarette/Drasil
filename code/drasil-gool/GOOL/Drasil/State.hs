{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GS, GOOLState(..), headers, sources, hasMain, mainMod, initialState, 
  putAfter, getPutReturn, getPutReturnFunc, getPutReturnFunc2, getPutReturnList,
  addFile, addCombinedHeaderSource, addHeader, addSource, addProgNameToPaths, 
  setMain, setMainMod, setFilePath, getFilePath, setModuleName, getModuleName, 
  setCurrMain, getCurrMain, setParameters, getParameters, setScope, getScope, 
  setCurrMainFunc, getCurrMainFunc
) where

import GOOL.Drasil.Data (FileType(..), ParamData, ScopeTag(..))

import Control.Lens (makeLenses,over,set,(^.))
import Control.Monad.State (State, get, put, gets)

data GOOLState = GS {
  _headers :: [FilePath],
  _sources :: [FilePath],
  _hasMain :: Bool,
  _mainMod :: Maybe FilePath,

  _currFilePath :: FilePath,
  _currModName :: String,
  _currMain :: Bool,
  _currParameters :: [ParamData],
  
  -- Only used for C++
  _currScope :: ScopeTag,
  _currMainFunc :: Bool
} 
makeLenses ''GOOLState

type GS = State GOOLState

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = [],
  _hasMain = False,
  _mainMod = Nothing,

  _currFilePath = "",
  _currModName = "",
  _currMain = False,
  _currParameters = [],

  _currScope = Priv,
  _currMainFunc = False
}

putAfter :: (GOOLState -> GOOLState) -> GS a -> GS a
putAfter sf sv = do
  v <- sv
  getPutReturn sf v

getPutReturn :: (GOOLState -> GOOLState) -> a -> GS a
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

setMain :: GOOLState -> GOOLState
setMain = over hasMain (\b -> if b then error "Multiple main functions defined"
  else not b)

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

setParameters :: [ParamData] -> GOOLState -> GOOLState
setParameters = set currParameters

getParameters :: GS [ParamData]
getParameters = gets (^. currParameters)

setScope :: ScopeTag -> GOOLState -> GOOLState
setScope = set currScope

getScope :: GS ScopeTag
getScope = gets (^. currScope)

setCurrMainFunc :: Bool -> GOOLState -> GOOLState
setCurrMainFunc = set currMainFunc

getCurrMainFunc :: GS Bool
getCurrMainFunc = gets (^. currMainFunc)