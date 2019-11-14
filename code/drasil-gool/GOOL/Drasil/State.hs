{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GOOLState(..), headers, sources, hasMain, mainMod, initialState, getPutReturn,
  getPutReturnFunc, getPutReturnFunc2, getPutReturnList, passState, 
  passState2Lists, checkGOOLState, addFile, addCombinedHeaderSource, addHeader, 
  addSource, addProgNameToPaths, setMain, setMainMod, setFilePath, getFilePath
) where

import GOOL.Drasil.Data (FileType(..))

import Control.Lens (makeLenses,over,set,(^.))
import Control.Monad.State (State, get, put, gets)

data GOOLState = GS {
  _headers :: [FilePath],
  _sources :: [FilePath],
  _hasMain :: Bool,
  _mainMod :: Maybe FilePath,

  _currFilePath :: FilePath
} 
makeLenses ''GOOLState

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = [],
  _hasMain = False,
  _mainMod = Nothing,

  _currFilePath = ""
}

getPutReturn :: (GOOLState -> GOOLState) -> a -> State GOOLState a
getPutReturn sf v = do
  s <- get
  put $ sf s
  return v

getPutReturnFunc :: State GOOLState b -> (GOOLState -> b -> GOOLState) -> 
  (b -> a) -> State GOOLState a
getPutReturnFunc st sf vf = do
  v <- st
  s <- get
  put $ sf s v
  return $ vf v

getPutReturnFunc2 :: State GOOLState c -> State GOOLState b -> 
  (GOOLState -> c -> b -> GOOLState) -> (c -> b -> a) -> State GOOLState a
getPutReturnFunc2 st1 st2 sf vf = do
  v1 <- st1
  v2 <- st2
  s <- get
  put $ sf s v1 v2
  return $ vf v1 v2

getPutReturnList :: [State GOOLState b] -> (GOOLState -> GOOLState) -> 
  ([b] -> a) -> State GOOLState a
getPutReturnList l sf vf = do
  v <- sequence l
  s <- get
  put $ sf s
  return $ vf v

passState :: State GOOLState a -> State GOOLState b -> State GOOLState b
passState s v = do
  _ <- s
  v

passState2Lists :: [State GOOLState a] -> [State GOOLState b] -> 
  State GOOLState c -> State GOOLState c
passState2Lists l1 l2 v = do
  sequence_ l1
  sequence_ l2
  v 

checkGOOLState :: (GOOLState -> Bool) -> State GOOLState b -> State GOOLState a 
  -> State GOOLState a -> State GOOLState a
checkGOOLState f st ifv elsev = do
  _ <- st
  s <- get
  if f s then ifv else elsev

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

getFilePath :: State GOOLState FilePath
getFilePath = gets (^. currFilePath)