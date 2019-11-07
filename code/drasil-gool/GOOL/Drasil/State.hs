{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GOOLState(..), headers, sources, hasMain, initialState, getPutReturn, 
  getPutReturnFunc,checkGOOLState, addFile, addCombinedHeaderSource, addHeader, 
  addSource, setMain
) where

import GOOL.Drasil.Data (FileType(..))

import Control.Lens (makeLenses,over)
import Control.Monad.State (State, get, put)

data GOOLState = GS {
  _headers :: [FilePath],
  _sources :: [FilePath],
  _hasMain :: Bool
} 
makeLenses ''GOOLState

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = [],
  _hasMain = False
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

setMain :: GOOLState -> GOOLState
setMain = over hasMain (\b -> if b then error "Multiple main functions defined" 
  else not b)