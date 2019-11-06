{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GOOLState(..), headers, sources, initialState, getPutReturn, addFile, 
  addCombinedHeaderSource, addHeader, addSource
) where

import GOOL.Drasil.Data (FileType(..))

import Control.Lens (makeLenses,over)
import Control.Monad.State (State, get, put)

data GOOLState = GS {
  _headers :: [FilePath],
  _sources :: [FilePath]
} 
makeLenses ''GOOLState

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = []
}

getPutReturn :: State GOOLState b -> (GOOLState -> b -> GOOLState) -> (b -> a) 
  -> State GOOLState a
getPutReturn st sf vf = do
  v <- st
  s <- get
  put $ sf s v
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