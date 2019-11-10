{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GOOLState(..), headers, sources, hasMain, mainMod, initialState, getPutReturn,
  getPutReturnFunc, getPutReturnList, passState, passState2Lists, 
  checkGOOLState, addFile, addCombinedHeaderSource, addHeader, addSource, 
  addProgNameToPaths, setMain, setMainMod
) where

import GOOL.Drasil.Symantics (Label)
import GOOL.Drasil.Data (FileType(..))

import Control.Lens (makeLenses,over,set)
import Control.Monad.State (State, get, put)

data GOOLState = GS {
  _headers :: [FilePath],
  _sources :: [FilePath],
  _hasMain :: Bool,
  _mainMod :: Maybe FilePath
} 
makeLenses ''GOOLState

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = [],
  _hasMain = False,
  _mainMod = Nothing
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

addProgNameToPaths :: Label -> GOOLState -> GOOLState
addProgNameToPaths n = over mainMod (fmap f) . over sources (map f) . 
  over headers (map f)
  where f = ((n++"/")++)

setMain :: GOOLState -> GOOLState
setMain = over hasMain (\b -> if b then error "Multiple main functions defined" 
  else not b)

setMainMod :: String -> GOOLState -> GOOLState
setMainMod n = set mainMod (Just n)