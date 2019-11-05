{-# LANGUAGE TemplateHaskell #-}

module GOOL.Drasil.State (
  GOOLState(..), combineStates, initialState, addFile, 
  addCombinedHeaderSource, addHeader, addSource
) where

import GOOL.Drasil.Data (FileType(..))

import Control.Lens ((^.),makeLenses,over)
import Data.List (nub)

data GOOLState = GS {
  _headers :: [FilePath],
  _sources :: [FilePath]
} 
makeLenses ''GOOLState

combineStates :: GOOLState -> GOOLState -> GOOLState
combineStates gs1 gs2 = GS {
  _headers = if length hdrs > length (nub hdrs) then hdrs else 
    error "Multiple modules with same name encountered", 
  _sources = if length srcs > length (nub srcs) then srcs else 
    error "Multiple modules with same name encountered"
}
  where hdrs = gs1 ^. headers ++ gs2 ^. headers
        srcs = gs1 ^. sources ++ gs2 ^. sources

initialState :: GOOLState
initialState = GS {
  _headers = [],
  _sources = []
}

addFile :: FileType -> FilePath -> GOOLState -> GOOLState
addFile Combined = addCombinedHeaderSource
addFile Source = addSource
addFile Header = addHeader

addHeader :: FilePath -> GOOLState -> GOOLState
addHeader fp = over headers (fp:)

addSource :: FilePath -> GOOLState -> GOOLState
addSource fp = over sources (fp:)

addCombinedHeaderSource :: FilePath -> GOOLState -> GOOLState
addCombinedHeaderSource fp = addSource fp . addHeader fp 