module Drasil.System.ProjectName (
  ProjectName, mkProjectName,
  humanReadable, repo
) where

import Control.Lens (makeLenses)

import Drasil.Database (UID, HasUID(..))

data ProjectName = PN
  { _pnUID :: UID,
    _humanReadable :: String,
    _repo :: String
  }
makeLenses ''ProjectName

mkProjectName :: UID -> String -> String -> ProjectName
mkProjectName = PN

instance HasUID ProjectName where
  uid = pnUID
