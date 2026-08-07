module Drasil.System.ProjectName (
  ProjectName,
  humanReadable, repo,
  mkProjectName, mkCommonProjName
) where

import Control.Lens (makeLenses)
import Data.Char (toLower, isAlphaNum)
import Data.List.Extras (replaceAll)

import Drasil.Database (UID, HasUID(..))

data ProjectName = PN
  { _pnUID :: UID,
    _humanReadable :: String,
    _repo :: String
  }
makeLenses ''ProjectName

-- FIXME: Need to create a 'suitable name' policy. Ideas:
--
-- Human-Readable Name:
-- 1. Non-empty
-- 2. No special whitespace (\t, \r, \n, \f, \v)
-- 3. <64 chars?
--
-- Repo Name:
-- 1. Non-empty
-- 2. Valid chars: lowercase alphanumeric or '-' only
-- 3. <64 chars?

mkProjectName :: UID -> String -> String -> ProjectName
mkProjectName u hrn ans
  | all (\c -> isAlphaNum c || c == '-') ans = PN u hrn ans
  | otherwise          = error "Project repo name must be alphanumeric."

mkCommonProjName :: UID -> String -> ProjectName
mkCommonProjName u hrn = PN u hrn (frmtr hrn)
  where
    frmtr = map toLower . replaceAll (not . isAlphaNum) '-'

instance HasUID ProjectName where
  uid = pnUID
