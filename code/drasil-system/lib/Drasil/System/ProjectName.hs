{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Drasil.System.ProjectName (
  ProjectName,
  title, abbreviation, repo,
  mkProjectName, mkCommonProjName
) where

import Control.Lens ((^.), makeLenses)
import Data.Char (toLower, isAlphaNum)
import Data.List.Extras (replaceAll)

import Drasil.Database (UID, HasUID(..), declareHasChunkRefs, Generically(..))
import Language.Drasil (NP, NamedIdea(..), Idea(..), CommonIdea(..))

data ProjectName = PN
  { _pnUID        :: UID,
    -- | Human-readable project title.
    _title        :: NP,
    -- | Preferred human-readable project abbreviation (the identifier commonly
    -- used to refer to the project, likely abbreviated).
    _abbreviation :: String,
    -- | Preferred project repository name.
    _repo         :: String
  }
declareHasChunkRefs ''ProjectName
makeLenses ''ProjectName

-- FIXME: Need to create a 'suitable name' policy. Ideas:
--
-- Title: Any NP.
--
-- Abbreviation:
-- 1. Non-empty
-- 2. No special whitespace (\t, \r, \n, \f, \v)
-- 3. <64 chars?
--
-- Repo Name:
-- 1. Non-empty
-- 2. Valid chars: lowercase alphanumeric or '-' only
-- 3. <64 chars?

mkProjectName :: UID -> NP -> String -> (String -> String) -> ProjectName
mkProjectName u ttl ab rpoF
  | all (\c -> isAlphaNum c || c == '-') rpo = PN u ttl ab rpo
  | otherwise          = error "Project repo name must be alphanumeric."
  where
    rpo = rpoF ab

mkCommonProjName :: UID -> NP -> String -> ProjectName
mkCommonProjName u ttl ab = mkProjectName u ttl ab frmtr
  where
    frmtr = map toLower . replaceAll (not . isAlphaNum) '-'

instance HasUID ProjectName where
  uid = pnUID

instance NamedIdea ProjectName where
  term = title

instance Idea ProjectName where
  getA pn = Just (pn ^. abbreviation)

instance CommonIdea ProjectName where
  abrv pn = pn ^. abbreviation
