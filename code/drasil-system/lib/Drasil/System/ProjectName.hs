{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Drasil.System.ProjectName (
  ProjectName,
  mkProjectName, mkCommonProjName,
  HasProjectName(..),
  projTitleS, projAbrvS, introduceProjName
) where

import Control.Lens ((^.), makeLensesFor, makeClassyFor)
import Data.Char (toLower, isAlphaNum)
import Data.List.Extras (replaceAll)

import Drasil.Database (UID, HasUID(..), declareHasChunkRefs, Generically(..))
import Language.Drasil (NP, Sentence(Ch), SentenceStyle(..), TermCapitalization(CapW), sParen, (+:+))

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
makeLensesFor [("_pnUID", "pnUID")] ''ProjectName
makeClassyFor "HasProjectName" "projectName" [("_title", "projTitle"), ("_abbreviation", "projAbrv"), ("_repo", "projRepoName")] ''ProjectName

instance HasUID ProjectName where
  uid = pnUID

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

projTitleS :: ProjectName -> Sentence
projTitleS = Ch TermStyle CapW . (^. uid)

projAbrvS :: ProjectName -> Sentence
projAbrvS = Ch ShortStyle CapW . (^. uid)

introduceProjName :: ProjectName -> Sentence
introduceProjName proj = projTitleS proj +:+ sParen (projAbrvS proj)
