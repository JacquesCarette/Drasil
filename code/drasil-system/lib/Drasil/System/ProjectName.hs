{-# LANGUAGE TemplateHaskell, DerivingVia #-}
-- | Project names for software projects.
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

-- | General software projects generally have at least 3 names: a human-readable
-- title, a human-preferred abbreviation, and a preferred repository name
-- (slug). For example, "Drasil: A Software Generation Framework", "Drasil", and
-- "drasil".
data ProjectName = PN
  { -- | The UID.
    _pnUID        :: UID,
    -- | Human-readable project title.
    _title        :: NP,
    -- | Preferred human-readable project abbreviation (the identifier commonly
    -- used to refer to the project, likely abbreviated).
    _abbreviation :: String,
    -- | Preferred project repository name (slug).
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

-- | Construct a 'ProjectName' from a 'UID', title ('NP'), abbreviation ('String'),
-- and repository name formatter ('String -> String'). Errors if the repository
-- name contains characters other than alphanumeric characters or @'-'@.
mkProjectName :: UID -> NP -> String -> (String -> String) -> ProjectName
mkProjectName u ttl ab rpoF
  | all (\c -> isAlphaNum c || c == '-') rpo = PN u ttl ab rpo
  | otherwise          = error "Project repo name must be alphanumeric."
  where
    rpo = rpoF ab

-- | Construct a 'ProjectName' using a common repository name formatting rule
-- (lowercase, naively replace non-alphanumeric characters with @'-'@s).
mkCommonProjName :: UID -> NP -> String -> ProjectName
mkCommonProjName u ttl ab = mkProjectName u ttl ab frmtr
  where
    frmtr = map toLower . replaceAll (not . isAlphaNum) '-'

-- | Get the title of a 'ProjectName' (as a 'Sentence').
projTitleS :: ProjectName -> Sentence
projTitleS = Ch TermStyle CapW . (^. uid)

-- | Get the human-readable abbreviation of a 'ProjectName' (as a 'Sentence').
projAbrvS :: ProjectName -> Sentence
projAbrvS = Ch ShortStyle CapW . (^. uid)

-- | Combine a 'ProjectName''s title and abbreviation into an introductory
-- 'Sentence' (e.g. "Title (Abbreviation)").
introduceProjName :: ProjectName -> Sentence
introduceProjName proj = projTitleS proj +:+ sParen (projAbrvS proj)
