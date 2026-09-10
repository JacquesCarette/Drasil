{-# LANGUAGE TemplateHaskell, DerivingVia #-}
-- | Project names for software projects.
module Drasil.System.ProjectName (
  ProjectName,
  mkProjectName, mkCommonProjName,
  HasProjectName(..),
  projTitleS, projAbrvS, introduceProjName
) where

import Control.Lens ((^.), makeLensesFor, makeClassyFor)
import Data.Char (toLower, isAlphaNum, isPrint)
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

-- | Construct a 'ProjectName' from a 'UID', title ('NP'), abbreviation ('String'),
-- and repository name formatter ('String -> String'). Errors if the repository
-- name contains characters other than alphanumeric characters or @'-'@.
--
-- Abbreviation rules:
-- 1. Non-empty, maximum of 64 chars.
-- 2. Only printable characters.
--
-- Repo name rules:
-- 1. Non-empty, maximum of 64 chars.
-- 2. Lowercase alphanumeric or '-' only.
mkProjectName :: UID -> NP -> String -> (String -> String) -> ProjectName
mkProjectName u title abrv abrv2repoF
  | not (all isPrint abrv)                         = error "Project abbreviation may only contain printable characters."
  | length abrv > 64 || null abrv                  = error "Project abbreviation must be between [1,64] characters long."
  | any (\c -> not (isAlphaNum c) && c /= '-') rpo = error "Project repository name must be alphanumeric."
  | length rpo > 64 || null rpo                    = error "Project repository name must be between [1,64] characters long."
  | otherwise          = PN u title abrv rpo
  where
    rpo = abrv2repoF abrv

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
