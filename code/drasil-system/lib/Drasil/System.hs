{-# LANGUAGE GADTs, TemplateHaskell, RankNTypes #-}
-- | Define types and functions related to creating a system information database.

-- Changes to System should be reflected in the 'Creating Your Project 
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil

module Drasil.System (
  -- * System
  -- ** Types
  System(..), SystemKind(..),
  -- ** Lenses
  HasSystem(..),
  -- ** Functions
  whatsTheBigIdea, mkSystem,
  -- * Reference Database
  -- ** Types
  Purpose, Background, Scope, Motivation,

  module Drasil.SoftwareSpecifications.Requirements
) where

import Control.Lens (makeClassy)

import qualified Data.Drasil.Concepts.Documentation as Doc

import Language.Drasil hiding (kind, Notebook)
import Database.Drasil (ChunkDB)
import Drasil.Metadata (runnableSoftware, website)

import Drasil.SoftwareSpecifications.Requirements

-- | Project Example purpose.
type Purpose = [Sentence]
-- | Project Example background information, used in the 'What' section of README.
type Background = [Sentence]
-- | Project Example scope.
type Scope = [Sentence]
-- | Project Example motivation.
type Motivation = [Sentence]

data SystemKind =
    Specification RequirementsSpecification
  | RunnableSoftware
  | Notebook
  | Website

whatsTheBigIdea :: System -> IdeaDict
whatsTheBigIdea si = whatKind' (_kind si)
  where
    whatKind' :: SystemKind -> IdeaDict
    whatKind' Specification{} = nw Doc.srs
    whatKind' RunnableSoftware = runnableSoftware
    whatKind' Notebook = nw Doc.notebook
    whatKind' Website = website

-- | Data structure for holding all of the requisite information about a system
-- to be used in artifact generation.
data System where
--FIXME:
--There should be a way to remove redundant "Quantity" constraint.
-- I'm thinking for getting concepts that are also quantities, we could
-- use a lookup of some sort from their internal (Drasil) ids.
 SI :: (CommonIdea a, Idea a) => 
  { _sys          :: a
  , _kind         :: SystemKind
  , _authors      :: People
  , _purpose      :: Purpose
  , _background   :: Background
  , _scope        :: Scope
  , _motivation   :: Motivation
  , _configFiles  :: [String]
  , _systemdb     :: ChunkDB
  } -> System

makeClassy ''System

mkSystem :: (CommonIdea a, Idea a) => a -> SystemKind -> People -> Purpose ->
  Background -> Scope -> Motivation -> [String] -> ChunkDB -> System
mkSystem = SI
