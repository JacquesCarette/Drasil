{-# LANGUAGE GADTs, TemplateHaskell, RankNTypes #-}
-- | Define types and functions related to creating a system information database.

-- Changes to System should be reflected in the 'Creating Your Project 
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil

module System.Drasil.System (
  -- * System
  -- ** Types
  System(..), SystemKind(..),
  -- ** Lenses
  HasSystem(..),
  -- ** Functions
  whatsTheBigIdea,
  -- * Reference Database
  -- ** Types
  Purpose, Background, Scope, Motivation,
  ) where

import Language.Drasil hiding (kind, Notebook)
import Theory.Drasil
import Database.Drasil (ChunkDB)

import Control.Lens (makeClassy)
import qualified Data.Drasil.Concepts.Documentation as Doc

-- | Project Example purpose.
type Purpose = [Sentence]
-- | Project Example background information, used in the 'What' section of README.
type Background = [Sentence]
-- | Project Example scope.
type Scope = [Sentence]
-- | Project Example motivation.
type Motivation = [Sentence]

data SystemKind =
    SRS
  | Notebook
  | Website

whatsTheBigIdea :: System -> IdeaDict
whatsTheBigIdea si = whatKind' (_kind si)
  where
    whatKind' :: SystemKind -> IdeaDict
    whatKind' SRS = nw Doc.srs
    whatKind' Notebook = nw Doc.notebook
    whatKind' Website = mkIdea "website" (cn "website") (Just "web")

-- | Data structure for holding all of the requisite information about a system
-- to be used in artifact generation.
data System where
--FIXME:
--There should be a way to remove redundant "Quantity" constraint.
-- I'm thinking for getting concepts that are also quantities, we could
-- use a lookup of some sort from their internal (Drasil) ids.
 SI :: (CommonIdea a, Idea a,
  Quantity e, Eq e, MayHaveUnit e,
  Quantity h, MayHaveUnit h,
  Quantity i, MayHaveUnit i,
  HasUID j, Constrained j) => 
  { _sys         :: a
  , _kind        :: SystemKind
  , _authors     :: People
  , _purpose     :: Purpose
  , _background  :: Background
  , _scope       :: Scope
  , _motivation  :: Motivation
  , _quants      :: [e]
  , _instModels  :: [InstanceModel]
  , _datadefs    :: [DataDefinition]
  , _configFiles :: [String]
  , _inputs      :: [h]
  , _outputs     :: [i]
  , _constraints :: [j] --TODO: Add SymbolMap OR enough info to gen SymbolMap
  , _constants   :: [ConstQDef]
  , _systemdb   :: ChunkDB
  , _usedinfodb  :: ChunkDB
  } -> System

makeClassy ''System
