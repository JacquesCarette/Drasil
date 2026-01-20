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
  -- * Hacks
  refbyLookup, traceLookup
) where

import Control.Lens (makeClassy, (^.))
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import Drasil.Database (UID, HasUID(..), ChunkDB)
import Language.Drasil (Quantity, MayHaveUnit, Sentence, Concept,
  Reference, People, IdeaDict, CI, Constrained, ConstQDef, nw, abrv)
import Theory.Drasil (TheoryModel, GenDefn, DataDefinition, InstanceModel)
import Drasil.Metadata (runnableSoftware, website, srs, notebook)
import Utils.Drasil (toPlainName)

-- | Project Example purpose.
type Purpose = [Sentence]
-- | Project Example background information, used in the 'What' section of README.
type Background = [Sentence]
-- | Project Example scope.
type Scope = [Sentence]
-- | Project Example motivation.
type Motivation = [Sentence]

data SystemKind =
    Specification
  | RunnableSoftware
  | Notebook
  | Website

whatsTheBigIdea :: System -> IdeaDict
whatsTheBigIdea si = whatKind' (_kind si)
  where
    whatKind' :: SystemKind -> IdeaDict
    whatKind' Specification = nw srs
    whatKind' RunnableSoftware = runnableSoftware
    whatKind' Notebook = nw notebook
    whatKind' Website = website

-- | Data structure for holding all of the requisite information about a system
-- to be used in artifact generation.
data System where
--FIXME:
--There should be a way to remove redundant "Quantity" constraint.
-- I'm thinking for getting concepts that are also quantities, we could
-- use a lookup of some sort from their internal (Drasil) ids.
 SI :: (Quantity h, MayHaveUnit h, Concept h,
  Quantity i, MayHaveUnit i, Concept i,
  HasUID j, Constrained j) =>
  { _sysName      :: CI
  , _programName  :: String
  , _kind         :: SystemKind
  , _authors      :: People
  , _purpose      :: Purpose
  , _background   :: Background
  , _scope        :: Scope
  , _motivation   :: Motivation
  , _theoryModels :: [TheoryModel]
  , _genDefns     :: [GenDefn]
  , _dataDefns    :: [DataDefinition]
  , _instModels   :: [InstanceModel]
  , _configFiles  :: [String]
  , _inputs       :: [h]
  , _outputs      :: [i]
  , _constraints  :: [j] --TODO: Add SymbolMap OR enough info to gen SymbolMap
  , _constants    :: [ConstQDef]
  , _systemdb     :: ChunkDB
    -- FIXME: Hacks to be removed once 'Reference's are rebuilt.
  , _refTable     :: M.Map UID Reference
  , _refbyTable   :: M.Map UID [UID]
  , _traceTable   :: M.Map UID [UID]
  } -> System

makeClassy ''System

mkSystem :: (Quantity h, MayHaveUnit h, Concept h,
  Quantity i, MayHaveUnit i, Concept i,
  HasUID j, Constrained j) =>
  CI -> SystemKind -> People -> Purpose -> Background -> Scope -> Motivation ->
    [TheoryModel] -> [GenDefn] -> [DataDefinition] -> [InstanceModel] ->
    [String] -> [h] -> [i] -> [j] -> [ConstQDef] -> ChunkDB -> [Reference] ->
    System
mkSystem nm sk ppl prps bkgrd scp motive tms gds dds ims ss hs is js cqds db refs
  = SI nm progName sk ppl prps bkgrd scp motive tms gds dds ims ss hs is js
      cqds db refsMap mempty mempty
  where
    refsMap = M.fromList $ map (\x -> (x ^. uid, x)) refs
    progName = toPlainName $ filter (not . isSpace) $ abrv nm

refbyLookup :: UID -> System -> [UID]
refbyLookup u = fromMaybe [] . M.lookup u . (^. refbyTable)

traceLookup :: UID -> System -> [UID]
traceLookup u = fromMaybe [] . M.lookup u . (^. traceTable)
