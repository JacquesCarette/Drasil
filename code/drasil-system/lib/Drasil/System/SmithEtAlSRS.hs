{-# LANGUAGE GADTs, RankNTypes #-}
-- | Define types and functions related to creating a system information database.

-- Changes to System should be reflected in the 'Creating Your Project in
-- Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil
module Drasil.System.SmithEtAlSRS (
  -- * System
  -- ** Types
  SmithEtAlSRS(..), SystemKind(..),
  Purpose, Background, Scope, Motivation,
  -- ** Lenses
  HasSmithEtAlSRS(..),
  -- ** Functions
  whatsTheBigIdea, mkSmithEtAlICO,
  -- ** Hacks
  refbyLookup, traceLookup
) where

import Control.Lens (makeClassy, (^.))
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import Drasil.Database (UID, HasUID(..), ChunkDB)
import Language.Drasil (Quantity, MayHaveUnit, Concept,
  Reference, People, IdeaDict, CI, Constrained, ConstQDef, nw, abrv)
import Theory.Drasil (TheoryModel, GenDefn, DataDefinition, InstanceModel)
import Drasil.Metadata.SupportedSoftware (runnableSoftware)
import Drasil.Metadata.Documentation (srs)
import Utils.Drasil (toPlainName)

import Drasil.System.Core

-- | Enumeration of /kinds/ of 'System's we can encode.
data SystemKind =
    Specification
  | RunnableSoftware

-- | Data structure for holding all of the requisite information about a system
-- to be used in artifact generation.
data SmithEtAlSRS where
 ICO :: (Quantity h, MayHaveUnit h, Concept h,
  Quantity i, MayHaveUnit i, Concept i,
  HasUID j, Constrained j) =>
  { _meta         :: SystemMeta
  , _kind         :: SystemKind
  , _programName  :: String
  , _theoryModels :: [TheoryModel]
  , _genDefns     :: [GenDefn]
  , _dataDefns    :: [DataDefinition]
  , _instModels   :: [InstanceModel]
  , _inputs       :: [h]
  , _outputs      :: [i]
  , _constraints  :: [j]
  , _constants    :: [ConstQDef]
  -- FIXME: Hacks to be removed once 'Reference's are rebuilt.
  , _refTable     :: M.Map UID Reference
  , _refbyTable   :: M.Map UID [UID]
  , _traceTable   :: M.Map UID [UID]
  } -> SmithEtAlSRS

makeClassy ''SmithEtAlSRS

instance HasSystemMeta SmithEtAlSRS where
  systemMeta = meta

-- | Probe what kind of 'System' one is. For example, does it represent a
-- (Problem) specification, runnable software, a 'notebook', or a website?
whatsTheBigIdea :: SmithEtAlSRS -> IdeaDict
whatsTheBigIdea = whatKind' . (^. kind)
  where
    whatKind' :: SystemKind -> IdeaDict
    whatKind' Specification = nw srs
    whatKind' RunnableSoftware = runnableSoftware

-- | Build a 'System'.
mkSmithEtAlICO :: (Quantity h, MayHaveUnit h, Concept h,
  Quantity i, MayHaveUnit i, Concept i,
  HasUID j, Constrained j) =>
  CI -> SystemKind -> People -> Purpose -> Background -> Scope -> Motivation ->
    [TheoryModel] -> [GenDefn] -> [DataDefinition] -> [InstanceModel] ->
    [h] -> [i] -> [j] -> [ConstQDef] -> ChunkDB -> [Reference] ->
    SmithEtAlSRS
mkSmithEtAlICO nm sk ppl prps bkgrd scp motive tms gds dds ims hs is js cqds db refs
  = ICO (mkSystemMeta nm ppl prps bkgrd scp motive db) sk progName tms gds dds ims hs is js
      cqds refsMap mempty mempty
  where
    refsMap = M.fromList $ map (\x -> (x ^. uid, x)) refs
    progName = toPlainName $ filter (not . isSpace) $ abrv nm

-- | Find what chunks reference a specific chunk.
refbyLookup :: UID -> SmithEtAlSRS -> [UID]
refbyLookup u = fromMaybe [] . M.lookup u . (^. refbyTable)

-- | Find what chunks a specific one references.
traceLookup :: UID -> SmithEtAlSRS -> [UID]
traceLookup u = fromMaybe [] . M.lookup u . (^. traceTable)
