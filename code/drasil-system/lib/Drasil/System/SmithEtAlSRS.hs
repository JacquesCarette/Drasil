{-# LANGUAGE GADTs, RankNTypes #-}
-- | Define types and functions related to creating a system information database.

-- Changes to System should be reflected in the 'Creating Your Project in
-- Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil
module Drasil.System.SmithEtAlSRS (
  -- * System
  -- ** Types
  SmithEtAlSRS(..),
  Purpose, Background, Scope, Motivation,
  -- ** Lenses
  HasSmithEtAlSRS(..),
  -- ** Constructors
  mkSmithEtAlICO,
  -- ** Hacks
  refbyLookup, traceLookup
) where

import Control.Lens (makeClassy, (^.))
import Data.Char (isSpace)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import Drasil.Database (UID, HasUID(..), ChunkDB)
import Language.Drasil (Quantity, MayHaveUnit, Concept, Reference, People, CI,
  Constrained, ConstQDef, abrv, DefinedQuantityDict)
import Language.Drasil.Docs (LabelledContent)
import Theory.Drasil (TheoryModel, GenDefn, DataDefinition, InstanceModel)
import Utils.Drasil (toPlainName)

import Drasil.System.Core (SystemMeta, Background, HasSystemMeta(..),
  mkSystemMeta, Motivation, Purpose, Scope)

-- | Data structure for holding all of the requisite information about a system
-- to be used in artifact generation.
data SmithEtAlSRS where
 ICO :: (Quantity h, MayHaveUnit h, Concept h,
  Quantity i, MayHaveUnit i, Concept i,
  HasUID j, Constrained j) =>
  { _meta         :: SystemMeta
  , _programName  :: String
  , _theoryModels :: [TheoryModel]
  , _genDefns     :: [GenDefn]
  , _dataDefns    :: [DataDefinition]
  , _instModels   :: [InstanceModel]
  , _inputs       :: NE.NonEmpty h
  , _outputs      :: NE.NonEmpty i
  , _constraints  :: [j]
  , _constants    :: [ConstQDef]
  -- FIXME: This is a list of all 'quantites' (variables) used/referenced in an
  -- SRS. Why is this here? For type-checking the SRS later. Should
  -- type-checking be done on the SRS level? No. This is a temporary hack.
  , _quantities   :: [DefinedQuantityDict]
  -- FIXME: This is a list of all labelled content required for the SRS to be
  -- generated. In particular, this is needed for the mdBook generator which
  -- _must_ export a CSV containing a list of all external resources that the
  -- mdBook compiler is allowed to access. This list should be re-written as
  -- part of a stateful renderer for the SRS instead.
  , _lbldCntnt    :: [LabelledContent]
  -- FIXME: Hacks to be removed once 'Reference's are rebuilt.
  , _refTable     :: M.Map UID Reference
  , _refbyTable   :: M.Map UID [UID]
  , _traceTable   :: M.Map UID [UID]
  } -> SmithEtAlSRS

makeClassy ''SmithEtAlSRS

instance HasSystemMeta SmithEtAlSRS where
  systemMeta = meta

-- | Build a 'System'.
mkSmithEtAlICO :: (Quantity h, MayHaveUnit h, Concept h,
  Quantity i, MayHaveUnit i, Concept i,
  HasUID j, Constrained j) =>
  CI -> People -> Purpose -> Background -> Scope -> Motivation ->
    [TheoryModel] -> [GenDefn] -> [DataDefinition] -> [InstanceModel] ->
    NE.NonEmpty h -> NE.NonEmpty i -> [j] -> [ConstQDef] -> [DefinedQuantityDict] ->
    [LabelledContent] -> ChunkDB -> [Reference] -> SmithEtAlSRS
mkSmithEtAlICO nm ppl prps bkgrd scp motive tms gds dds ims hs is js cqds qs lcs db refs
  = ICO (mkSystemMeta nm ppl prps bkgrd scp motive db) progName tms gds dds ims hs is js
      cqds qs lcs refsMap mempty mempty
  where
    refsMap = M.fromList $ map (\x -> (x ^. uid, x)) refs
    progName = toPlainName $ filter (not . isSpace) $ abrv nm

-- | Find what chunks reference a specific chunk.
refbyLookup :: UID -> SmithEtAlSRS -> [UID]
refbyLookup u = fromMaybe [] . M.lookup u . (^. refbyTable)

-- | Find what chunks a specific one references.
traceLookup :: UID -> SmithEtAlSRS -> [UID]
traceLookup u = fromMaybe [] . M.lookup u . (^. traceTable)
