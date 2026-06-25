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

import Drasil.Database (UID, HasUID(..), ChunkDB, insert, showUID)
import Language.Drasil
import Language.Drasil.Document (LabelledContent, Reference, refS)
import Theory.Drasil (TheoryModel, GenDefn, DataDefinition, InstanceModel, HasOutput (..))
import Data.String.Extras (toPlainName)

import Drasil.System.Core (SystemMeta, Background, HasSystemMeta(..),
  mkSystemMeta, Motivation, Purpose, Scope)
import Data.Drasil.Concepts.Documentation (funcReqDom)

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
  = ICO (mkSystemMeta nm ppl prps bkgrd scp motive db') progName tms gds dds ims hs is js
      cqds qs lcs refsMap mempty mempty
  where
    refsMap = M.fromList $ map (\x -> (x ^. uid, x)) refs
    progName = toPlainName $ filter (not . isSpace) $ abrv nm

    -- Create a map of var -> sentences containing references to the IM/DD
    -- defining the var
    varSrcRef model = let out = model ^. output in (out ^. uid, refS model)
    outMap = M.fromList $ map varSrcRef dds ++ map varSrcRef ims -- FIXME: What do if an IM and a DD have the same output?

    -- Calculate where the outputs came from, formatted as strings
    outSrcRef q = let src = fromMaybe (error $ "no solution for " ++ showUID q) $ M.lookup (q ^. uid) outMap
                   in ch q +:+ S "(from" +:+ src :+: S ")"
    outSrcs = outSrcRef <$> is

    -- Create the output FR
    outFR = cic "outputValues2" desc "Output-Values-2" funcReqDom
    desc = foldlSent [S "Output", foldlList Comma List $ NE.toList outSrcs]

    -- Insert the output FR into the db
    db' = insert outFR db

-- | Find what chunks reference a specific chunk.
refbyLookup :: UID -> SmithEtAlSRS -> [UID]
refbyLookup u = fromMaybe [] . M.lookup u . (^. refbyTable)

-- | Find what chunks a specific one references.
traceLookup :: UID -> SmithEtAlSRS -> [UID]
traceLookup u = fromMaybe [] . M.lookup u . (^. traceTable)
