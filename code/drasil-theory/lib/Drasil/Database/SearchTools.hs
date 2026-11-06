{- This code really doesn't belong here, but it didn't belong in drasil-printers
   either! It will need to find a proper home, later. Right now, it needs to be
   'here' because it depends on things defined in this package
-}
module Drasil.Database.SearchTools (
  -- * Types
  TermAbbr,
  -- * Field Accessors
  longForm, shortForm,
  -- * Resolvers
  termResolve, termResolve',
  refResolve,
  findAllConcInstsUnder
) where

import Control.Lens ((^.))

import Drasil.Database (ChunkDB, UID, find, findAll, findOrErr)
import Language.Drasil
import Language.Drasil.Document
import Drasil.System (ProjectName, HasProjectName(..))
import Theory.Drasil.DataDefinition (DataDefinition)
import Theory.Drasil.InstanceModel (InstanceModel)
import Theory.Drasil.GenDefn (GenDefn)
import Theory.Drasil.Theory (TheoryModel)

-- This is only needed for as long as `TypedUIDRef` is underused.
data TermAbbr = TermAbbr { longForm :: NP, shortForm :: Maybe String }

-- | Search for a chunk that is an instance of 'Idea' and return its "term" and
-- abbreviation, erroring out if it doesn't exist.
termResolve :: (NP -> Maybe String -> c) -> ChunkDB -> UID -> c
termResolve f db trg
  | (Just c) <- find trg db :: Maybe IdeaDict            = go f c
  | (Just c) <- find trg db :: Maybe CI                  = go f c
  | (Just c) <- find trg db :: Maybe ProjectName         = f (c ^. projTitle) (Just $ c ^. projAbrv)
  | (Just c) <- find trg db :: Maybe DefinedQuantityDict = go f c
  | (Just c) <- find trg db :: Maybe ConceptChunk        = go f c
  | (Just c) <- find trg db :: Maybe UnitDefn            = go f c
  | (Just c) <- find trg db :: Maybe DataDefinition      = go f c
  | (Just c) <- find trg db :: Maybe InstanceModel       = go f c
  | (Just c) <- find trg db :: Maybe GenDefn             = go f c
  | (Just c) <- find trg db :: Maybe TheoryModel         = go f c
  | (Just c) <- find trg db :: Maybe ConceptInstance     = go f c
  | otherwise = error $ "Term: `" <> show trg <> "` not found in TermMap"
  where
    go :: Idea t => (NP -> Maybe String -> c) -> t -> c
    go f' c = f' (c ^. term) (getA c)

-- | Find a chunk's "term" and abbreviation, erroring out if it doesn't exist.
termResolve' :: ChunkDB -> UID -> TermAbbr
termResolve' = termResolve TermAbbr

-- | Looks up a 'UID' in a 'ChunkDB' with a /referrable/ handle.
refResolve :: ChunkDB -> UID -> Reference
refResolve db trg
  | (Just c) <- find trg db :: Maybe DataDefinition      = ref c
  | (Just c) <- find trg db :: Maybe InstanceModel       = ref c
  | (Just c) <- find trg db :: Maybe GenDefn             = ref c
  | (Just c) <- find trg db :: Maybe TheoryModel         = ref c
  | (Just c) <- find trg db :: Maybe ConceptInstance     = ref c
  | (Just c) <- find trg db :: Maybe LabelledContent     = ref c
  | (Just c) <- find trg db :: Maybe Citation            = ref c
  | (Just c) <- find trg db :: Maybe Section             = ref c
  -- FIXME: When URIs are made not-`Reference`-types, we can (should) remove the
  -- below `Just` case.
  | (Just c) <- find trg db :: Maybe Reference           = c
  | otherwise = error $ "Reference for `" <> show trg <> "` not found."

-- | Find all 'ConceptInstance's whose domain is the given one, or is nested
-- (through parent domains, which are always 'ConceptChunk's) under it.
findAllConcInstsUnder :: UID -> ChunkDB -> [ConceptInstance]
findAllConcInstsUnder dom db = filter (isUnder . sDom . cdom) $ findAll db
  where
    isUnder :: UID -> Bool
    isUnder curr
      | curr == dom = True
      -- Check parent domain: MUST be a ConceptChunk as per current design.
      | otherwise = case cdom (findOrErr curr db :: ConceptChunk) of
          [] -> False
          ds -> isUnder (sDom ds)
