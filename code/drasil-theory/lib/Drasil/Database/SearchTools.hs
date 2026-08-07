{- This code really doesn't belong here, but it didn't belong in drasil-printers
   either! It will need to find a proper home, later. Right now, it needs to be
   'here' because it depends on things defined in this package
-}
module Drasil.Database.SearchTools (
  -- * Types
  TermAbbr, DomDefn,
  -- * Field Accessors
  longForm, shortForm, domain, definition,
  -- * Resolvers
  termResolve, termResolve',
  defResolve, defResolve',
  refResolve,
  findAllConcInsts
) where

import Control.Lens ((^.))

import Drasil.Database (ChunkDB, UID, find, findAll)
import Language.Drasil
import Language.Drasil.Document
import Drasil.System (ProjectName)
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
  | (Just c) <- find trg db :: Maybe ProjectName         = go f c
  | (Just c) <- find trg db :: Maybe DefinedQuantityDict = go f c
  | (Just c) <- find trg db :: Maybe ConceptChunk        = go f c
  | (Just c) <- find trg db :: Maybe UnitDefn            = go f c
  | (Just c) <- find trg db :: Maybe DataDefinition      = go f c
  | (Just c) <- find trg db :: Maybe InstanceModel       = go f c
  | (Just c) <- find trg db :: Maybe GenDefn             = go f c
  | (Just c) <- find trg db :: Maybe TheoryModel         = go f c
  | (Just c) <- find trg db :: Maybe ConceptInstance     = go f c
  | otherwise = error $ "Term: `" ++ show trg ++ "` not found in TermMap"
  where
    go :: Idea t => (NP -> Maybe String -> c) -> t -> c
    go f' c = f' (c ^. term) (getA c)

-- | Find a chunk's "term" and abbreviation, erroring out if it doesn't exist.
termResolve' :: ChunkDB -> UID -> TermAbbr
termResolve' = termResolve TermAbbr

data DomDefn = DomDefn { domain :: [UID], definition :: Sentence }

-- | Looks up a 'UID' in the 'ChunkDB' for a chunk's definition concept domain
-- and definition. If nothing is found, an error is thrown.
defResolve :: ([UID] -> Sentence -> c) -> ChunkDB -> UID -> c
defResolve f db trg
  | (Just c) <- find trg db :: Maybe ConceptChunk        = go f c
  | (Just c) <- find trg db :: Maybe ConceptInstance     = go f c
  | otherwise = error $ "Definition: `" ++ show trg ++ "` not found in ConceptMap"
  where
    go :: (Concept c, ConceptDomain c) => ([UID] -> Sentence -> r) -> c -> r
    go f' c = f' (cdom c) (c ^. defn)

defResolve' :: ChunkDB -> UID -> DomDefn
defResolve' = defResolve DomDefn

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
  | otherwise = error $ "Reference for `" ++ show trg ++ "` not found."

findAllConcInsts :: ChunkDB -> [ConceptInstance]
findAllConcInsts = findAll
