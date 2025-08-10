module Drasil.Database.SearchTools where

import Control.Lens ((^.))

import Database.Drasil
import Drasil.Database.Chunk (IsChunk)
import Language.Drasil
import Theory.Drasil

-- This is only neede for as long as `TypedUIDRef` is underused.
data TermAbbr = TermAbbr { longForm :: NP, shortForm :: Maybe String }

-- | Search for _any_ chunk that is an instance of 'Idea' and process its "term"
-- and abbreviation.
termResolve :: (NP -> Maybe String -> c) -> ChunkDB -> UID -> c
termResolve f db trg
  | (Just c) <- (find trg db :: Maybe IdeaDict)            = go f c
  | (Just c) <- (find trg db :: Maybe DefinedQuantityDict) = go f c
  | (Just c) <- (find trg db :: Maybe ConceptChunk)        = go f c
  | (Just c) <- (find trg db :: Maybe UnitDefn)            = go f c
  | (Just c) <- (find trg db :: Maybe DataDefinition)      = go f c
  | (Just c) <- (find trg db :: Maybe InstanceModel)       = go f c
  | (Just c) <- (find trg db :: Maybe GenDefn)             = go f c
  | (Just c) <- (find trg db :: Maybe TheoryModel)         = go f c
  | (Just c) <- (find trg db :: Maybe ConceptInstance)     = go f c
  | otherwise = error $ "Term: " ++ show trg ++ " not found in TermMap"
  where 
    go :: Idea t => (NP -> Maybe String -> c) -> t -> c
    go f ch = f (ch ^. term) (getA ch)

-- | Find a chunks "term" and abbreviation, if it exists.
termResolve' :: ChunkDB -> UID -> TermAbbr
termResolve' = termResolve TermAbbr
