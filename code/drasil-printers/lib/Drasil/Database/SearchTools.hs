module Drasil.Database.SearchTools where

import Control.Lens ((^.))

import Database.Drasil
import Language.Drasil
import Theory.Drasil
import Debug.Trace (trace)

-- This is only needed for as long as `TypedUIDRef` is underused.
data TermAbbr = TermAbbr { longForm :: NP, shortForm :: Maybe String }

-- | Search for a chunk that is an instance of 'Idea' and return its "term" and
-- abbreviation, erroring out if it doesn't exist.
termResolve :: (NP -> Maybe String -> c) -> ChunkDB -> UID -> c
termResolve f db trg
  | (Just c) <- find trg db :: Maybe IdeaDict            = go f c -- trace (show trg ++ " TermAbbr :: IdeaDict") $ go f c
  | (Just c) <- find trg db :: Maybe DefinedQuantityDict = go f c -- trace (show trg ++ " TermAbbr :: DQD" ) $ go f c
  | (Just c) <- find trg db :: Maybe ConceptChunk        = go f c -- trace (show trg ++ " TermAbbr :: CC") $ go f c
  | (Just c) <- find trg db :: Maybe UnitDefn            = go f c -- trace (show trg ++ " TermAbbr :: UD") $ go f c
  | (Just c) <- find trg db :: Maybe DataDefinition      = go f c -- trace (show trg ++ " TermAbbr :: DD") $ go f c
  | (Just c) <- find trg db :: Maybe InstanceModel       = go f c -- trace (show trg ++ " TermAbbr :: IM") $ go f c
  | (Just c) <- find trg db :: Maybe GenDefn             = go f c -- trace (show trg ++ " TermAbbr :: GD") $ go f c
  | (Just c) <- find trg db :: Maybe TheoryModel         = go f c -- trace (show trg ++ " TermAbbr :: TM") $ go f c
  | (Just c) <- find trg db :: Maybe ConceptInstance     = go f c -- trace (show trg ++ " TermAbbr :: CI") $ go f c
  | otherwise = error $ "Term: " ++ show trg ++ " not found in TermMap"
  where
    go :: Idea t => (NP -> Maybe String -> c) -> t -> c
    go f' c = f' (c ^. term) (getA c)

-- | Find a chunk's "term" and abbreviation, erroring out if it doesn't exist.
termResolve' :: ChunkDB -> UID -> TermAbbr
termResolve' = termResolve TermAbbr

data DomDefn = DomDefn { domain :: [UID], definition :: Sentence }

-- | Looks up a 'UID' in all tables with concepts from the 'ChunkDB'. If nothing
-- is found, an error is thrown.
defResolve :: ([UID] -> Sentence -> c) -> ChunkDB -> UID -> c
defResolve f db trg
  | (Just c) <- find trg db :: Maybe DefinedQuantityDict = go f c
  | (Just c) <- find trg db :: Maybe ConceptChunk        = go f c
  | (Just c) <- find trg db :: Maybe UnitDefn            = go f c
  | (Just c) <- find trg db :: Maybe InstanceModel       = go f c
  | (Just c) <- find trg db :: Maybe GenDefn             = go f c
  | (Just c) <- find trg db :: Maybe TheoryModel         = go f c
  | (Just c) <- find trg db :: Maybe ConceptInstance     = go f c
  | otherwise = error $ "Definition: " ++ show trg ++ " not found in ConceptMap"
  where
    go :: Concept c => ([UID] -> Sentence -> r) -> c -> r
    go f' c = f' (cdom c) (c ^. defn)

defResolve' :: ChunkDB -> UID -> DomDefn
defResolve' = defResolve DomDefn

findAllDataDefns :: ChunkDB -> [DataDefinition]
findAllDataDefns = findAll

findAllGenDefns :: ChunkDB -> [GenDefn]
findAllGenDefns = findAll

findAllInstMods :: ChunkDB -> [InstanceModel]
findAllInstMods = findAll

findAllTheoryMods :: ChunkDB -> [TheoryModel]
findAllTheoryMods = findAll

findAllConcInsts :: ChunkDB -> [ConceptInstance]
findAllConcInsts = findAll

findAllIdeaDicts :: ChunkDB -> [IdeaDict]
findAllIdeaDicts = findAll

findAllDefinedQuantities :: ChunkDB -> [DefinedQuantityDict]
findAllDefinedQuantities = findAll

findAllCitations :: ChunkDB -> [Citation]
findAllCitations = findAll

findAllLabelledContent :: ChunkDB -> [LabelledContent]
findAllLabelledContent = findAll
