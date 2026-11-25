{- This code really doesn't belong here, but it didn't belong in drasil-printers
   either! It will need to find a proper home, later. Right now, it needs to be
   'here' because it depends on things defined in this package
-}
module Drasil.Database.SearchTools (
  -- * Types
  TermAbbr(..),
  -- * Resolvers
  termResolve, termResolve', termResolveNP,
  findAllConcInstsUnder
) where

import Control.Lens ((^.))
import Data.Typeable (Proxy(Proxy), typeRep)
import Debug.Trace (trace)

import Drasil.Database (ChunkDB, UID, UIDRef, IsChunk, find, findAll,
  findOrErr, findTypeOf, raw, unhide)
import Language.Drasil
import Language.Drasil.Document hiding (raw)
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
  | otherwise = error $ "Term: `" <> show trg <> "` not found in TermMap"
  where
    go :: Idea t => (NP -> Maybe String -> c) -> t -> c
    go f' c = f' (c ^. term) (getA c)

resolveOrFallback
  :: forall t a. IsChunk t
  => String
  -> ChunkDB
  -> UIDRef t
  -> (t -> a)
  -> (UID -> a)
  -> a
resolveOrFallback fn db uref ok fallback = case unhide uref db of
  Just c  -> ok c
  Nothing ->
    let u = raw uref
    in trace
      (fn <> ": `" <> show u <> "` expected `" <> show (typeRep (Proxy @t))
        <> "`, found `" <> maybe "nothing" show (findTypeOf u db) <> "`")
      (fallback u)

-- | Find a chunk's "term" and abbreviation, first trying to resolve it as the
-- expected chunk type @t@, falling back to the 'termResolve' lookup with a
-- trace message if that fails, and erroring out if it doesn't exist.
termResolve' :: Idea t => ChunkDB -> UIDRef t -> TermAbbr
termResolve' db uref =
  resolveOrFallback "termResolve'" db uref
    (\c -> TermAbbr (c ^. term) (getA c))
    (termResolve TermAbbr db)

-- | Find a chunk's "term" ('NP'), first trying to resolve it as the expected
-- chunk type @t@, falling back to the 'termResolve' lookup with a trace message
-- if that fails, and erroring out if it doesn't exist.
termResolveNP :: NamedIdea t => ChunkDB -> UIDRef t -> NP
termResolveNP db uref =
  resolveOrFallback "termResolveNP" db uref
    (^. term)
    (termResolve const db)

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
