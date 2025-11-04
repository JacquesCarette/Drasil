{-# LANGUAGE PostfixOperators #-}
-- | Defines functions to create traceability matrices in SRS documents.
module Drasil.DocumentLanguage.TraceabilityMatrix where

import Control.Lens ((^.))
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map as Map

import Drasil.Database.SearchTools (defResolve', findAllConcInsts, DomDefn(domain))
import Language.Drasil
import Database.Drasil (traceLookup, ChunkDB(traceTable, refbyTable))
import Drasil.System hiding (purpose)
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Drasil.Concepts.Documentation (purpose, component, dependency,
  item, reference, traceyMatrix)
import Drasil.DocumentLanguage.Definitions (helpToRefField)

-- * Types

-- | Helper type that takes two sets of 'UID's and a 'ChunkDB'.
type TraceViewCat = [UID] -> ChunkDB -> [UID]

-- * Main Functions

-- | Generalized traceability matrix introduction: appends references to the traceability matrices in 'Sentence' form
-- and wraps in 'Contents'. Usually references the four tables generally found in this section (in order of being mentioned).
traceMIntro :: [LabelledContent] -> [Sentence] -> Contents
traceMIntro refs trailings = UlC $ ulcc $ Paragraph $ foldlSent [phrase purpose
        `S.the_ofTheC` plural traceyMatrix, S "is to provide easy", plural reference,
        S "on what has to be additionally modified if a certain",
        phrase component, S "is changed. Every time a", phrase component,
        S "is changed, the", plural item, S "in the column of that",
        phrase component, S "that are marked with an", Quote (S "X"),
        S "should be modified as well"] +:+ foldlSent_ (zipWith tableShows refs trailings)

-- | Generates a traceability table. Takes a 'UID' for the table, a description ('Sentence'), columns ('TraceViewCat'), rows ('TraceViewCat'), and 'System'.
generateTraceTableView :: UID -> Sentence -> [TraceViewCat] -> [TraceViewCat] -> System -> LabelledContent
generateTraceTableView u desc cols rows c = llcc (makeTabRef' u) $ Table
  (EmptyS : traceMColHeader colf c)
  (makeTMatrix (traceMRowHeader rowf c) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb)
  (showingCxnBw traceyMatrix desc) True
    where
    cdb = _systemdb c
    colf = layoutUIDs cols cdb
    rowf = layoutUIDs rows cdb

-- * Helper Functions

-- | Helper that finds the traceability matrix references (things being referenced).
traceMReferees :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferees f = f . nubOrd . Map.keys . refbyTable

-- | Helper that finds the traceability matrix references (things that are referring to other things).
traceMReferrers :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferrers f = f . nubOrd . concat . Map.elems . refbyTable

-- | Helper that finds the header of a traceability matrix.
traceMHeader :: (ChunkDB -> [UID]) -> System -> [Sentence]
traceMHeader f c = map (`helpToRefField` (c ^. systemdb)) $ f $ _systemdb c

-- | Helper that finds the headers of the traceability matrix columns.
traceMColHeader :: ([UID] -> [UID]) -> System -> [Sentence]
traceMColHeader f = traceMHeader (traceMReferees f)

-- | Helper that finds the headers of the traceability matrix rows.
traceMRowHeader :: ([UID] -> [UID]) -> System -> [Sentence]
traceMRowHeader f = traceMHeader (traceMReferrers f)

-- | Helper that makes the columns of a traceability matrix.
traceMColumns :: ([UID] -> [UID]) -> ([UID] -> [UID]) -> ChunkDB -> [[UID]]
traceMColumns fc fr c = map ((\u -> filter (`elem` u) $ fc u) . flip traceLookup (traceTable c)) $ traceMReferrers fr c

-- | Helper that makes references of the form "@reference@ shows the dependencies of @something@".
tableShows :: (Referable a, HasShortName a) => a -> Sentence -> Sentence
tableShows r end = refS r +:+ S "shows the" +:+ plural dependency `S.ofThe` (end !.)

-- | Helper that finds the layout 'UID's of a traceability matrix.
layoutUIDs :: [TraceViewCat] -> ChunkDB -> [UID] -> [UID]
layoutUIDs a c e = concatMap (filter (`elem` (Map.keys $ traceTable c)) . (\ x -> x e c)) a

-- | Helper that filters a traceability matrix given a predicate and a 'ChunkDB' lens field.
traceViewFilt :: HasUID a => (a -> Bool) -> (ChunkDB -> [a]) -> TraceViewCat
traceViewFilt f table _ = map (^. uid) . filter f . table

-- | Helper that is similar to 'traceViewFilt', but the filter is always 'True'.
traceView :: HasUID a => (ChunkDB -> [a]) -> TraceViewCat
traceView = traceViewFilt (const True)

-- | Turns a 'Concept' into a 'TraceViewCat' via its domain.
traceViewCC :: Concept c => c -> TraceViewCat
traceViewCC dom u c = traceViewFilt (isDomUnder (dom ^. uid) . sDom . cdom) findAllConcInsts u c
  where
    isDomUnder :: UID -> UID -> Bool
    isDomUnder filtDom curr
      | filtDom == curr = True
      | not $ null $ getDom curr = isDomUnder filtDom (sDom $ getDom curr)
      | otherwise = False
    getDom :: UID -> [UID]
    getDom curr = domain $ defResolve' c curr
