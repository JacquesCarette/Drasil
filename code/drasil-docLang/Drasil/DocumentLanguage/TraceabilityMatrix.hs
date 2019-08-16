module Drasil.DocumentLanguage.TraceabilityMatrix where

import Language.Drasil
import Database.Drasil (ChunkDB, SystemInformation, UMap, _sysinfodb,
  asOrderedList, asOrderedListCC, citeDB, conceptinsLookup, conceptinsTable,
  datadefnLookup, dataDefnTable, defResolve, gendefLookup, gendefTable, insmodelLookup,
  insmodelTable, labelledconLookup, labelledcontentTable, refbyTable, sectionLookup,
  sectionTable, theoryModelLookup, theoryModelTable, traceTable, traceLookup)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (purpose, component, dependency,
  item, reference, traceyGraph, traceyMatrix)
import Data.Drasil.Concepts.Math (graph)

import qualified Drasil.DocLang.SRS as SRS

import Control.Lens ((^.), Getting)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type TraceViewCat = [UID] -> ChunkDB -> [UID]

-- wrapper for traceMGIntro
traceMGF :: [LabelledContent] -> [Sentence] -> [Contents] -> [Section] -> Section
traceMGF refs trailing otherContents = SRS.traceyMandG (traceMIntro refs trailing : otherContents)

-- generalized traceability matrix and graph introduction: variables are references to the three tables
-- generally found in this section (in order of being mentioned)
traceMIntro :: [LabelledContent] -> [Sentence] -> Contents
traceMIntro refs trailings = UlC $ ulcc $ Paragraph $ foldlSent [phrase purpose
        `ofThe'` plural traceyMatrix, S "is to provide easy", plural reference, 
        S "on what has to be additionally modified if a certain",
        phrase component, S "is changed. Every time a", phrase component, 
        S "is changed, the", plural item, S "in the column of that", 
        phrase component, S "that are marked with an", Quote (S "X"), 
        S "should be modified as well"] +:+ foldlSent (zipWith tableShows refs trailings)

-- generalized traceability matrix and graph introduction: variables are references to the three tables
-- generally found in this section (in order of being mentioned)
traceGIntro :: [LabelledContent] -> [Sentence] -> [UnlabelledContent]
traceGIntro refs trailings = map ulcc [Paragraph $ foldlSent
        [phrase purpose `ofThe'` plural traceyGraph,
        S "is also to provide easy", plural reference, S "on what has to be",
        S "additionally modified if a certain", phrase component +:+. S "is changed", 
        S "The arrows in the", plural graph, S "represent" +:+. plural dependency,
        S "The", phrase component, S "at the tail of an arrow is depended on by the",
        phrase component, S "at the head of that arrow. Therefore, if a", phrase component,
        S "is changed, the", plural component, S "that it points to should also be changed"] +:+
        foldlSent (zipWith tableShows refs trailings)]
 
traceMReferees :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferees f = f . nub . Map.keys . (^. refbyTable)

traceMReferrers :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferrers f = f . nub . concat . Map.elems . (^. refbyTable)

traceMHeader :: (ChunkDB -> [UID]) -> SystemInformation -> [Sentence]
traceMHeader f c = map (`markerHelper` c) l
  where l = f $ _sysinfodb c

markerHelper :: UID -> SystemInformation -> Sentence
markerHelper t si
  | t `elem` Map.keys (s ^. dataDefnTable)        = shortRef abrv $ datadefnLookup    t (s ^. dataDefnTable)
  | t `elem` Map.keys (s ^. insmodelTable)        = shortRef abrv $ insmodelLookup    t (s ^. insmodelTable)
  | t `elem` Map.keys (s ^. gendefTable)          = shortRef abrv $ gendefLookup      t (s ^. gendefTable)
  | t `elem` Map.keys (s ^. theoryModelTable)     = shortRef abrv $ theoryModelLookup t (s ^. theoryModelTable)
  | t `elem` Map.keys (s ^. conceptinsTable)      = shortRef conA $ conceptinsLookup  t (s ^. conceptinsTable)
  | t `elem` Map.keys (s ^. sectionTable)         = makeRef2S $ sectionLookup     t (s ^. sectionTable)
  | t `elem` Map.keys (s ^. labelledcontentTable) = makeRef2S $ labelledconLookup t (s ^. labelledcontentTable)
  | t `elem` map (^. uid) (citeDB si) = EmptyS
  | otherwise = error $ t ++ "Caught."
  where
    s = _sysinfodb si
    conA = \x -> fromMaybe (error $ "No abbreviation found for " ++ x ^. uid) (getA x)
 
traceMColHeader :: ([UID] -> [UID]) -> SystemInformation -> [Sentence]
traceMColHeader f = traceMHeader (traceMReferees f)

traceMRowHeader :: ([UID] -> [UID]) -> SystemInformation -> [Sentence]
traceMRowHeader f = traceMHeader (traceMReferrers f)

traceMColumns :: ([UID] -> [UID]) -> ([UID] -> [UID]) -> ChunkDB -> [[UID]]
traceMColumns fc fr c = map ((\u -> filter (`elem` u) $ fc u) . flip traceLookup (c ^. traceTable)) $ traceMReferrers fr c

tableShows :: LabelledContent -> Sentence -> Sentence
tableShows ref end = makeRef2S ref +:+ S "shows the" +:+ plural dependency `sOf` end

generateTraceTableView :: UID -> Sentence -> [TraceViewCat] -> [TraceViewCat] -> SystemInformation -> LabelledContent
generateTraceTableView u _ [] _ _ = error $ "Expected non-empty list of column-view categories for traceability matrix " ++ u
generateTraceTableView u _ _ [] _ = error $ "Expected non-empty list of row-view categories for traceability matrix " ++ u
generateTraceTableView u desc cols rows c = llcc (makeTabRef u) $ Table
  (EmptyS : ensureItems u (traceMColHeader colf c))
  (makeTMatrix (ensureItems u $ traceMRowHeader rowf c) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb)
  (showingCxnBw traceyMatrix desc) True where
    cdb = _sysinfodb c
    colf = layoutUIDs cols cdb
    rowf = layoutUIDs rows cdb

ensureItems :: UID -> [a] -> [a]
ensureItems u [] = error $ "Expected non-empty matrix dimension for traceability matrix " ++ u
ensureItems _ l = l

layoutUIDs :: [TraceViewCat] -> ChunkDB -> [UID] -> [UID]
layoutUIDs a c e = filter (`elem` (Map.keys $ c ^. traceTable)) $ concatMap (\x -> x e c) a

traceViewFilt :: HasUID a => (a -> Bool) -> Getting (UMap a) ChunkDB (UMap a) -> TraceViewCat
traceViewFilt f table _ = map (^. uid) . filter f . asOrderedList . (^. table)

traceView :: HasUID a => Getting (UMap a) ChunkDB (UMap a) -> TraceViewCat
traceView = traceViewFilt (const True)

traceViewFiltCC :: (ConceptInstance -> Bool) ->
  Getting (UMap ConceptInstance) ChunkDB (UMap ConceptInstance) -> TraceViewCat
traceViewFiltCC f table _ = map (^. uid) . filter f . asOrderedListCC . (^. table)

traceViewCC :: Concept c => c -> TraceViewCat
traceViewCC dom u c = traceViewFiltCC (isDomUnder (dom ^. uid) . sDom . cdom) conceptinsTable u c
  where
    isDomUnder :: UID -> UID -> Bool
    isDomUnder filtDom curr
      | filtDom == curr = True
      | not $ null $ getDom curr = isDomUnder filtDom (sDom $ getDom curr)
      | otherwise = False
    getDom :: UID -> [UID]
    getDom curr = cdom $ defResolve c curr
