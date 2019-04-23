module Drasil.Sections.TraceabilityMandGs
  ( traceMGF,
    traceGIntro,
    generateTraceTable
   ) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (purpose, component, dependency, item,
 reference, section_, traceyGraph, traceyMatrix)
import Data.Drasil.Concepts.Math ( graph)
import Data.Drasil.SentenceStructures (ofThe', foldlSent, showingCxnBw,
  tableShows)
import Data.Drasil.Utils (makeTMatrix)

import Drasil.DocumentLanguage.Definitions (helpToRefField)
import qualified Drasil.DocLang.SRS as SRS

import Control.Lens ((^.))
import Data.List (nub)
import qualified Data.Map as Map

-- wrapper for traceMGIntro
traceMGF :: [LabelledContent] -> [Sentence] -> [Contents] -> [Section] -> Section
traceMGF refs trailing otherContents subSec = SRS.traceyMandG ((traceMIntro refs trailing):otherContents) subSec

-- generalized traceability matrix and graph introduction: variables are references to the three tables
-- generally found in this section (in order of being mentioned)
traceMIntro :: [LabelledContent] -> [Sentence] -> Contents
traceMIntro refs trailings = UlC $ ulcc $ Paragraph $ foldlSent [(phrase purpose)
        `ofThe'` (plural traceyMatrix), S "is to provide easy", plural reference, 
        S "on what has to be additionally modified if a certain",
        phrase component, S "is changed. Every time a", phrase component, 
        S "is changed, the", plural item, S "in the row of that", 
        phrase component, S "that are marked with an", Quote (S "X"), 
        S "should be modified as well"] +:+ foldlSent (zipWith tableShows refs trailings)

-- generalized traceability matrix and graph introduction: variables are references to the three tables
-- generally found in this section (in order of being mentioned)
traceGIntro :: [LabelledContent] -> [Sentence] -> [UnlabelledContent]
traceGIntro refs trailings = map ulcc [Paragraph $ foldlSent
        [(phrase purpose) `ofThe'` (plural traceyGraph),
        S "is also to provide easy", plural reference, S "on what has to be",
        S "additionally modified if a certain", phrase component +:+. S "is changed", 
        S "The arrows in the", (plural graph), S "represent" +:+.
        plural dependency, S "The", phrase component, S "at the tail of an arrow",
        S "is depended on by the", phrase component, S "at the head of that arrow. Therefore, if a",
        phrase component, S "is changed, the", plural component, S "that it points to should also",
        S "be changed"] +:+ foldlSent (zipWith tableShows refs trailings)]
 
traceMRow :: ChunkDB -> [UID]
traceMRow = nub . Map.keys . (^. refbyTable)

traceMCol :: ChunkDB -> [UID]
traceMCol = nub . concat . Map.elems . (^. refbyTable)

traceMHeader :: (ChunkDB -> [UID]) -> SystemInformation -> [Sentence]
traceMHeader f c = map (`helpToRefField` c) $ f $ _sysinfodb c
 
traceMRowHeader :: SystemInformation -> [Sentence]
traceMRowHeader = traceMHeader traceMRow

traceMColHeader :: SystemInformation -> [Sentence]
traceMColHeader = traceMHeader traceMCol

traceMColumns :: ChunkDB -> [[UID]]
traceMColumns c = map (`refbyLookup` (c ^. refbyTable)) $ traceMRow c
 
generateTraceTable :: SystemInformation -> LabelledContent
generateTraceTable c = llcc (makeTabRef "Tracey") $ Table
  (EmptyS : (traceMColHeader c))
  (makeTMatrix (traceMRowHeader c) (traceMColumns $ _sysinfodb c) $ traceMCol $ _sysinfodb c)
  (showingCxnBw traceyMatrix $
  titleize' item +:+ S "of Different" +:+ titleize' section_) True
