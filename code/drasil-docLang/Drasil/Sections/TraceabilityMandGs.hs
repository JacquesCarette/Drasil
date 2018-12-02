module Drasil.Sections.TraceabilityMandGs
  ( traceMGF,
    traceGIntro,
    generateTraceTable
   ) where

import Language.Drasil
import Data.Drasil.SentenceStructures (ofThe', foldlSent, tableShows)
import Data.Drasil.Concepts.Documentation (purpose, component, column,
  reference, traceyGraph, traceyMatrix, item, dependency)
import Data.Drasil.Concepts.Math ( graph)
import Data.Drasil.Concepts.Documentation(item, section_)
import Data.Drasil.Utils (makeTMatrix)
import qualified Drasil.DocLang.SRS as SRS
import Data.List (nub, sort, sortBy)
import qualified Data.Map as Map
import Data.Drasil.SentenceStructures (showingCxnBw)
import Control.Lens ((^.))
import Drasil.DocumentLanguage.Definitions (helpToRefField)

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
        S "is changed, the", plural item, S "in the", phrase column, S "of that", 
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
traceMRow cdb = nub $ Map.keys (cdb ^. refbyTable)

traceMCol :: ChunkDB -> [UID]
traceMCol cdb = nub $ concat $ Map.elems (cdb ^. refbyTable)
 
traceMRowHeader :: ChunkDB -> [Sentence]
traceMRowHeader cdb = map (\x -> helpToRefField x cdb) (traceMRow cdb)

traceMColHeader :: ChunkDB -> [Sentence]
traceMColHeader cdb = map (\x -> helpToRefField x cdb) (traceMCol cdb)

helpLookup :: UID -> RefbyMap -> [UID]
helpLookup uid rm = refbyLookup uid rm

traceMColumns :: ChunkDB -> [[UID]]
traceMColumns cdb = map (\x -> helpLookup x (cdb ^. refbyTable)) (traceMRow cdb)
 
generateTraceTable :: ChunkDB -> LabelledContent
generateTraceTable cdb = llcc (mkLabelSame "Tracey" Tab) $ Table
  (EmptyS:(traceMColHeader cdb))
  (makeTMatrix (traceMRowHeader cdb) (traceMColumns cdb) (traceMCol cdb))
  (showingCxnBw traceyMatrix
  (titleize' item +:+ S "of Different" +:+ titleize' section_)) True 