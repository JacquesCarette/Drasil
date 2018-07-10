module Drasil.Sections.TraceabilityMandGs
  ( traceMGF,
    traceGIntro
   ) where

import Data.Drasil.SentenceStructures (ofThe, ofThe', foldlSent, tableShows)
import Language.Drasil
import Data.Drasil.Concepts.Documentation (purpose, component, column, label, reference,
  traceyGraph, traceyMatrix, item, dependency)
import Data.Drasil.Concepts.Math (matrix, graph)
import qualified Drasil.SRS as SRS

-- wrapper for traceMGIntro
traceMGF :: [LabelledContent] -> [Sentence] -> [Contents] -> [Section] -> Section
traceMGF refs trailing otherContents subSec = SRS.traceyMandG ((traceMIntro refs trailing):otherContents) subSec

-- generalized traceability matrix and graph introduction: variables are references to the three tables
-- generally found in this section (in order of being mentioned)
traceMIntro :: [LabelledContent] -> [Sentence] -> Contents
traceMIntro refs trailings = Paragraph $ foldlSent [(phrase purpose) `ofThe'` (plural traceyMatrix),
        S "is to provide easy", plural reference, S "on what has to be additionally modified if a certain",
        phrase component, S "is changed. Every time a", phrase component, S "is changed, the", 
        plural item, S "in the", phrase column, S "of that", phrase component, S "that are",
        S "marked with an", Quote (S "X"), S "should be modified as well"] +:+
        foldlSent (zipWith tableShows refs trailings)

-- generalized traceability matrix and graph introduction: variables are references to the three tables
-- generally found in this section (in order of being mentioned)
traceGIntro :: [LabelledContent] -> [Sentence] -> Contents
traceGIntro refs trailings = Paragraph $ foldlSent
        [(phrase purpose) `ofThe'` (plural traceyGraph),
        S "is also to provide easy", plural reference, S "on what has to be",
        S "additionally modified if a certain", phrase component +:+. S "is changed", 
        S "The arrows in the", (plural graph), S "represent" +:+.
        plural dependency, S "The", phrase component, S "at the tail of an arrow",
        S "is depended on by the", phrase component, S "at the head of that arrow. Therefore, if a",
        phrase component, S "is changed, the", plural component, S "that it points to should also",
        S "be changed"] +:+ foldlSent (zipWith tableShows refs trailings)
        --Paragraph $ foldlSent [S "NOTE: Building a tool to automatically generate", 
        --S "graphical representation" `ofThe` (phrase matrix), S "by scanning the",
        --plural label, S "and", phrase reference, S "can be future work"]]
