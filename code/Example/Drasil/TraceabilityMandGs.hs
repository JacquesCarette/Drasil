module Drasil.TraceabilityMandGs
  ( traceMGF,
    traceGIntro
   ) where

import Data.Drasil.SentenceStructures
import Control.Lens ((^.))
import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (matrix, graph)
import qualified Drasil.SRS as SRS

-- wrapper for traceMGIntro
traceMGF :: [Contents] -> [Sentence] -> [Contents] -> [Section] -> Section
traceMGF refs trailing otherContents subSec = SRS.traceyMandG ((traceMIntro refs trailing):otherContents) subSec

-- generalized traceability matrix and graph introduction: variables are references to the three tables
-- generally found in this section (in order of being mentioned)
traceMIntro :: [Contents] -> [Sentence] -> Contents
traceMIntro refs trailings = Paragraph $ foldlSent [fterm phrase purpose `ofThe'` fterm plural traceyMatrix,
        S "is to provide easy", fterm plural reference, S "on what has to be additionally modified if a certain",
        fterm phrase component, S "is changed. Every time a", fterm phrase component, S "is changed, the", 
        fterm plural item, S "in the", fterm phrase column, S "of that", fterm phrase component, S "that are",
        S "marked with an", Quote (S "X"), S "should be modified as well"] +:+
        foldlSent (zipWith tableShows refs trailings)

-- generalized traceability matrix and graph introduction: variables are references to the three tables
-- generally found in this section (in order of being mentioned)
traceGIntro :: [Contents] -> [Sentence] -> [Contents]
traceGIntro refs trailings = [Paragraph $ foldlSent
        [(fterm phrase purpose) `ofThe'` (fterm plural traceyGraph),
        S "is also to provide easy", fterm plural reference, S "on what has to be",
        S "additionally modified if a certain", fterm phrase component +:+. S "is changed", 
        S "The arrows in the", (plural $ graph ^. term), S "represent" +:+.
        fterm plural dependency, S "The", fterm phrase component, S "at the tail of an arrow",
        S "is depended on by the", fterm phrase component, S "at the head of that arrow. Therefore, if a",
        fterm phrase component, S "is changed, the", fterm plural component, S "that it points to should also",
        S "be changed"] +:+ foldlSent (zipWith tableShows refs trailings),
        Paragraph $ foldlSent [S "NOTE: Building a tool to automatically generate", 
        S "graphical representation" `ofThe` (fterm phrase $ matrix ^. term), S "by scanning the",
        fterm plural label, S "and", fterm phrase reference, S "can be future work"]]