-- | Defines lesson plan notebook section constructors.
module Drasil.NBSections.Body (
  -- * Constructors
  reviewSec, mainIdeaSec, mthdAndanls, exampleSec) where

import Language.Drasil
import qualified Drasil.DocLang.Notebook as NB (review, mainIdea, methAndAnls, example)

-- Leave blank for now
--bodyIntro :: Contents
--bodyIntro = foldlSP [S ""]

-- | Review Section.
reviewSec :: [Contents] -> Section
reviewSec cs = NB.review cs []

-- | Main Idea Section.
mainIdeaSec :: [Contents] -> [Section] -> Section
mainIdeaSec = NB.mainIdea

-- | Method and Analysis Section.
mthdAndanls :: [Contents] -> [Section] -> Section
mthdAndanls = NB.methAndAnls

-- | Example Section.
exampleSec :: [Contents] -> [Section] -> Section
exampleSec = NB.example