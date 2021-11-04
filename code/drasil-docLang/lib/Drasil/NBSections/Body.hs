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
reviewSec = NB.review 1

-- | Main Idea Section.
mainIdeaSec :: [Contents] -> Section
mainIdeaSec = NB.mainIdea 1

-- | Method and Analysis Section.
mthdAndanls :: [Contents] -> Section
mthdAndanls = NB.methAndAnls 1

-- | Example Section.
exampleSec :: [Contents] -> Section
exampleSec = NB.example 1