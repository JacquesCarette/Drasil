-- | Defines lesson plan notebook section constructors.
module Drasil.NBSections.Body (
  -- * Constructors
  reviewSec, mainIdeaSec, supportSS1, supportSS2, supportSS3, mthdAndanls, exampleSec) where

import Language.Drasil
import qualified Drasil.DocLang.Notebook as NB (review, mainIdea, hormotion, vermotion, 
  summary, methAndAnls, example)

-- Leave blank for now
--bodyIntro :: Contents
--bodyIntro = foldlSP [S ""]

-- | Review Section.
reviewSec :: [Contents] -> Section
reviewSec = NB.review "body" 1

-- | Main Idea Section.
mainIdeaSec :: [Contents] -> Section
mainIdeaSec = NB.mainIdea "body" 1

-- | Method and Analysis Section.
mthdAndanls :: [Contents] -> Section
mthdAndanls = NB.methAndAnls "body" 1

-- | Example Section.
exampleSec :: [Contents] -> Section
exampleSec = NB.example "body" 1

-- | Support main idea Subsection.
supportSS1 :: [Contents] -> Section
supportSS1 = NB.hormotion "example" 2

-- | Support main idea Subsection.
supportSS2 :: [Contents] -> Section
supportSS2 = NB.vermotion "example" 2

-- | Support main idea Subsection.
supportSS3 :: [Contents] -> Section
supportSS3 = NB.summary "example" 2