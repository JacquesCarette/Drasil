module Drasil.NBSections.Body (bodyIntro, reviewSec, mainIdeaSec, mthdAndanls) where

import Language.Drasil
import Utils.Drasil
import qualified Drasil.DocLang.Notebook as NB (review, mainIdea, methAndAnls)

-- **** Leave blank for now
bodyIntro :: Contents
bodyIntro = foldlSP [S ""]

-- Review
reviewSec :: [Contents] -> Section
reviewSec cs = NB.review cs []

-- Main Idea
mainIdeaSec :: [Contents] -> [Section] -> Section
mainIdeaSec a subSec = NB.mainIdea a subSec

-- Method and Anaysis
mthdAndanls :: [Contents] -> [Section] -> Section
mthdAndanls a subSec = NB.methAndAnls a subSec