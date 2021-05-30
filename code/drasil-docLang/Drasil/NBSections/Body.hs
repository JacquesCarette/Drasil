module Drasil.NBSections.Body (reviewSec, motionSec, mthdAndanls) where

import qualified Drasil.DocLang.Notebook as NB (review, motion, method)

-- **** Leave blank for now
bodyIntro :: Contents
bodyIntro = foldlSP [S ""]

-- Review
reviewSec :: [Contents] -> Section
reviewSec cs = NB.review cs []

-- Motion
-- **** Rename the subsection
motionSec :: [Contents] -> [Section] -> Section
motionSec a subSec = NB.motion a subSec

-- Method and Anaysis
-- **** Rename the subsection
mthdAndanls :: [Contents] -> [Section] -> Section
mthdAndanls a subSec = NB.MethsAndAnls a subSec