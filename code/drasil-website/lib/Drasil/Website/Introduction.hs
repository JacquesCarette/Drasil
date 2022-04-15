{-# LANGUAGE PostfixOperators #-}
-- | Introduce the Drasil website.
module Drasil.Website.Introduction where

import Language.Drasil


-- * Introduction Section

-- | Creates the introduction section.
introSec :: Reference -> Reference -> Reference -> Reference -> Reference -> Section
introSec csRef docRef analysisSecRef repoRef wikiRef = 
  section (S "Introduction") -- Title
  (map mkParagraph [introParagraph1 repoRef wikiRef, introParagraph2 csRef docRef analysisSecRef]) -- Contents
  [] $ makeSecRef "Introduction" $ S "Introduction" -- Section reference

-- | Paragraph to introduce Drasil and its goals.
introParagraph1 :: Reference -> Reference -> Sentence
introParagraph1 repoRef wikiRef = S "Drasil is a framework for generating all of the software artifacts from a stable knowledge base, \
  \focusing currently on scientific software. The main goals are to reduce knowledge duplication and \
  \improve traceability. The artifacts are generated from a common knowledge-base using recipes \
  \written in a Domain-Specific Language (DSL). These recipes allow us to specify which pieces of \
  \knowledge should be used in which artifacts, how to transform them, and more. For more information on the design, documentation, \
  \useage, and specifics of Drasil, please visit the" +:+ namedRef repoRef (S "GitHub repository") +:+ S "or the" +:+ (namedRef wikiRef (S "GitHub Wiki") !.)

-- | Paragraph to describe the layout of the Drasil website.
introParagraph2 :: Reference -> Reference -> Reference -> Sentence
introParagraph2 caseStudySecRef docsRef analysisSecRef = S "This webpage is designed to contain the most up to date" +:+
  foldlList Comma List (zipWith (\x y -> namedRef x (S y)) [caseStudySecRef, docsRef, analysisSecRef] ["case study artifacts", "Haddock documentation", "Drasil analysis"])
  +:+ S "from the Drasil repository. \
  \The case study artifacts include the Software Requirements Specification (SRS) for each case study, \
  \which specifies what the program sets out to achieve. \
  \The Haddock Documentation section contains the current documentation for the Drasil framework. \
  \The package dependency graphs shows the hierarchy of modules within each package."
  -- \The footer of this page contains the continuous integration build of the project, \
  -- \as well as the commit number that the build and artifacts are based off of.