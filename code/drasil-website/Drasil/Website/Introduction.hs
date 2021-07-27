module Drasil.Website.Introduction (introSec, introRefs) where

import Language.Drasil
import Utils.Drasil

-----------------------
-- Introduction Section
-----------------------

-- | Creates the introduction section.
introSec ::  Reference -> Reference -> Reference -> Section
introSec r1 r2 r3 = section (S "Introduction") (map mkParagraph [introParagraph1, introParagraph2 r1 r2 r3]) [] introSecRef

-- | Paragraph to introduce Drasil and its goals.
introParagraph1 :: Sentence
introParagraph1 = S "Drasil is a framework for generating all of the software artifacts from a stable knowledge base, \
  \focusing currently on scientific software. The main goals are to reduce knowledge duplication and \
  \improve traceability. The artifacts are generated from a common knowledge-base using recipes \
  \written in a Domain-Specific Language (DSL). These recipes allow us to specify which pieces of \
  \knowledge should be used in which artifacts, how to transform them, and more."

-- | Paragraph to describe the layout of the Drasil website.
introParagraph2 :: Reference -> Reference -> Reference -> Sentence
introParagraph2 caseStudySecRef docsRef graphSecRef = S "This webpage is designed to contain the most up to date" +:+
  foldlList Comma List (zipWith (\x y -> namedRef x (S y)) [caseStudySecRef, docsRef, graphSecRef] ["case study artifacts", "Haddock documentation", "package dependency graphs"])
  +:+ S "from the Drasil repository. \
  \The case study artifacts include the Software Requirements Specification (SRS) for each case study, \
  \which specifies what the program sets out to achieve. \
  \The Haddock Documentation section contains the current documentation for the Drasil framework. \
  \The package dependency graphs shows the hierarchy of modules within each package."
  -- \The footer of this page contains the continuous integration build of the project, \
  -- \as well as the commit number that the build and artifacts are based off of.

-- | Section reference.
introSecRef :: Reference
introSecRef = makeSecRef "Introduction" $ S "Introduction"

-- | Gathers all references used in this file.
introRefs :: Reference -> Reference -> Reference -> [Reference]
introRefs r1 r2 r3 = [introSecRef, ref $ introSec r1 r2 r3]