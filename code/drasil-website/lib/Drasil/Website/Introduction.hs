{-# LANGUAGE PostfixOperators #-}
-- | Introduce the Drasil website.
module Drasil.Website.Introduction where

import Language.Drasil


-- * Introduction Section

-- | Creates the about section.
introSec :: Section
introSec =
  section (S "Introduction") -- Title
  [mkParagraph introParagraph1] -- Contents
  [] $ makeSecRef "Introduction" $ S "Introduction" -- Section reference

-- | Paragraph to introduce Drasil and its goals.
introParagraph1 :: Sentence
introParagraph1 = S "Drasil is a framework for generating all of the software artifacts from a stable knowledge base, \
  \focusing currently on scientific software. We welcome students and collaborators to assist us as we research optimal ways \
  \to extend Drasil's functionality."
