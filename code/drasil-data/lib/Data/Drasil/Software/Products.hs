-- | Defines common software products.
module Data.Drasil.Software.Products
  ( module Data.Drasil.Software.Products
  , module Drasil.Metadata.Software.Products
  ) where

import Drasil.Database (mkUid)
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (game, video, open, source)
import Data.Drasil.Concepts.Computation (computer)
import Data.Drasil.Concepts.Software (program)
import Drasil.Metadata.Software.Products (sciCompS)

prodtcon :: [IdeaDict]
prodtcon = [sciCompS, videoGame, openSource, compPro]

videoGame, openSource, compPro :: IdeaDict
videoGame   = compoundNC video game
openSource  = compoundNC open source
compPro     = compoundNC computer program

matlab :: ConceptChunk
matlab = cncpt''' (mkUid "matlab") (pn' "MATLAB")
  (S "a proprietary multi-paradigm programming language and numeric computing environment developed by MathWorks; portmanteau for Matrix Laboratory") -- Source: <https://en.wikipedia.org/wiki/MATLAB>
