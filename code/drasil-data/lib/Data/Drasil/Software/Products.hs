-- | Defines common software products.
module Data.Drasil.Software.Products
  ( module Data.Drasil.Software.Products
  , module Drasil.Metadata.Software.Products
  ) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (game, video, open, source)
import Data.Drasil.Concepts.Computation (computer)
import Data.Drasil.Concepts.Software (program)
import Drasil.Metadata.Domains (progLanguage)
import Drasil.Metadata.Software.Products (sciCompS)

prodtcon :: [IdeaDict]
prodtcon = [sciCompS, videoGame, openSource, compPro]

matlab :: CI
matlab     = commonIdeaWithDict "matlab" (pn' "MATLAB programming language")       "MATLAB"  [progLanguage]

videoGame, openSource, compPro :: IdeaDict
videoGame   = compoundNC video game
openSource  = compoundNC open source
compPro     = compoundNC computer program
