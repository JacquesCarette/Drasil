module Language.Drasil.Recipe(Recipe(..)) where

import Language.Drasil.Output.Formats (DocSpec)
import Language.Drasil.Document (Document)

data Recipe = Recipe DocSpec Document


