module Language.Drasil.Recipe(Recipe(..)) where

import Language.Drasil.Output.Formats (DocType(..))
import Language.Drasil.Document (Document(..))

data Recipe = Recipe DocType Document


