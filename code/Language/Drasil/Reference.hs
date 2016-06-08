module Language.Drasil.Reference where

import Language.Drasil.Document
import Language.Drasil.Spec

-- Creating References --
makeRef :: (LayoutObj l) => l -> Sentence
makeRef r = Ref (rType r) (refName r)