module Language.Drasil.Reference where

import Language.Drasil.Document
import Language.Drasil.Spec


-- | Create References to a given 'LayoutObj'
makeRef :: (LayoutObj l) => l -> Sentence
makeRef r = Ref (rType r) (refName r)
