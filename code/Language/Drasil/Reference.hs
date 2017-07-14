module Language.Drasil.Reference where

import Language.Drasil.Document
import Language.Drasil.Spec
import Control.Lens ((^.))


-- | Create References to a given 'LayoutObj'
makeRef :: (LayoutObj l) => l -> Sentence
makeRef r = Ref (rType r) (refName r)

acroTest :: Contents -> [Contents] -> Sentence
acroTest ref reflst = makeRef $ find ref reflst

find :: Contents -> [Contents] -> Contents
find _ [] = error "This object does not match any of the enumerated objects provided by the list."
find itm@(Assumption comp1 _) (frst@(Assumption comp2 _):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst