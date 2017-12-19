module Language.Drasil.Reference where

import Language.Drasil.Document
import Language.Drasil.Spec
import Control.Lens ((^.))

--import Language.Drasil.Chunk.Relation
--import Language.Drasil.Chunk.Eq


-- | Create References to a given 'LayoutObj'
makeRef :: (LayoutObj l) => l -> Sentence
makeRef r = Ref (rType r) (refName r)

-- This works for passing the correct id to the reference generator for Assumptions,
-- Requirements and Likely Changes but I question whether we should use it.
-- Pass it the item to be referenced and the enumerated list of the respective
-- contents for that file. Change rType values to implement.


acroTest :: Contents -> [Contents] -> Sentence
acroTest ref reflst = makeRef $ find ref reflst

find :: Contents -> [Contents] -> Contents
find _ [] = error "This object does not match any of the enumerated objects provided by the list."
find itm@(Assumption comp1) (frst@(Assumption comp2):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find itm@(Definition (Data comp1)) (frst@(Definition (Data comp2)):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find itm@(Definition (Theory comp1)) (frst@(Definition (Theory comp2)):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find itm@(Requirement comp1) (frst@(Requirement comp2):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find itm@(LikelyChange comp1) (frst@(LikelyChange comp2):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find itm@(UnlikelyChange comp1) (frst@(UnlikelyChange comp2):lst)
  | (comp1 ^. id) == (comp2 ^. id) = frst
  | otherwise = find itm lst
find _ _ = error "Error: Attempting to find unimplemented type"
