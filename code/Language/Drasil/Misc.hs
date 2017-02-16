module Language.Drasil.Misc where

import Language.Drasil.Spec
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Unit
import Language.Drasil.Chunk (NamedIdea, getA)

import Control.Lens ((^.))

-- Should now make rows.
mkTable :: [a -> b] -> [a] -> [[b]]
mkTable _     []  = []
mkTable []     _  = error "Attempting to make table without data"
mkTable fl (c:cl) = map ($ c) fl : mkTable fl cl


-- where should this go?
unit'2Contents :: Quantity u => u -> Sentence
unit'2Contents x = maybe (S "") (\y -> Sy (y ^. unit)) (getUnit x)

getAcc :: (NamedIdea c) => c -> Sentence
getAcc = (\(Just x) -> x) . getA
