module Language.Drasil.Misc where

import Language.Drasil.Spec
import Language.Drasil.Chunk.Quantity

import Control.Lens ((^.))

-- Should now make rows.
mkTable :: [a -> b] -> [a] -> [[b]]
mkTable _     []  = []
mkTable []     _  = error "Attempting to make table without data"
mkTable fl (c:cl) = map ($ c) fl : mkTable fl cl

{-
-- where should this go?
unit'2Contents :: Unit' u => u -> Sentence
unit'2Contents x = maybe (S "") Sy (x ^. unit')
-}

{-
unit'2Contents :: Quantity u => u -> Sentence
unit'2Contents x = maybe (S "") Sy (x ^. getUnit)-}

--FIXME: This is a placeholder until getUnit is working.
--       It should be replaced with something like the above.
unit'2Contents :: Quantity u => u -> Sentence
unit'2Contents x = S ""