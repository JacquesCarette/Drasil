module Language.Drasil.Misc where

import Language.Drasil.Spec
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Unit
import Language.Drasil.Chunk.NamedIdea (NamedIdea, getA, short, term)
import Language.Drasil.Chunk.Unitary
import Language.Drasil.NounPhrase (titleize)

import Control.Lens ((^.))

-- Should now make rows.
mkTable :: [a -> b] -> [a] -> [[b]]
mkTable _     []  = []
mkTable []     _  = error "Attempting to make table without data"
mkTable fl (c:cl) = map ($ c) fl : mkTable fl cl


-- where should this go?
unit'2Contents :: Quantity u => u -> Sentence
unit'2Contents x = maybe (EmptyS) (\y -> Sy (y ^. usymb)) (getUnit x)

getAcc :: (NamedIdea c) => c -> Sentence
getAcc = (\(Just x) -> x) . getA

unit_symb :: (Unitary c) => c -> USymb
unit_symb c = (unit c) ^. usymb

introduceAbb :: NamedIdea n => n -> Sentence
introduceAbb n = (titleize $ n ^. term) +:+ (sParen (short n))