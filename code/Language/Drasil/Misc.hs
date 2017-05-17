module Language.Drasil.Misc where

import Language.Drasil.Spec
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Unit
import Language.Drasil.Chunk.NamedIdea (NamedIdea, getA, short, term)
import Language.Drasil.Chunk.Unitary
import Language.Drasil.NounPhrase (titleize)

import Control.Lens ((^.))

{- |
  Create a table body (not including header row) by applying the given
  functions to the column elements of the table rows (in order).
  The first argument is a list of functions to be applied (one per column).
  This essentially creates the rows.
  The second argument is a list of elements apply the functions to.

  For example, @mkTable [id, *5] [1,2,3]@ should produce a table:
  
  > | 1 |  5 |
  > | 2 | 10 |
  > | 3 | 15 |
  
-}
mkTable :: [a -> b] -> [a] -> [[b]]
mkTable _     []  = []
mkTable []     _  = error "Attempting to make table without data"
mkTable fl (c:cl) = map ($ c) fl : mkTable fl cl


-- where should this go?
-- | Get the units for a Quantity, if they exist, and wrap them as a Sentence
unit'2Contents :: Quantity u => u -> Sentence
unit'2Contents x = maybe (EmptyS) (\y -> Sy (y ^. usymb)) (getUnit x)

-- | Unwrap the /maybe/ abbreviation/acronym for a chunk. 
-- Only to be used on chunks that definitely __have__ an abbreviation.
getAcc :: (NamedIdea c) => c -> Sentence
getAcc = (\(Just x) -> x) . getA

-- | Helper for getting the unit's symbol from a chunk, 
-- as opposed to the symbols of the chunk itself.
unit_symb :: (Unitary c) => c -> USymb
unit_symb c = (unit c) ^. usymb

-- | Helper for common pattern of introducing the title-case version of a 
-- noun phrase (from a NamedIdea)
-- followed by its abbreviation in parentheses.
introduceAbb :: NamedIdea n => n -> Sentence
introduceAbb n = (titleize $ n ^. term) +:+ (sParen (short n))