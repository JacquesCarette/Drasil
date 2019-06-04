module Data.Drasil.SentenceStructures
  ( tableShows
  , underConsidertn, showingCxnBw, refineChain
  , maybeChanged, maybeExpanded, maybeWOVerb
  , tAndDWAcc, tAndDWSym, tAndDOnly
  , follows
  , getTandS, getTDS
  , eqN
  , displayConstrntsAsSet, chgsStart
  ) where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation hiding (constraint)
import Data.Drasil.Concepts.Math (equation)

import Control.Lens ((^.))

{--** Miscellaneous **--}
tableShows :: LabelledContent -> Sentence -> Sentence
tableShows ref trailing = makeRef2S ref +:+ S "shows the" +:+ 
  plural dependency +:+ S "of" +:+ trailing

showingCxnBw :: NamedIdea c => c -> Sentence -> Sentence
showingCxnBw traceyVar contents = titleize traceyVar +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between" +:+ contents

underConsidertn :: ConceptChunk -> Sentence
underConsidertn chunk = S "The" +:+ phrase chunk +:+ 
  S "under consideration is" +:+. (chunk ^. defn)

-- | Create a list in the pattern of "The __ are refined to the __".
-- Note: Order matters!
refineChain :: NamedIdea c => [(c, Section)] -> Sentence
refineChain [x,y] = S "The" +:+ plural (fst x) +:+ sParen (makeRef2S $ snd x) +:+
  S "are refined to the" +:+ plural (fst y)
refineChain (x:y:xs) = refineChain [x,y] `sC` rc (y : xs)
refineChain _ = error "refineChain encountered an unexpected empty list"

-- | Helper used by refineChain
rc :: NamedIdea c => [(c, Section)] -> Sentence
rc [x,y] = S "and the" +:+ plural (fst x) +:+ sParen (makeRef2S $ snd x) 
  +:+ S "to the" +:+ plural (fst y) +:+. sParen (makeRef2S $ snd y)
rc (x:y:xs) = S "the" +:+ plural (fst x) +:+ sParen (makeRef2S $ snd x) +:+ 
  S "to the" +:+ plural (fst y) `sC` rc (y : xs)
rc _ = error "refineChain helper encountered an unexpected empty list"

-- | helper functions for making likely change statements
likelyFrame :: Sentence -> Sentence -> Sentence -> Sentence
likelyFrame a verb x = foldlSent [S "The", a, S "may be", verb, x]
maybeWOVerb, maybeChanged, maybeExpanded :: Sentence -> Sentence -> Sentence
maybeWOVerb a   = likelyFrame a EmptyS
maybeChanged a  = likelyFrame a (S "changed")
maybeExpanded a = likelyFrame a (S "expanded")

-- | helpful combinators for making Sentences for Terminologies with Definitions
-- term (acc) - definition
tAndDWAcc :: Concept s => s -> ItemType
tAndDWAcc temp = Flat $ at_start temp +:+ (sParen (short temp) `sDash` (temp ^. defn))
-- term (symbol) - definition
tAndDWSym :: (Concept s, Quantity a) => s -> a -> ItemType
tAndDWSym tD sym = Flat $ at_start tD +:+ (sParen (ch sym) `sDash` (tD ^. defn))
-- term - definition
tAndDOnly :: Concept s => s -> ItemType
tAndDOnly chunk  = Flat $ at_start chunk `sDash` (chunk ^. defn)

follows :: (Referable r, HasShortName r) => Sentence -> r -> Sentence
preceding `follows` ref = preceding +:+ S "following" +:+ makeRef2S ref

-- | Used when you want to say a term followed by its symbol. ex. "...using the Force F in..."
getTandS :: (Quantity a) => a -> Sentence
getTandS a = phrase a +:+ ch a

-- | get term, definition, and symbol
getTDS :: (Quantity a, Concept a) => a -> Sentence
getTDS a = phrase a +:+ (a ^. defn) +:+ ch a

--Ideally this would create a reference to the equation too
eqN :: Int -> Sentence
eqN n = at_start equation +:+ sParen (S $ show n)

--Produces a sentence that displays the constraints in a {}.
displayConstrntsAsSet :: Quantity a => a -> [String] -> Sentence
displayConstrntsAsSet sym listOfVals = E $ sy sym `isin` DiscreteS listOfVals

--Produces a common beginning of a likely change of the form "reference - sentence"
chgsStart :: (HasShortName x, Referable x) => x -> Sentence -> Sentence
chgsStart = sDash . makeRef2S
