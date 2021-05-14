module Utils.Drasil.Sentence (andIts, andThe, fromThe, inThe, isExpctdToHv, isThe, ofGiv,
  ofGiv', ofThe, the_ofThe, the_ofThe', sOf, sOr, sVersus, sAnd, sAre, sIn, sIs, toThe) where

import Language.Drasil

---------
-- This contains English-based combinators that may not necessarily have a conceptual link
--------

sentHelper :: String -> Sentence -> Sentence -> Sentence
-- | Inserts a String between two Sentences
sentHelper inStr a b = a +:+ S inStr +:+ b

andIts, andThe, fromThe, inThe, isExpctdToHv, isThe, ofGiv, ofGiv', ofThe, the_ofThe, the_ofThe', sOf,
  sOr, sVersus, sAnd, sAre, sIn, sIs, toThe :: Sentence -> Sentence -> Sentence

-- | Inserts the words "and its" between two Sentences
andIts  = sentHelper "and its"
-- | Inserts the words "and the" between two Sentences
andThe  = sentHelper "and the"
-- | Inserts the words "from the" between two Sentences
fromThe = sentHelper "from the"
-- | Inserts the words "in the" between two Sentences
inThe   = sentHelper "in the"
-- | Inserts the words "is the" between two Sentences
isThe   = sentHelper "is the"
-- | Inserts the word "and" between two Sentences
sAnd    = sentHelper "and"
-- | Inserts the word "are" between two Sentences
sAre    = sentHelper "are"
-- | Inserts the word "in" between two Sentences
sIn     = sentHelper "in"
-- | Inserts the word "is" between two Sentences
sIs     = sentHelper "is"
-- | Inserts the word "of" between two Sentences
sOf     = sentHelper "of"
-- | Inserts the word "or" between two Sentences
sOr     = sentHelper "or"
-- | Inserts the word "versus" between two Sentences
sVersus = sentHelper "versus"
-- | Inserts the words "to the" between two Sentences
toThe   = sentHelper "to the"
-- | Inserts the words "of the" between two Sentences
ofThe   = sentHelper "of the"

-- | Prepends "The" and inserts "is expected to have" between two Sentences
isExpctdToHv a b = S "The" +:+ sentHelper "is expected to have" a b
-- | Prepends "the" and inserts "of a given" between two Sentences
ofGiv        a b = S "the" +:+ sentHelper "of a given"          a b
-- | Same as 'ofGiv', except first "the" is capitalized
ofGiv'       a b = S "The" +:+ sentHelper "of a given"          a b
-- | Same as 'ofThe', but inserts "the" at the beginning of the Sentence
the_ofThe    a b = S "the" +:+ sentHelper "of the"              a b
-- | Same as 'the_ofThe', except first "the" is capitalized
the_ofThe'   a b = S "The" +:+ sentHelper "of the"              a b
