module Utils.Drasil.Sentence (andIts, andThe, fromThe, inThe, isExpctdToHv, isThe, ofGiv, forTT, forTT',
  ofGiv', ofThe, the_ofThe, the_ofThe', sOf, sOfA, sOr, sVersus, sAnd, sAre, sIn, sIs, toThe, sFor, sFor', sFor'',
  denotes, wrt, defnAs) where

import Language.Drasil

---------
-- This contains English-based combinators that may not necessarily have a conceptual link
--------

sentHelper :: String -> Sentence -> Sentence -> Sentence
-- | Inserts a String between two Sentences
sentHelper inStr a b = a +:+ S inStr +:+ b

andIts, andThe, fromThe, inThe, isExpctdToHv, isThe, ofGiv, ofGiv', ofThe, the_ofThe, the_ofThe', sOf, sOfA,
  sOr, sVersus, sAnd, sAre, sIn, sIs, toThe, sFor, denotes, wrt, defnAs :: Sentence -> Sentence -> Sentence

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
-- | Inserts the words "of a" between two Sentences
sOfA    = sentHelper "of a"
-- | Inserts the word "or" between two Sentences
sOr     = sentHelper "or"
-- | Inserts the word "versus" between two Sentences
sVersus = sentHelper "versus"
-- | Inserts the words "to the" between two Sentences
toThe   = sentHelper "to the"
-- | Inserts the words "of the" between two Sentences
ofThe   = sentHelper "of the"
-- | Inserts the word "for" between two Sentences
sFor    = sentHelper "for"
-- | Inserts the words "denotes the" between two Sentences
denotes = sentHelper "denotes the"
-- | Inserts the words "with respect to" between two Sentences
wrt     = sentHelper "with respect to"
-- | Inserts the words "defined as" between two Sentences
defnAs  = sentHelper "defined as"

-- | Similar to 'sFor', but both terms are 'titleize'd
sFor' :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
sFor' t1 t2 = titleize t1 +:+ S "for" +:+ titleize t2
-- | Similar to 'sFor'', but takes two arguments (for capitalization or pluralization) to apply to the two terms respectively
sFor'' :: (c -> Sentence) -> (d -> Sentence) -> c -> d -> Sentence
sFor'' f1 f2 t1 t2 = f1 t1 +:+ S "for" +:+ f2 t2   

-- | Similar to 'sFor', but used for titles and first 'NamedIdea' is pluralized
forTT :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
forTT = sFor'' titleize' titleize
-- | Similar to 'forTT', but both 'NamedIdea's are pluralized
forTT' :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
forTT' = sFor'' titleize' titleize'

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
