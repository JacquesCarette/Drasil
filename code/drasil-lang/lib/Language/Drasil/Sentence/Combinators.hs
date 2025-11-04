-- | Defines various sentence level combinators that may be based in English and
-- not necessarily have a conceptual link. See the [Wiki](https://github.com/JacquesCarette/Drasil/wiki/Combinator-Documentation)
-- for more information. There are also @C@ variants to each combinator to
-- denote those meant for use at the start of a sentence (capitalizes the first word).
-- This module should be used as a qualified import (usually as @S@),
-- as many function names clash with those in Concepts.hs and NounPhrase.hs.
module Language.Drasil.Sentence.Combinators (
  -- * \"And\" Combinators
  and_, andIts, andThe,
  -- * \"The\" Combinators
  fromThe, inThe, onThe, toThe, isThe, ofThe,
  the_ofThe, the_ofTheC, the_ofGiv, the_ofGivC, the_isExpctdToHvC,
  -- * \"For\" Combinators
  forTPS, forTPP, for, forT, forGen,
  -- * Other Combinators
  of_, ofA, or_, are, in_, is, defnAs, denotes, versus, wrt) where

import Language.Drasil.Classes ( NamedIdea )
import Language.Drasil.Development.Sentence ( titleize, titleize' )
import Language.Drasil.Sentence ( Sentence(S), (+:+) )


sentHelper :: String -> Sentence -> Sentence -> Sentence
-- | Inserts a String between two Sentences
sentHelper inStr a b = a +:+ S inStr +:+ b

andIts, andThe, fromThe, inThe, the_isExpctdToHvC, isThe, onThe, the_ofGiv, the_ofGivC, ofThe, the_ofThe, the_ofTheC, of_, ofA,
  or_, versus, and_, are, in_, is, toThe, for, denotes, wrt, defnAs :: Sentence -> Sentence -> Sentence

-- | Inserts the words "and its" between two Sentences.
andIts  = sentHelper "and its"
-- | Inserts the words "and the" between two Sentences.
andThe  = sentHelper "and the"
-- | Inserts the words "from the" between two Sentences.
fromThe = sentHelper "from the"
-- | Inserts the words "in the" between two Sentences.
inThe   = sentHelper "in the"
-- | Inserts the words "is the" between two Sentences.
isThe   = sentHelper "is the"
-- | Inserts the words "on the" between two Sentences.
onThe   = sentHelper "on the"
-- | Inserts the word "and" between two Sentences.
and_    = sentHelper "and"
-- | Inserts the word "are" between two Sentences.
are    = sentHelper "are"
-- | Inserts the word "in" between two Sentences.
in_     = sentHelper "in"
-- | Inserts the word "is" between two Sentences.
is     = sentHelper "is"
-- | Inserts the word "of" between two Sentences.
of_     = sentHelper "of"
-- | Inserts the words "of a" between two Sentences.
ofA    = sentHelper "of a"
-- | Inserts the word "or" between two Sentences.
or_     = sentHelper "or"
-- | Inserts the word "versus" between two Sentences.
versus = sentHelper "versus"
-- | Inserts the words "to the" between two Sentences.
toThe   = sentHelper "to the"
-- | Inserts the words "of the" between two Sentences.
ofThe   = sentHelper "of the"
-- | Inserts the word "for" between two Sentences.
for    = sentHelper "for"
-- | Inserts the words "denotes the" between two Sentences.
denotes = sentHelper "denotes the"
-- | Inserts the words "with respect to" between two Sentences.
wrt     = sentHelper "with respect to"
-- | Inserts the words "defined as" between two Sentences.
defnAs  = sentHelper "defined as"

-- | Similar to 'for', but both terms are 'titleize'd.
forT :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
forT t1 t2 = titleize t1 +:+ S "for" +:+ titleize t2
-- | Similar to 'forTT', but takes two arguments (for capitalization or pluralization) to apply to the two terms respectively.
forGen :: (c -> Sentence) -> (d -> Sentence) -> c -> d -> Sentence
forGen f1 f2 t1 t2 = f1 t1 +:+ S "for" +:+ f2 t2

-- | Similar to 'for', but used for titles and first 'NamedIdea' is pluralized.
forTPS :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
forTPS = forGen titleize' titleize
-- | Similar to 'forTTPS', but both 'NamedIdea's are pluralized.
forTPP :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
forTPP = forGen titleize' titleize'

-- | Prepends \"The\" and inserts "is expected to have" between two Sentences.
the_isExpctdToHvC a b = S "The" +:+ sentHelper "is expected to have" a b
-- | Prepends "the" and inserts "of a given" between two Sentences.
the_ofGiv        a b = S "the" +:+ sentHelper "of a given"          a b
-- | Same as 'ofGiv', except first "the" is capitalized.
the_ofGivC       a b = S "The" +:+ sentHelper "of a given"          a b
-- | Same as 'ofThe', but inserts "the" at the beginning of the Sentence.
the_ofThe    a b = S "the" +:+ sentHelper "of the"              a b
-- | Same as 'the_ofThe', except first "the" is capitalized.
the_ofTheC   a b = S "The" +:+ sentHelper "of the"              a b
