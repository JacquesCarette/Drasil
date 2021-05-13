module Utils.Drasil.Sentence (andIts, andThe, fromThe, inThe, isExpctdToHv, isThe, ofGiv,
  ofGiv', ofThe, the_ofThe, the_ofThe', sOf, sOr, sVersus, sAnd, sAre, sIn, sIs, toThe, sFor, sFor', sFor'') where

import Language.Drasil

sentHelper :: String -> Sentence -> Sentence -> Sentence
sentHelper inStr a b = a +:+ S inStr +:+ b

andIts, andThe, fromThe, inThe, isExpctdToHv, isThe, ofGiv, ofGiv', ofThe, the_ofThe, the_ofThe', sOf,
  sOr, sVersus, sAnd, sAre, sIn, sIs, toThe, sFor :: Sentence -> Sentence -> Sentence

andIts  = sentHelper "and its"
andThe  = sentHelper "and the"
fromThe = sentHelper "from the"
inThe   = sentHelper "in the"
isThe   = sentHelper "is the"
sAnd    = sentHelper "and"
sAre    = sentHelper "are"
sIn     = sentHelper "in"
sIs     = sentHelper "is"
sOf     = sentHelper "of"
sOr     = sentHelper "or"
sVersus = sentHelper "versus"
toThe   = sentHelper "to the"
ofThe   = sentHelper "of the"
sFor    = sentHelper "for"

sFor' :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
sFor' t1 t2 = titleize t1 +:+ S "for" +:+ titleize t2
sFor'' :: (c -> Sentence) -> (d -> Sentence) -> c -> d -> Sentence
sFor'' f1 f2 t1 t2 = f1 t1 +:+ S "for" +:+ f2 t2   

isExpctdToHv a b = S "The" +:+ sentHelper "is expected to have" a b
ofGiv        a b = S "the" +:+ sentHelper "of a given"          a b
ofGiv'       a b = S "The" +:+ sentHelper "of a given"          a b
the_ofThe    a b = S "the" +:+ sentHelper "of the"              a b
the_ofThe'   a b = S "The" +:+ sentHelper "of the"              a b
