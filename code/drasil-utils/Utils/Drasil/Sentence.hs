module Utils.Drasil.Sentence (andIts, andThe, inThe, isExpctdToHv, isThe, ofGiv,
  ofGiv', ofThe, ofThe', sOf, sOr, sVersus, sAnd, sAre, sIn, sIs, toThe) where

import Language.Drasil

sentHelper :: String -> Sentence -> Sentence -> Sentence
sentHelper inStr a b = a +:+ S inStr +:+ b

andIts, andThe, inThe, isExpctdToHv, isThe, ofGiv, ofGiv', ofThe, ofThe', sOf,
  sOr, sVersus, sAnd, sAre, sIn, sIs, toThe :: Sentence -> Sentence -> Sentence

andIts  = sentHelper "and its"
andThe  = sentHelper "and the"
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

isExpctdToHv a b = S "The" +:+ sentHelper "is expected to have" a b
ofGiv        a b = S "the" +:+ sentHelper "of a given"          a b
ofGiv'       a b = S "The" +:+ sentHelper "of a given"          a b
ofThe        a b = S "the" +:+ sentHelper "of the"              a b
ofThe'       a b = S "The" +:+ sentHelper "of the"              a b
