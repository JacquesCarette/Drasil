module Utils.Drasil.Sentence (andIts, andThe, isExpctdToHv, isThe, ofGiv,
  ofGiv', ofThe, ofThe', sOf, sOr, sVersus, sAnd, sAre, sIn, sIs, toThe) where

import Language.Drasil

andIts, andThe, isExpctdToHv, isThe, ofGiv, ofGiv', ofThe, ofThe', sOf, sOr,
  sVersus, sAnd, sAre, sIn, sIs, toThe :: Sentence -> Sentence -> Sentence

andIts p1 p2 = p1 +:+ S "and its" +:+ p2
andThe p1 p2 = p1 +:+ S "and the" +:+ p2
isThe p1 p2 = p1 +:+ S "is the" +:+ p2
sAnd p1 p2 = p1 +:+ S "and" +:+ p2
sAre p1 p2 = p1 +:+ S "are" +:+ p2
sIn p1 p2 = p1 +:+ S "in" +:+ p2
sIs p1 p2 = p1 +:+ S "is" +:+ p2
sOf p1 p2 = p1 +:+ S "of" +:+ p2
sOr p1 p2 = p1 +:+ S "or" +:+ p2
sVersus p1 p2 = p1 +:+ S "versus" +:+ p2
toThe p1 p2 = p1 +:+ S "to the" +:+ p2

isExpctdToHv a b = S "The" +:+ a +:+ S "is expected to have" +:+ b
ofGiv  p1 p2 = S "the" +:+ p1 +:+ S "of a given" +:+ p2
ofGiv' p1 p2 = S "The" +:+ p1 +:+ S "of a given" +:+ p2
ofThe  p1 p2 = S "the" +:+ p1 +:+ S "of the" +:+ p2
ofThe' p1 p2 = S "The" +:+ p1 +:+ S "of the" +:+ p2
