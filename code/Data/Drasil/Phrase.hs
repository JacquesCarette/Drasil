module Data.Drasil.Phrase where
import Language.Drasil
  
of_'' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_'' t1 t2 = nounPhrase'' 
  ((phrase t1) +:+ S "of" +:+ (phrase t2))
  ((plural t1) +:+ S "of" +:+ (phrase t2))
  (Replace ((at_start t1) +:+ S "of" +:+ (phrase t2)))
  (Replace ((titleize t1) +:+ S "of" +:+ (titleize t2)))

ofA :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofA t1 t2 = nounPhrase'' 
  ((plural t1) +:+ S "of a" +:+ (phrase t2))
  ((plural t1) +:+ S "of a" +:+ (phrase t2))
  (Replace ((at_start' t1) +:+ S "of a" +:+ (phrase t2)))
  (Replace ((titleize' t1) +:+ S "of a" +:+ (titleize t2)))

andRT :: (NamedIdea c, NamedIdea d) => 
  (c -> Sentence) -> (d -> Sentence) -> c -> d -> NP
andRT f1 f2 t1 t2 = nounPhrase''
  ((phrase t1) +:+ S "and" +:+ (plural t2))
  ((phrase t1) +:+ S "and" +:+ (phrase t2))
  (Replace ((at_start t1) +:+ S "and" +:+ (phrase t2)))
  (Replace ((f1 t1) +:+ S "and" +:+ (f2 t2)))

and_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_ t1 t2 = nounPhrase''
  ((phrase t1) +:+ S "and" +:+ (phrase t2))
  ((phrase t1) +:+ S "and" +:+ (plural t2))
  (Replace ((at_start t1) +:+ S "and" +:+ (phrase t2)))
  (Replace ((titleize t1) +:+ S "and" +:+ (titleize t2)))

and_' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_' t1 t2 = nounPhrase'' 
  ((phrase t1) +:+ S "and" +:+ (plural t2))
  ((phrase t1) +:+ S "and" +:+ (plural t2))
  (Replace ((at_start t1) +:+ S "and" +:+ (plural t2)))
  (Replace ((titleize t1) +:+ S "and" +:+ (titleize' t2)))