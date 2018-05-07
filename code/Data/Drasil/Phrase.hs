module Data.Drasil.Phrase where
import Language.Drasil
  
of_'' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_'' t1 t2 = nounPhrase'' 
  ((phrase t1) +:+ S "of" +:+ (phrase t2))
  ((plural t1) +:+ S "of" +:+ (phrase t2))
  (Replace ((at_start t1) +:+ S "of" +:+ (phrase t2)))
  (Replace ((titleize t1) +:+ S "of" +:+ (titleize t2)))