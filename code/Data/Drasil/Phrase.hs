module Data.Drasil.Phrase where
import Language.Drasil

import Control.Lens ((^.))
  
{-of_'' :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_'' t1 t2 = nounPhrase'' 
  ((phrase $ t1^.term) +:+ S "of" +:+ (phrase $ t2^.term))
  ((plural $ t1^.term) +:+ S "of" +:+ (phrase $ t2^.term))
  (Replace ((at_start $ t1 ^. term) +:+ S "of" +:+ (phrase $ t2^.term)))
  (Replace ((titleize $ t1 ^. term) +:+ S "of" +:+ (titleize $ t2 ^. term)))-}