-- Helpers for dealing with formatting of Units
module Drasil.DocumentLanguage.Units where

import Language.Drasil (Sentence(S, Sy), usymb, MayHaveUnit(getUnit))

-- | Get the units, if they exist, and wrap them as a Sentence
toSentence :: (MayHaveUnit u) => u -> Sentence
toSentence x = maybe (S "--") (Sy . usymb) (getUnit x)

toSentenceUnitless :: (MayHaveUnit u) => u -> Sentence
toSentenceUnitless x = maybe (S "Unitless") (Sy . usymb) (getUnit x)
