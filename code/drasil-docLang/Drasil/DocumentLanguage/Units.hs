-- Helpers for dealing with formatting of Units
module Drasil.DocumentLanguage.Units where

import Language.Drasil (Sentence(S, Sy), usymb, MayHaveUnit(getUnit))

-- | Get the units, if they exist, and wrap them as a 'Sentence'. Default value is \"--\".
toSentence :: (MayHaveUnit u) => u -> Sentence
toSentence x = maybe (S "--") (Sy . usymb) (getUnit x)

-- | Similar to 'toSentence', except default value is \"Unitless\".
toSentenceUnitless :: (MayHaveUnit u) => u -> Sentence
toSentenceUnitless x = maybe (S "Unitless") (Sy . usymb) (getUnit x)
