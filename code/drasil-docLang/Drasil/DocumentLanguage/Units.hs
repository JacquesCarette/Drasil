-- Helpers for dealing with formatting of Units
module Drasil.DocumentLanguage.Units where

import Language.Drasil (Quantity, Sentence(S, Sy), usymb)
import Language.Drasil.Development (MayHaveUnit(getUnit))

-- | Get the units, if they exist, and wrap them as a Sentence
toSentence :: (Quantity u, MayHaveUnit u) => u -> Sentence
toSentence x = maybe (S "--") (Sy . usymb) (getUnit x)

toSentenceUnitless :: (Quantity u, MayHaveUnit u) => u -> Sentence
toSentenceUnitless x = maybe (S "Unitless") (Sy . usymb) (getUnit x)
