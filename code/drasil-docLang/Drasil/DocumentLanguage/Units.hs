-- Helpers for dealing with formatting of Units
module Drasil.DocumentLanguage.Units where

import Language.Drasil (Quantity, Sentence(S, Sy), usymb)
import Language.Drasil.Development (getUnit)

import Control.Lens ((^.))

-- | Get the units, if they exist, and wrap them as a Sentence
toSentence :: Quantity u => u -> Sentence
toSentence x = maybe (S "--") (\y -> Sy (y ^. usymb)) (getUnit x)

toSentenceUnitless :: Quantity u => u -> Sentence
toSentenceUnitless x = maybe (S "Unitless") (\y -> Sy (y ^. usymb)) (getUnit x)
