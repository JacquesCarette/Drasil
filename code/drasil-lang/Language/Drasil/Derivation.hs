module Language.Drasil.Derivation where

import Language.Drasil.Spec (Sentence)

-- Derivations are an ordered list of sentences and expressions.
-- They are rendered in order as paragraphs and equation blocks to display
-- the derivation.
type Derivation = [Sentence]
