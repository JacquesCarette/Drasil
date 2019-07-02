module Language.Drasil.Derivation where

import Language.Drasil.Sentence (Sentence(EmptyS, S), (+:))

-- Derivations are an ordered list of sentences and expressions.
-- They are rendered in order as paragraphs and equation blocks to display
-- the derivation.
data Derivation = Derivation Sentence [Sentence]

mkDeriv :: Sentence -> [Sentence] -> Derivation
mkDeriv = Derivation

mkDerivName :: Sentence -> [Sentence] -> Derivation
mkDerivName s = Derivation (S "Detailed derivation of" +: s)

mkDerivNoHeader :: [Sentence] -> Derivation
mkDerivNoHeader = Derivation EmptyS
