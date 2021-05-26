module Language.Drasil.Derivation where

import Language.Drasil.Sentence (Sentence(EmptyS, S), (+:))

-- | Derivations are an ordered list of sentences and expressions.
-- They are rendered in order as paragraphs and equation blocks to display
-- the derivation.
data Derivation = Derivation Sentence [Sentence]

-- | Smart constructor for creating a 'Derivation'.
mkDeriv :: Sentence -> [Sentence] -> Derivation
mkDeriv = Derivation
-- | Similar to 'mkDeriv', but prepends "Detailed derivation of" to the header.
mkDerivName :: Sentence -> [Sentence] -> Derivation
mkDerivName s = Derivation (S "Detailed derivation of" +: s)
-- | Similar to 'mkDeriv', but without a header 'Sentence'.
mkDerivNoHeader :: [Sentence] -> Derivation
mkDerivNoHeader = Derivation EmptyS
