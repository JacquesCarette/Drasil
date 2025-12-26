-- | For deriving equations in examples.
module Theory.Drasil.Components.Derivation where

import Language.Drasil (Sentence(EmptyS, S), (+:))
import Control.Lens (Lens')

-- * Type

-- | Derivations are an ordered list of sentences and expressions.
-- They are rendered in order as paragraphs and equation blocks to display
-- the derivation.
data Derivation = Derivation Sentence [Sentence]

-- * Class

-- | A class that might have a 'Derivation'.
class MayHaveDerivation c where
  -- | Provides a 'Lens' to a possible derivation.
  derivations :: Lens' c (Maybe Derivation)

-- * Functions

-- | Smart constructor for creating a 'Derivation'.
mkDeriv :: Sentence -> [Sentence] -> Derivation
mkDeriv = Derivation

-- | Similar to 'mkDeriv', but prepends "Detailed derivation of" to the header.
mkDerivName :: Sentence -> [Sentence] -> Derivation
mkDerivName s = Derivation (S "Detailed derivation of" +: s)

-- | Similar to 'mkDeriv', but without a header 'Sentence'.
mkDerivNoHeader :: [Sentence] -> Derivation
mkDerivNoHeader = Derivation EmptyS
