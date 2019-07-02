{-# Language TemplateHaskell #-}
module Language.Drasil.Derivation where

import Control.Lens (makeLenses)

import Language.Drasil.Sentence (Sentence(S), (+:))

-- Derivations are an ordered list of sentences and expressions.
-- They are rendered in order as paragraphs and equation blocks to display
-- the derivation.
data Derivation = Deriv { _title :: Sentence, _deriv :: [Sentence] }
makeLenses ''Derivation

mkDeriv :: Sentence -> [Sentence] -> Derivation
mkDeriv = Deriv

mkDerivName :: Sentence -> [Sentence] -> Derivation
mkDerivName s = Deriv (S "Detailed derivation of" +: s)
