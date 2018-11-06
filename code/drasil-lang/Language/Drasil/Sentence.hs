{-# LANGUAGE GADTs #-}
-- | Contains Sentences and helpers
module Language.Drasil.Sentence
  (Sentence(Ch, Sy, S, Sp, E, Ref, Quote, (:+:), EmptyS, P),
   sParen, sSqBr, (+:+), sC, (+:+.), (+:)) where

import Language.Drasil.Unicode (Special(SqBrClose, SqBrOpen))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Expr (Expr)
import Language.Drasil.RefTypes (Reference)
import Language.Drasil.UnitLang (USymb)
import Language.Drasil.UID (UID)

-- | For writing "sentences" via combining smaller elements
-- Sentences are made up of some known vocabulary of things:
-- - units (their visual representation)
-- - words (via String)
-- - special characters
-- - accented letters
-- - References to specific layout objects
infixr 5 :+:
data Sentence where
  Ch    :: UID -> Sentence
  Sy    :: USymb -> Sentence        -- Unit Symbol
  S     :: String -> Sentence       -- Strings, used for Descriptions in Chunks
  Sp    :: Special -> Sentence
  P     :: Symbol -> Sentence       -- should not be used in examples?
  E     :: Expr -> Sentence
  Ref   :: Reference -> Sentence  -- Needs helper func to create Ref
                                                       -- See Reference.hs
  Quote :: Sentence -> Sentence     -- Adds quotation marks around a sentence
                                    
  -- Direct concatenation of two Sentences (no implicit spaces!)
  (:+:) :: Sentence -> Sentence -> Sentence   
  EmptyS :: Sentence

instance Monoid Sentence where
  mempty = EmptyS
  mappend = (:+:)

-- | Helper function for wrapping sentences in parentheses.
sParen :: Sentence -> Sentence
sParen x = S "(" :+: x :+: S ")"

-- | Helper function for wrapping sentences in square brackets.
sSqBr :: Sentence -> Sentence
sSqBr x = Sp SqBrOpen :+: x :+: Sp SqBrClose

-- | Helper for concatenating two sentences with a space between them.
(+:+) :: Sentence -> Sentence -> Sentence
EmptyS +:+ b = b
a +:+ EmptyS = a
a +:+ b = a :+: S " " :+: b

-- | Helper for concatenating two sentences with a comma and space between them.
sC :: Sentence -> Sentence -> Sentence
a `sC` b = a :+: S "," +:+ b

-- | Helper which concatenates two sentences using '+:+' then adds a period to
-- the end.
(+:+.) :: Sentence -> Sentence -> Sentence
a +:+. b = a +:+ b :+: S "."

-- | Helper which concatenates two sentences using '+:+' then adds a colon to
-- the end.
(+:) :: Sentence -> Sentence -> Sentence
a +: b = a +:+ b :+: S ":"
