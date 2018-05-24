{-# LANGUAGE GADTs #-}
-- | Contains Sentences and helpers
module Language.Drasil.Spec where

import Language.Drasil.Unicode (Special(SqBrOpen,SqBrClose))
import Language.Drasil.Symbol
import Language.Drasil.Expr
import Language.Drasil.RefTypes
import Language.Drasil.UnitLang (USymb)

-- | One slight hack remaining
type RefName = String

-- | For writing "sentences" via combining smaller elements
-- Sentences are made up of some known vocabulary of things:
-- - units (their visual representation)
-- - words (via String)
-- - special characters
-- - accented letters
-- - References to specific layout objects
infixr 5 :+:
data Sentence where
  Sy    :: USymb -> Sentence
  S     :: String -> Sentence       -- Strings, used for Descriptions in Chunks
  Sp    :: Special -> Sentence
  P     :: Symbol -> Sentence
  Ref   :: RefType -> RefAdd -> RefName -> Sentence  -- Needs helper func to create Ref
                                    -- See Reference.hs
  Quote :: Sentence -> Sentence     -- Adds quotation marks around a sentence
                                    
  -- Direct concatenation of two Sentences (no implicit spaces!)
  (:+:) :: Sentence -> Sentence -> Sentence   
  EmptyS :: Sentence
  E :: Expr -> Sentence

instance Monoid Sentence where
  mempty = EmptyS
  mappend = (:+:)

-- | Helper function for wrapping sentences in parentheses.
sParen :: Sentence -> Sentence
sParen x = S "(" :+: x :+: S ")"

sParenNum :: Int -> Sentence
sParenNum y = sParen (S (show y))

-- | Helper function for wrapping sentences in square brackets.
sSqBr :: Sentence -> Sentence
sSqBr x = Sp SqBrOpen :+: x :+: Sp SqBrClose

sSqBrNum :: Int -> Sentence
sSqBrNum y = sSqBr (S (show y))

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

-- | Helper which concatenates two sentences using ':+:' then adds a period to
-- the end.
(+.) :: Sentence -> Sentence -> Sentence
a +. b = a :+: b :+: S "."

-- | Helper which concatenates two sentences using '+:+' then adds a colon to
-- the end.
(+:) :: Sentence -> Sentence -> Sentence
a +: b = a +:+ b :+: S ":"

-- | Helper for concatenating two sentences with a semi-colon and space between them.
semiCol :: Sentence -> Sentence -> Sentence
a `semiCol` b = a :+: S ";" +:+ b

sParenDash :: Sentence -> Sentence
sParenDash = \x -> S " (" :+: x :+: S ") - "

sDash :: Sentence -> Sentence -> Sentence
y `sDash` z = y +:+ S "-" +:+ z
