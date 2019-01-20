{-# LANGUAGE GADTs #-}
-- | Contains Sentences and helpers
module Language.Drasil.Sentence
  (Sentence(Ch, Sy, S, E, Quote, (:+:), EmptyS, P, Ref, Percent),
   sParen, (+:+), sC, (+:+.), (+:), ch,
   SentenceStyle(..), sentenceShort, sentenceSymb, sentenceTerm, sentencePlural) where

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol)
import Language.Drasil.Expr (Expr)
import Language.Drasil.RefProg (Reference)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.UnitLang (USymb)
import Language.Drasil.UID (UID)

import Control.Lens ((^.))

-- | For writing "sentences" via combining smaller elements
-- Sentences are made up of some known vocabulary of things:
-- - units (their visual representation)
-- - words (via String)
-- - special characters
-- - accented letters
-- - References to specific layout objects
data SentenceStyle = ShortStyle
                   | SymbolStyle
                   | TermStyle
                   | PluralTerm

infixr 5 :+:
data Sentence where
  Ch    :: SentenceStyle -> UID -> Sentence
  Sy    :: USymb -> Sentence
  S     :: String -> Sentence       -- Strings, used for Descriptions in Chunks
  P     :: Symbol -> Sentence       -- should not be used in examples?
  E     :: Expr -> Sentence
  Ref   :: Reference -> Sentence

  Quote :: Sentence -> Sentence     -- Adds quotation marks around a sentence
  Percent :: Sentence               -- % symbol
                                    
  -- Direct concatenation of two Sentences (no implicit spaces!)
  (:+:) :: Sentence -> Sentence -> Sentence   
  EmptyS :: Sentence

ch :: (HasUID c, HasSymbol c) => c -> Sentence
ch x = Ch SymbolStyle (x ^. uid)

instance Semigroup Sentence where
  (<>) = (:+:)

instance Monoid Sentence where
  mempty = EmptyS
  mappend = (:+:)

sentenceShort :: UID ->Sentence
sentenceShort u = Ch ShortStyle u

sentenceSymb :: UID ->Sentence
sentenceSymb u = Ch SymbolStyle u

sentenceTerm :: UID ->Sentence
sentenceTerm u = Ch TermStyle u

sentencePlural :: UID ->Sentence
sentencePlural u = Ch PluralTerm u
-- | Helper function for wrapping sentences in parentheses.
sParen :: Sentence -> Sentence
sParen x = S "(" :+: x :+: S ")"

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
