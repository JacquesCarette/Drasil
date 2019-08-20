{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Contains Sentences and helpers
module Language.Drasil.Sentence (Sentence(..), SentenceStyle(..), (+:+),
  (+:+.), (+:), capSent, ch, sC, sDash, sentencePlural, sentenceShort,
  sentenceSymb, sentenceTerm, sParen) where

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol)
import Language.Drasil.Expr (Expr)
import Language.Drasil.RefProg (Reference)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.UnitLang (USymb)
import Language.Drasil.UID (UID)

import Control.Lens ((^.))

import Data.Char (toUpper)

-- | For writing "sentences" via combining smaller elements
-- Sentences are made up of some known vocabulary of things:
-- - units (their visual representation)
-- - words (via String)
-- - special characters
-- - accented letters
-- - References to specific layout objects
data SentenceStyle = PluralTerm
                   | SymbolStyle
                   | TermStyle
                   | ShortStyle

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

-- The HasSymbol is redundant, but on purpose
ch :: (HasUID c, HasSymbol c) => c -> Sentence
ch x = Ch SymbolStyle (x ^. uid)

instance Semigroup Sentence where
  (<>) = (:+:)

instance Monoid Sentence where
  mempty = EmptyS
  mappend = (:+:)

sentencePlural, sentenceShort, sentenceSymb, sentenceTerm :: UID -> Sentence
sentencePlural = Ch PluralTerm
sentenceShort  = Ch ShortStyle
sentenceSymb   = Ch SymbolStyle
sentenceTerm   = Ch TermStyle

-- | Helper for wrapping sentences in parentheses.
sParen :: Sentence -> Sentence
sParen x = S "(" :+: x :+: S ")"

-- | Helper for concatenating two sentences with a space-surrounded dash between them.
sDash :: Sentence -> Sentence -> Sentence
sDash a b = a +:+ S "-" +:+ b

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

-- | Capitalizes a Sentence.
capSent :: Sentence -> Sentence
capSent (S (s:ss)) = S (toUpper s : ss)
--capSent (phrase x) = atStart x
--capSent (plural x) = atStart' x
capSent (a :+: b)  = capSent a :+: b
capSent x          = x
