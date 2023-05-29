{-# LANGUAGE GADTs, PostfixOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Contains Sentences and helpers functions.
module Language.Drasil.Sentence (
  -- * Types
  -- ** Sentence
  Sentence(..),
  -- ** Context Types
  SentenceStyle(..), RefInfo(..), TermCapitalization(..),
  -- * Functions
  (+:+), (+:+.), (+:), (!.), capSent, headSent, ch, eS, eS', sC, sDash, sParen,
  sentencePlural, sentenceShort,
  sentenceSymb, sentenceTerm) where

import Language.Drasil.UID (HasUID(..), UID)
import Language.Drasil.Symbol (HasSymbol, Symbol)
import Language.Drasil.ModelExpr.Lang (ModelExpr)
import Language.Drasil.ExprClasses (Express(express))
import Language.Drasil.UnitLang (USymb)

import Control.Lens ((^.))

import Data.Char (toUpper)

-- | Used in 'Ch' constructor to determine the state of a term
-- (can record whether something is in plural form, a singular term, or in short form).
data SentenceStyle = PluralTerm
                   | TermStyle
                   | ShortStyle

-- | Used in 'Ch' constructor to determine the capitalization of a term.
-- CapF is for capitalizing the first word from the 'UID' of the given term.
-- CapW is for capitalizing all words from the 'UID' of the given term.
-- Mirrors 'CapFirst' and 'CapWords' from 'CapitalizationRule'.
data TermCapitalization = CapF | CapW | NoCap

-- | Holds any extra information needed for a 'Reference', be it an equation, pages, a note, or nothing.
data RefInfo = None
             | Equation [Int]
             | Page [Int]
             | RefNote String

-- | For writing 'Sentence's via combining smaller elements.
-- 'Sentence's are made up of some known vocabulary of things:
--
--     * units (their visual representation)
--     * words (via 'String's)
--     * special characters
--     * accented letters
--     * references to specific layout objects
infixr 5 :+:
data Sentence where
  -- | Ch looks up the term for a given 'UID' and displays the term with a given 'SentenceStyle' and 'CapitalizationRule'.
  -- This allows Sentences to hold plural forms of 'NounPhrase's and 'NamedIdea's.
  Ch    :: SentenceStyle -> TermCapitalization -> UID -> Sentence
  -- | A branch of Ch dedicated to SymbolStyle only.
  SyCh  :: UID -> Sentence
  -- | Converts a unit symbol into a usable Sentence form.
  Sy    :: USymb -> Sentence
  -- | Constructor for 'String's, used often for descriptions in Chunks.
  S     :: String -> Sentence
  -- | Converts the graphical representation of a symbol into a usable Sentence form.
  P     :: Symbol -> Sentence       -- should not be used in examples?
  -- | Lifts an expression into a Sentence.
  E     :: ModelExpr -> Sentence
  -- | Takes a 'UID' to a reference, a display name ('Sentence'), and any additional reference display information ('RefInfo'). Resolves the reference later (similar to Ch).
  Ref   :: UID -> Sentence -> RefInfo -> Sentence
  -- | Adds quotation marks around a Sentence.
  Quote :: Sentence -> Sentence
  -- | Used for a % symbol.
  Percent :: Sentence
  -- | Direct concatenation of two Sentences (no implicit spaces!).
  (:+:) :: Sentence -> Sentence -> Sentence
  -- | Empty Sentence.
  EmptyS :: Sentence

eS :: ModelExpr -> Sentence
eS = E

eS' :: Express t => t -> Sentence
eS' = E . express

-- The HasSymbol is redundant, but on purpose
-- | Gets a symbol and places it in a 'Sentence'.
ch :: (HasUID c, HasSymbol c) => c -> Sentence
ch x = SyCh (x ^. uid)

-- | Sentences can be concatenated.
instance Semigroup Sentence where
  (<>) = (:+:)

-- | Sentences can be empty or directly concatenated.
instance Monoid Sentence where
  mempty = EmptyS

-- | Smart constructors for turning a 'UID' into a 'Sentence'.
sentencePlural, sentenceShort, sentenceSymb, sentenceTerm :: UID -> Sentence
-- | Gets plural term of 'UID'.
sentencePlural = Ch PluralTerm NoCap
-- | Gets short form of 'UID'.
sentenceShort  = Ch ShortStyle NoCap
-- | Gets symbol form of 'UID'.
sentenceSymb   = SyCh
-- | Gets singular form of 'UID'.
sentenceTerm   = Ch TermStyle NoCap

-- | Helper for wrapping 'Sentence's in parentheses.
sParen :: Sentence -> Sentence
sParen x = S "(" :+: x :+: S ")"

-- | Helper for concatenating two 'Sentence's with a space-surrounded dash between them.
sDash :: Sentence -> Sentence -> Sentence
sDash a b = a +:+ S "-" +:+ b

-- | Helper for concatenating two 'Sentence's with a space between them.
(+:+) :: Sentence -> Sentence -> Sentence
EmptyS +:+ b = b
a +:+ EmptyS = a
a +:+ b = a :+: S " " :+: b

-- | Helper for concatenating two 'Sentence's with a comma and space between them.
sC :: Sentence -> Sentence -> Sentence
a `sC` b = a :+: S "," +:+ b

-- | Helper which concatenates two 'Sentence's using '+:+' and appends a period.
(+:+.) :: Sentence -> Sentence -> Sentence
a +:+. b = a +:+ b :+: S "."

-- | Helper which appends a period to the end of a 'Sentence' (used often as a post-fix operator).
(!.) :: Sentence -> Sentence
(!.) a = a :+: S "."

-- | Helper which concatenates two sentences using '+:+' and appends a colon.
(+:) :: Sentence -> Sentence -> Sentence
a +: b = a +:+ b :+: S ":"

-- | Capitalizes a Sentence.
capSent :: Sentence -> Sentence
capSent (S (s:ss)) = S (toUpper s : ss)
--capSent (phrase x) = atStart x
--capSent (plural x) = atStart' x
capSent (a :+: b)  = capSent a :+: b
capSent x          = x

-- | Helper which creates a Header with size s of the 'Sentence'.
headSent :: Int -> Sentence -> Sentence
headSent s x = S (concat (replicate s "#")) :+: S " " :+: x