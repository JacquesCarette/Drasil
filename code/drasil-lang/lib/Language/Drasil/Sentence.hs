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
  sentenceSymb, sentenceTerm,
  sdep, shortdep, lnames, lnames', sentenceRefs
) where

import Control.Lens ((^.))
import Data.Char (toUpper)

import Drasil.Database (HasChunkRefs(..), HasUID(..), UID)

import Language.Drasil.ExprClasses (Express(express))
import Language.Drasil.ModelExpr.Lang (ModelExpr)
import Language.Drasil.ModelExpr.Extract (meNames)
import Language.Drasil.NounPhrase.Types (NP)
import Language.Drasil.UnitLang (USymb)
import Language.Drasil.Symbol (HasSymbol, Symbol)

import Data.Containers.ListUtils (nubOrd)
import qualified Data.Set as Set

-- | Used in 'Ch' constructor to determine the state of a term
-- (can record whether something is in plural form, a singular term, or in short form).
data SentenceStyle = PluralTerm
                   | TermStyle
                   | ShortStyle -- TODO: Short plural?

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
  -- This allows Sentences to hold plural forms of 'NamedIdea's.
  Ch    :: SentenceStyle -> TermCapitalization -> UID -> Sentence
  -- | A branch of Ch dedicated to SymbolStyle only.
  SyCh  :: UID -> Sentence
  -- | Converts a unit symbol into a usable Sentence form.
  Sy    :: USymb -> Sentence
  -- | Directly embeds a 'NP'
  NP    :: NP -> Sentence
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

-- | Helpers for extracting references -----------------------------------------

-- | Generic traverse of all positions that could lead to /symbolic/ 'UID's from 'Sentence's.
getUIDs :: Sentence -> [UID]
getUIDs (Ch ShortStyle _ _) = []
getUIDs (Ch TermStyle _ _)  = []
getUIDs (Ch PluralTerm _ _) = []
getUIDs (SyCh a)            = [a]
getUIDs Sy {}               = []
getUIDs NP {}               = []
getUIDs S {}                = []
getUIDs P {}                = []
getUIDs Ref {}              = []
getUIDs Percent             = []
getUIDs ((:+:) a b)         = getUIDs a ++ getUIDs b
getUIDs (Quote a)           = getUIDs a
getUIDs (E a)               = meNames a
getUIDs EmptyS              = []

-- | Generic traverse of all positions that could lead to /symbolic/ and /abbreviated/ 'UID's from 'Sentence's
-- but doesn't go into expressions.
getUIDshort :: Sentence -> [UID]
getUIDshort (Ch ShortStyle _ a) = [a]
getUIDshort (Ch TermStyle _ _)  = []
getUIDshort (Ch PluralTerm _ _) = []
getUIDshort SyCh {}             = []
getUIDshort Sy {}               = []
getUIDshort NP {}               = []
getUIDshort S {}                = []
getUIDshort Percent             = []
getUIDshort P {}                = []
getUIDshort Ref {}              = []
getUIDshort ((:+:) a b)         = getUIDshort a ++ getUIDshort b
getUIDshort (Quote a)           = getUIDshort a
getUIDshort E {}                = []
getUIDshort EmptyS              = []

-----------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above
-- | This is to collect /symbolic/ 'UID's that are printed out as a 'Symbol'.
sdep :: Sentence -> [UID]
sdep = nubOrd . getUIDs
{-# INLINE sdep #-}

-- This is to collect symbolic 'UID's that are printed out as an /abbreviation/.
shortdep :: Sentence -> [UID]
shortdep = nubOrd . getUIDshort
{-# INLINE shortdep #-}

-- | Generic traverse of all positions that could lead to /reference/ 'UID's from 'Sentence's.
lnames :: Sentence -> [UID]
lnames Ch {}       = []
lnames SyCh {}     = []
lnames Sy {}       = []
lnames NP {}       = []
lnames S {}        = []
lnames Percent     = []
lnames P {}        = []
lnames (Ref a _ _) = [a]
lnames ((:+:) a b) = lnames a ++ lnames b
lnames Quote {}    = []
lnames E {}        = []
lnames EmptyS      = []
{-# INLINE lnames #-}

-- | Get /reference/ 'UID's from 'Sentence's.
lnames' :: [Sentence] -> [UID]
lnames' = concatMap lnames
{-# INLINE lnames' #-}

sentenceRefs :: Sentence -> Set.Set UID
sentenceRefs sent = Set.fromList (lnames sent ++ sdep sent ++ shortdep sent)
{-# INLINE sentenceRefs #-}

instance HasChunkRefs Sentence where
  chunkRefs = sentenceRefs
  {-# INLINABLE chunkRefs #-}
