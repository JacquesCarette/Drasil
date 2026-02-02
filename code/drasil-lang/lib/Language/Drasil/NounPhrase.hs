module Language.Drasil.NounPhrase (
  -- * Types
  NounPhrase(..), NP,
  -- * Phrase Accessors
  atStartNP, atStartNP', titleizeNP, titleizeNP',
  -- * Constructors
  -- ** Common Noun Constructors
  cn, cn', cn'', cn''', cnICES, cnIES, cnIP, cnIS, cnIrr, cnUM,
  -- ** Proper Noun Constructors
  pn, pn', pn'', pn''', pnIrr,
  -- ** Noun Phrase Constructors
  nounPhrase, nounPhrase', nounPhrase'', nounPhraseSP, nounPhraseSent,
  -- * Combinators
  compoundPhrase,
  compoundPhrase', compoundPhrase'', compoundPhrase''', compoundPhraseP1,
  surroundNPStruct,
  -- * Re-exported Types
  CapitalizationRule(..), PluralRule(..), NPStruct(..)
  ) where

import Data.Char (isLatin1, isLetter, toLower, toUpper)

import Language.Drasil.NounPhrase.Core -- uses whole module

--Linguistically, nounphrase might not be the best name (yet!), but once
-- it is fleshed out and/or we do more with it, it will likely be a good fit

class NounPhrase n where
  -- | Retrieves singular form of term. Ex. "the quick brown fox".
  phraseNP :: n -> NPStruct
  -- | Retrieves plural form of term. Ex. "the quick brown foxes".
  pluralNP :: n -> PluralForm
    --Could replace plural string with a function.
  -- | Retrieves the singular form and applies a captalization
  -- rule (usually capitalizes the first word) to produce a 'NPStruct.
  -- Ex. "The quick brown fox".
  sentenceCase :: n -> (NP -> NPStruct) -> Capitalization
    --Should this be replaced with a data type instead?
    --Data types should use functions to determine capitalization based
    -- on rules.
  -- | Retrieves the singular form and applies a captalization
  -- rule (usually capitalizes all words) to produce a 'NPStruct.
  -- Ex. "The Quick Brown Fox".
  titleCase :: n -> (NP -> NPStruct) -> Capitalization

-- | Type synonym for 'NPStruct.
type Capitalization = NPStruct
-- | Type synonym for 'String'.
type PluralString   = String

-- | Defines NP as a NounPhrase.
-- Default capitalization rules for proper and common nouns
-- are 'CapFirst' for sentence case and 'CapWords' for title case.
-- Also accepts a 'Phrase' where the capitalization case may be specified.
instance NounPhrase NP where
  phraseNP (ProperNoun n _)           = S n
  phraseNP (CommonNoun n _ _)         = S n
  phraseNP (Phrase n _ _ _)           = n
  pluralNP n@(ProperNoun _ p)         = sPlur (phraseNP n) p
  pluralNP n@(CommonNoun _ p _)       = sPlur (phraseNP n) p
  pluralNP (Phrase _ p _ _)           = p
  sentenceCase   (ProperNoun n _)   _ = S n
  sentenceCase n@(CommonNoun _ _ r) f = cap (f n) r
  sentenceCase n@(Phrase _ _ r _)   f = cap (f n) r
  titleCase n@ProperNoun {}         _ = phraseNP n
  titleCase n@CommonNoun {}         f = cap (f n) CapWords
  titleCase n@(Phrase _ _ _ r)      f = cap (f n) r

-- ===Constructors=== --
-- | Constructs a Proper Noun, it is always capitalized as written.
pn, pn', pn'', pn''' :: String -> NP
-- | Self plural.
pn    n = ProperNoun n SelfPlur
-- | Plural form simply adds "s" (ex. Henderson -> Hendersons).
pn'   n = ProperNoun n AddS
-- | Plural form adds "e".
pn''  n = ProperNoun n AddE
-- | Plural form adds "es" (ex. Bush -> Bushes).
pn''' n = ProperNoun n AddES

-- | Constructs a 'ProperNoun' with a custom plural rule (using 'IrregPlur' from 'PluralRule').
-- First argument is the String representing the noun, second is the rule.
pnIrr :: String -> PluralRule -> NP
pnIrr = ProperNoun

-- | Constructs a common noun which capitalizes the first letter of the first word
-- at the beginning of a sentence.
cn, cn', cn'', cn''' :: String -> NP
-- | Self plural.
cn    n = CommonNoun n SelfPlur CapFirst
-- | Plural form simply adds "s" (ex. dog -> dogs).
cn'   n = CommonNoun n AddS CapFirst
-- | Plural form adds "e" (ex. formula -> formulae).
cn''  n = CommonNoun n AddE CapFirst
-- | Plural form adds "es" (ex. bush -> bushes).
cn''' n = CommonNoun n AddES CapFirst

-- | Constructs a common noun that pluralizes by dropping the last letter and adding an "ies"
-- ending (ex. body -> bodies).
cnIES :: String -> NP
cnIES n = CommonNoun n (IrregPlur (\x -> init x ++ "ies")) CapFirst

--FIXME: Shouldn't this just be drop one and add "ces"?
-- | Construct a common noun that pluralizes by dropping the last two letters and adding an
-- "ices" ending (ex. matrix -> matrices).
cnICES :: String -> NP
cnICES n = CommonNoun n (IrregPlur (\x -> init (init x) ++ "ices")) CapFirst

-- | Constructs a common noun that pluralizes by dropping the last two letters and adding
-- "es" (ex. analysis -> analyses).
cnIS :: String -> NP
cnIS n = CommonNoun n (IrregPlur (\x -> init (init x) ++ "es")) CapFirst

-- | Constructs a common noun that pluralizes by dropping the last two letters and adding "a"
-- (ex. datum -> data).
cnUM :: String -> NP
cnUM n = CommonNoun n (IrregPlur (\x -> init (init x) ++ "a")) CapFirst

-- | Constructs a common noun that allows you to specify the pluralization rule
-- (as in 'pnIrr').
cnIP :: String -> PluralRule -> NP
cnIP n p = CommonNoun n p CapFirst

-- | Common noun that allows you to specify both the pluralization rule and the
-- capitalization rule for sentence case (if the noun is used at the beginning
-- of a sentence).
cnIrr :: String -> PluralRule -> CapitalizationRule -> NP
cnIrr = CommonNoun

-- | Creates a 'NP' with a given singular and plural form (as 'String's) that capitalizes the first
-- letter of the first word for sentence case.
nounPhrase :: String -> PluralString -> NP
nounPhrase s p = Phrase (S s) (S p) CapFirst CapWords

-- | Similar to 'nounPhrase', but takes a specified capitalization rule for the sentence case.
nounPhrase' :: String -> PluralString -> CapitalizationRule -> NP
nounPhrase' s p c = Phrase (S s) (S p) c CapWords

-- | Custom noun phrase constructor that takes a singular form ('NPStruct), plural form ('NPStruct),
-- sentence case capitalization rule, and title case capitalization rule.
nounPhrase'' :: NPStruct -> PluralForm -> CapitalizationRule -> CapitalizationRule -> NP
nounPhrase'' = Phrase

-- | For things that should not be pluralized (or are self-plural). Works like 'nounPhrase', but with
-- only the first argument.
nounPhraseSP :: String -> NP
nounPhraseSP s = Phrase (S s) (S s) CapFirst CapWords

-- | Similar to nounPhrase, except it only accepts one 'NPStruct.
-- Plural case is just 'AddS'.
nounPhraseSent :: NPStruct -> NP
nounPhraseSent s = Phrase s (sPlur s AddS) CapFirst CapWords

-- | Combine two noun phrases. The singular form becomes 'phrase' from t1 followed
-- by 'phrase' of t2. The plural becomes 'phrase' of t1 followed by 'plural' of t2.
-- Uses standard 'CapFirst' sentence case and 'CapWords' title case.
-- For example: @compoundPhrase system constraint@ will have singular form
-- "system constraint" and plural "system constraints".
compoundPhrase :: (NounPhrase a, NounPhrase b) => a -> b -> NP
compoundPhrase t1 t2 = Phrase
  (phraseNP t1 :+: phraseNP t2) (phraseNP t1 :+: pluralNP t2) CapFirst CapWords

-- | Similar to 'compoundPhrase', but the sentence case is the same
-- as the title case ('CapWords').
compoundPhrase' :: NP -> NP -> NP
compoundPhrase' t1 t2 = Phrase
  (phraseNP t1 :+: phraseNP t2) (phraseNP t1 :+: pluralNP t2) CapWords CapWords

-- | Similar to 'compoundPhrase'', but accepts two functions that will be used to
-- construct the plural form. For example,
-- @compoundPhrase'' plural phrase system constraint@ would have the plural
-- form "systems constraint".
compoundPhrase'' :: (NP -> NPStruct) -> (NP -> NPStruct) -> NP -> NP -> NP
compoundPhrase'' f1 f2 t1 t2 = Phrase
  (phraseNP t1 :+: phraseNP t2) (f1 t1 :+: f2 t2) CapWords CapWords

--More primes might not be wanted but fixes two issues
-- pluralization problem with software requirements specification (Documentation.hs)
-- SWHS program not being about to use a compound to create the IdeaDict
-- | Similar to 'compoundPhrase', but used when you need a special function applied
-- to the first term of both singular and pluralcases (eg. short or plural).
compoundPhrase''' :: (NP -> NPStruct) -> NP -> NP -> NP
compoundPhrase''' f1 t1 t2 = Phrase
  (f1 t1 :+: phraseNP t2) (f1 t1 :+: pluralNP t2) CapFirst CapWords

--For Data.Drasil.Documentation
-- | Similar to 'compoundPhrase', but pluralizes the first 'NP' for both singular and plural cases.
compoundPhraseP1 :: NP -> NP -> NP
compoundPhraseP1 = compoundPhrase''' pluralNP

-- === Helpers ===
-- | Helper function for getting the sentence case of a noun phrase.
atStartNP, atStartNP' :: NounPhrase n => n -> Capitalization
-- | Singular sentence case.
atStartNP  n = sentenceCase n phraseNP
-- | Plural sentence case.
atStartNP' n = sentenceCase n pluralNP

-- | Helper function for getting the title case of a noun phrase.
titleizeNP, titleizeNP' :: NounPhrase n => n -> Capitalization
-- | Singular title case.
titleizeNP  n = titleCase n phraseNP
-- | Plural title case.
titleizeNP' n = titleCase n pluralNP

-- DO NOT EXPORT --
-- | Pluralization helper function.
sPlur :: NPStruct -> PluralRule -> NPStruct
sPlur (S s) AddS = S (s ++ "s")
sPlur (S s) AddE = S (s ++ "e")
sPlur s@(S _) AddES = sPlur (sPlur s AddE) AddS
sPlur s@(S _) SelfPlur = s
sPlur (S sts) (IrregPlur f) = S $ f sts --Custom pluralization
sPlur (a :+: b) pt = a :+: sPlur b pt
sPlur (a :-: b) pt = a :-: sPlur b pt
sPlur a _ = S "MISSING PLURAL FOR:" :+: a

-- | Capitalization helper function given a noun phrase.
cap :: NPStruct -> CapitalizationRule -> NPStruct
cap _ (Replace s) = s
cap s CapNothing = s
cap (S [])     CapFirst = S [] -- ignore this
cap (S (s:ss)) CapFirst = S (toUpper s : ss)
cap (S s)      CapWords = capString s capFirstWord capWords
cap (P symb :+: x) CapFirst = P symb :+: x -- TODO: See why the Table of Symbols uses the CapWords case instead of CapFirst for items of the form:
cap (P symb :+: x) CapWords = P symb :+: x -- "x-component". Instead, it displays as "x-Component". Using a temp fix for now by ignoring everything after a P symbol.
cap (s1 :+: s2) CapWords = cap s1 CapWords :+: capTail s2
cap (s1 :+: s2) CapFirst = cap s1 CapFirst :+: s2
cap (P symb :-: x) CapFirst = P symb :-: x -- TODO: See why the Table of Symbols uses the CapWords case instead of CapFirst for items of the form:
cap (P symb :-: x) CapWords = P symb :-: x -- "x-component". Instead, it displays as "x-Component". Using a temp fix for now by ignoring everything after a P symbol.
cap (s1 :-: s2) CapWords = cap s1 CapWords :-: capTail s2
cap (s1 :-: s2) CapFirst = cap s1 CapFirst :-: s2
cap (P p) _ = P p

-- | Helper for 'cap' and for capitalizing the end of a 'NPStruct (assumes 'CapWords').
capTail :: NPStruct -> NPStruct
capTail (S s) = capString s capWords capWords
capTail (P symb :+: b) = P symb :+: b
capTail (a :+: b) = capTail a :+: capTail b
capTail (P symb :-: b) = P symb :-: b
capTail (a :-: b) = capTail a :-: capTail b
capTail (P p) = P p

-- | Helper for capitalizing a string.
capString :: String -> (String -> String) -> (String -> String) -> NPStruct
capString s f g = S . findHyph g . unwords $ process (words s)
  where
    process (x:xs) = f x : map g xs
    process []     = []

-- | Finds hyphens in a 'String' and applies capitalization to words after a hyphen.
findHyph :: (String -> String) -> String -> String
findHyph _ "" = ""
findHyph _ [x] = [x]
findHyph f (x:xs)
  | x == '-'  = '-' : findHyph f (f xs)
  | otherwise = x : findHyph f xs

-- | Capitalize first word of a 'String'. Does not ignore prepositions, articles, or conjunctions (intended for beginning of a phrase/sentence).
capFirstWord :: String -> String
capFirstWord "" = ""
capFirstWord w@(c:cs)
  | not (isLetter c) = w
  | not (isLatin1 c) = w
  | otherwise        = toUpper c : cs

-- | Capitalize all words of a 'String' (unless they are prepositions, articles, or conjunctions).
capWords :: String -> String
capWords "" = ""
capWords w@(c:cs)
  | not (isLetter c)   = w
  | not (isLatin1 c)   = w
  | w `elem` doNotCaps = toLower c : cs
  | otherwise          = toUpper c : cs

-- | Words that should not be capitalized in a title (prepositions, articles, or conjunctions).
doNotCaps :: [String]
doNotCaps = ["a", "an", "the", "at", "by", "for", "in", "of",
  "on", "to", "up", "and", "as", "but", "or", "nor"] --Ref http://grammar.yourdictionary.com

surroundNPStruct :: String -> String -> NPStruct -> NPStruct
surroundNPStruct l r (S s)       = S $ l ++ s ++ r
surroundNPStruct l r (s1 :+: s2) = surroundNPStruct l "" s1 :+: surroundNPStruct "" r s2
surroundNPStruct l r (s1 :-: s2) = surroundNPStruct l "" s1 :-: surroundNPStruct "" r s2
surroundNPStruct l r (P p)       = S l :-: P p :-: S r
