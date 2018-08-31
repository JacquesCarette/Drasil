{-# LANGUAGE GADTs #-}
module Language.Drasil.NounPhrase 
  ( NounPhrase(..)
  , NP
  , pn, pn', pn'', pn''', pnIrr
  , cn, cn', cn'', cn''', cnIP, cnIrr, cnIES, cnICES, cnIS, cnUM
  , nounPhrase, nounPhrase', nounPhrase'', nounPhraseSP, nounPhraseSent
  , compoundPhrase, compoundPhrase', compoundPhrase'', compoundPhrase''', compoundPhraseP1
  , at_start, at_start', titleize, titleize'
  -- re-export these
  , CapitalizationRule(..), PluralRule(..)
  ) where

import Data.Char (isLatin1, isLetter, toLower, toUpper)

import Language.Drasil.NounPhrase.Core -- uses whole module
import Language.Drasil.Spec (Sentence((:+:), S), (+:+))


--Linguistically, nounphrase might not be the best name (yet!), but once
-- it is fleshed out and/or we do more with it, it will likely be a good fit

class NounPhrase n where
  phrase :: n -> Sentence -- ex. "the quick brown fox"
  plural :: n -> PluralForm -- ex. "the quick brown foxes" 
    --Could replace plural string with a function.
  sentenceCase :: n -> (NP -> Sentence) -> Capitalization 
    --Should this be replaced with a data type instead?
    --example: "The quick brown fox" 
    --Data types should use functions to determine capitalization based
    -- on rules.
  titleCase :: n -> (NP -> Sentence) -> Capitalization

type Capitalization = Sentence  --Using type synonyms for clarity.
type PluralString   = String

instance NounPhrase NP where
  phrase (ProperNoun n _)             = S n
  phrase (CommonNoun n _ _)           = S n
  phrase (Phrase n _ _ _)             = n
  plural n@(ProperNoun _ p)           = sPlur (phrase n) p
  plural n@(CommonNoun _ p _)         = sPlur (phrase n) p
  plural (Phrase _ p _ _)             = p
  sentenceCase n@ProperNoun {}   _ = phrase n
  sentenceCase n@(CommonNoun _ _ r) f = cap (f n) r
  sentenceCase n@(Phrase _ _ r _)   f = cap (f n) r
  titleCase n@ProperNoun {}      _ = phrase n
  titleCase n@CommonNoun {}    f = cap (f n) CapWords
  titleCase n@(Phrase _ _ _ r)      f = cap (f n) r
  
-- ===Constructors=== --
-- | Construct a Proper Noun, it is always capitalized as written.
pn, pn', pn'', pn''' :: String -> NP
-- | Self plural
pn    n = ProperNoun n SelfPlur
-- | Plural form simply adds "s" (ex. Henderson -> Hendersons)
pn'   n = ProperNoun n AddS
-- | Plural form adds "e"
pn''  n = ProperNoun n AddE
-- | Plural form adds "es" (ex. Bush -> Bushes)
pn''' n = ProperNoun n AddES

-- | Construct a Proper Noun. Meant for use with IrregPlur from 'PluralRule'.
-- First argument is the String representing the noun, second is the rule.
pnIrr :: String -> PluralRule -> NP
pnIrr = ProperNoun

-- | Construct a common noun which capitalizes the first letter of the first word
-- at the beginning of a sentence.
cn, cn', cn'', cn''' :: String -> NP
-- | Self plural
cn    n = CommonNoun n SelfPlur CapFirst
-- | Plural form simply adds "s" (ex. dog -> dogs)
cn'   n = CommonNoun n AddS CapFirst
-- | Plural form adds "e" (ex. formula -> formulae)
cn''  n = CommonNoun n AddE CapFirst
-- | Plural form adds "es" (ex. bush -> bushes)
cn''' n = CommonNoun n AddES CapFirst

-- | Common noun that pluralizes by dropping the last letter and adding an "ies"
-- | ending (ex. body -> bodies)
cnIES :: String -> NP
cnIES n = CommonNoun n (IrregPlur (\x -> init x ++ "ies")) CapFirst

--FIXME: Shouldn't this just be drop one and add "ces"?
-- | Common noun that pluralizes by dropping the last two letters and adding an 
-- | "ices" ending (ex. matrix -> matrices)
cnICES :: String -> NP
cnICES n = CommonNoun n (IrregPlur (\x -> init (init x) ++ "ices")) CapFirst

-- | Common noun that pluralizes by dropping the last two letters and adding
-- "es" (ex. analysis -> analyses)
cnIS :: String -> NP
cnIS n = CommonNoun n (IrregPlur (\x -> init (init x) ++ "es")) CapFirst

-- | Common noun that pluralizes by dropping the last two letters and adding "a"
-- (ex. datum -> data)
cnUM :: String -> NP
cnUM n = CommonNoun n (IrregPlur (\x -> init (init x) ++ "a")) CapFirst

-- | Common noun that allows you to specify the pluralization rule 
-- (as in 'pnIrr')
cnIP :: String -> PluralRule -> NP
cnIP n p = CommonNoun n p CapFirst

-- | Common noun that allows you to specify both the pluralization rule and the
-- capitalization rule for sentence case (if the noun is used at the beginning
-- of a sentence)
cnIrr :: String -> PluralRule -> CapitalizationRule -> NP
cnIrr = CommonNoun 

-- | Noun phrase with a given singular and plural form that capitalizes the first
-- letter of the first word for sentence case
nounPhrase :: String -> PluralString -> NP
nounPhrase s p = Phrase (S s) (S p) CapFirst CapWords

-- | Similar to 'nounPhrase', but following a given capitalization rule for 
-- sentence case
nounPhrase' :: String -> PluralString -> CapitalizationRule -> NP
nounPhrase' s p c = Phrase (S s) (S p) c CapWords

-- | Custom noun phrase constructor that takes a singular form, plural form, 
-- sentence case capitalization rule, and title case capitalization rule
nounPhrase'' :: Sentence -> PluralForm -> CapitalizationRule -> CapitalizationRule -> NP
nounPhrase'' = Phrase

-- | For things that should not be pluralized. Works like 'nounPhrase', but with
-- only the first argument
nounPhraseSP :: String -> NP
nounPhraseSP s = Phrase (S s) (S s) CapFirst CapWords

-- | For Reuirements, Assumptions, LikelyChanges, etc. to allow for referencing.
nounPhraseSent :: Sentence -> NP
nounPhraseSent s = Phrase s s CapFirst CapWords

-- | Combine two noun phrases. The singular form becomes 'phrase' from t1 followed
-- by phrase of t2. The plural becomes phrase of t1 followed by plural of t2.
-- Uses standard CapFirst sentence case and CapWords title case.
-- For example: @compoundPhrase system constraint@ will have singular form
-- "system constraint" and plural "system constraints"
compoundPhrase :: (NounPhrase a, NounPhrase b) => a -> b -> NP
compoundPhrase t1 t2 = Phrase 
  (phrase t1 +:+ phrase t2) (phrase t1 +:+ plural t2) CapFirst CapWords
  
-- | Similar to 'compoundPhrase', but where the sentence case is the same
-- as the title case (CapWords).
compoundPhrase' :: NP -> NP -> NP
compoundPhrase' t1 t2 = Phrase
  (phrase t1 +:+ phrase t2) (phrase t1 +:+ plural t2) CapWords CapWords

-- | Similar to 'compoundPhrase\'', but which accepts functions to be used for
-- constructing the plural form. For example 
-- @compoundPhrase'' plural phrase system constraint@ would have the plural
-- form "systems constraint". 
compoundPhrase'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
compoundPhrase'' f1 f2 t1 t2 = Phrase
  (phrase t1 +:+ phrase t2) (f1 t1 +:+ f2 t2) CapWords CapWords

--More primes might not be wanted but fixes two issues
-- pluralization problem with software requirements specification (Documentation.hs)
-- SWHS program not being about to use a compound to create the NamedChunk
--Used when you need a special function apllied to the first term (eg. short or plural)
compoundPhrase''' :: (NP -> Sentence) -> NP -> NP -> NP
compoundPhrase''' f1 t1 t2 = Phrase 
  (f1 t1 +:+ phrase t2) (f1 t1 +:+ plural t2) CapFirst CapWords

--For Data.Drasil.Documentation
--Pluralizes the first word in two phrases
compoundPhraseP1 :: NP -> NP -> NP
compoundPhraseP1 = compoundPhrase''' plural

-- === Helpers === 
-- | Helper function for getting the sentence case of a noun phrase.
at_start, at_start' :: NounPhrase n => n -> Capitalization
-- | Singular sentence case.
at_start  n = sentenceCase n phrase
-- | Plural sentence case.
at_start' n = sentenceCase n plural

-- | Helper function for getting the title case of a noun phrase.
titleize, titleize' :: NounPhrase n => n -> Capitalization
-- | Singular title case.
titleize  n = titleCase n phrase
-- | Plural title case.
titleize' n = titleCase n plural

-- DO NOT EXPORT --                
-- | Pluralization helper function.
sPlur :: Sentence -> PluralRule -> Sentence
sPlur s@(S _) AddS = s :+: S "s"
sPlur s@(S _) AddE = s :+: S "e"
sPlur s@(S _) AddES = sPlur (sPlur s AddE) AddS
sPlur s@(S _) SelfPlur = s
sPlur (S sts) (IrregPlur f) = S $ f sts --Custom pluralization
sPlur (a :+: b) pt = a :+: sPlur b pt
sPlur a _ = S "MISSING PLURAL FOR:" +:+ a

-- | Capitalization helper function.
cap :: Sentence -> CapitalizationRule -> Sentence
cap _ (Replace s) = s
cap (S (s:ss))   CapFirst = S (toUpper s : ss)
cap (S s)        CapWords = S $ findNotCaps $ findHyph $ unwords 
  (map (\x -> toUpper (head x) : tail x) (words s))
cap (S s1 :+: S s2) r = cap (S (s1 ++ s2)) r
cap (s1 :+: s2 :+: s3)  CapWords = cap (s1 :+: s2) CapWords +:+ cap s3 CapWords
  --could change associativity of :+: instead?
cap (s1 :+: s2)  CapWords = cap s1 CapWords :+: cap s2 CapWords
cap (s1 :+: s2)  CapFirst = cap s1 CapFirst :+: s2
cap a _ = a

findHyph :: String -> String
findHyph "" = ""
findHyph s
      | [head s] == "-" = "-" ++ [toUpper (head(tail s))] ++ tail (tail s)
      | otherwise = head s : findHyph (tail s)

-- Finds words that should not be capitalized in a title and changes them back to lowercase
findNotCaps :: String -> String
findNotCaps "" = ""
findNotCaps s = unwords (head (words s) : map isNotCaps (tail $ words s))

isNotCaps :: String -> String
isNotCaps (c:cs)
    | not (isLetter c && isLatin1 c)  = toLower c : cs
    | (toLower c : cs) `elem` doNotCaps = toLower c : cs
isNotCaps s = s

doNotCaps :: [String]
doNotCaps = ["a", "an", "the", "at", "by", "for", "in", "of",
  "on", "to", "up", "and", "as", "but", "or", "nor"] --Ref http://grammar.yourdictionary.com
