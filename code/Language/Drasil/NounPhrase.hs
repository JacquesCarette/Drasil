{-# LANGUAGE GADTs #-}
module Language.Drasil.NounPhrase 
  ( NounPhrase(..)
  , NP
  , pn, pn', pn'', pn''', pnIrr
  , cn, cn', cn'', cn''', cnIP, cnIrr, cnIES, cnICES, cnIS, cnUM
  , nounPhrase, nounPhrase', nounPhrase'', nounPhraseSP
  , compoundPhrase, compoundPhrase', compoundPhrase''
  , at_start, at_start', titleize, titleize'
  , CapitalizationRule(..)
  , PluralRule(..)
  )where

import Data.Char (toUpper)
import Data.List (intersperse)
import Language.Drasil.Spec (Sentence(..), (+:+))
--Linguistically, nounphrase might not be the best name (yet!), but once
-- it is fleshed out and/or we do more with it, it will likely be a good fit

--Using String for now, as it will allow us to add these to sentence 
-- very easily. If we need other options we can always change it.
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
type PluralForm     = Sentence  -- These might change.
type PluralString   = String

data NP where
  ProperNoun :: String -> PluralRule -> NP
  CommonNoun :: String -> PluralRule -> CapitalizationRule -> NP
  Phrase     :: Sentence -> PluralForm -> CapitalizationRule -> CapitalizationRule -> NP
  --Phrase plurals can get very odd, so it seems best (for now) to encode
  --them directly. FIXME: If the singular/plural phrase has special (replace)
  --capitalization, one of the two cannot be capitalized right now.
  --The two capitalization rules are for sentenceCase / titleCase respectively
instance NounPhrase NP where
  phrase (ProperNoun n _)       = S n
  phrase (CommonNoun n _ _)     = S n
  phrase (Phrase n _ _ _)         = n
  plural n@(ProperNoun _ p)     = sPlur (phrase n) p
  plural n@(CommonNoun _ p _)   = sPlur (phrase n) p
  plural (Phrase _ p _ _)         = p
  sentenceCase n@(ProperNoun _ _)   _ = phrase n
  sentenceCase n@(CommonNoun _ _ r) f = cap (f n) r
  sentenceCase n@(Phrase _ _ r _)     f = cap (f n) r
  titleCase n@(ProperNoun _ _)      _ = phrase n
  titleCase n@(CommonNoun _ _ _)    f = cap (f n) CapWords
  titleCase n@(Phrase _ _ _ r)        f = cap (f n) r
  
-- ===Constructors=== --
pn, pn', pn'', pn''' :: String -> NP
pn    n = ProperNoun n SelfPlur
pn'   n = ProperNoun n AddS
pn''  n = ProperNoun n AddE
pn''' n = ProperNoun n AddES

pnIrr :: String -> PluralRule -> NP
pnIrr = ProperNoun

cn, cn', cn'', cn''' :: String -> NP
cn    n = CommonNoun n SelfPlur CapFirst
cn'   n = CommonNoun n AddS CapFirst
cn''  n = CommonNoun n AddE CapFirst
cn''' n = CommonNoun n AddES CapFirst

cnIES :: String -> NP
cnIES n = CommonNoun n (IrregPlur (\x -> init x ++ "ies")) CapFirst

cnICES :: String -> NP
cnICES n = CommonNoun n (IrregPlur (\x -> (init (init x)) ++ "ices")) CapFirst

cnIS :: String -> NP
cnIS n = CommonNoun n (IrregPlur (\x -> (init (init x)) ++ "es")) CapFirst

cnUM :: String -> NP
cnUM n = CommonNoun n (IrregPlur (\x -> (init (init x)) ++ "a")) CapFirst

cnIP :: String -> PluralRule -> NP
cnIP n p = CommonNoun n p CapFirst

cnIrr :: String -> PluralRule -> CapitalizationRule -> NP
cnIrr = CommonNoun 

nounPhrase :: String -> PluralString -> NP
nounPhrase s p = Phrase (S s) (S p) CapFirst CapWords

nounPhrase' :: String -> PluralString -> CapitalizationRule -> NP
nounPhrase' s p c = Phrase (S s) (S p) c CapWords

nounPhrase'' :: Sentence -> PluralForm -> CapitalizationRule -> CapitalizationRule -> NP
nounPhrase'' = Phrase

--For things that should not be pluralized.
nounPhraseSP :: String -> NP
nounPhraseSP s = Phrase (S s) (S s) CapFirst CapWords

compoundPhrase :: (NounPhrase a, NounPhrase b) => a -> b -> NP
compoundPhrase t1 t2 = Phrase 
  (phrase t1 +:+ phrase t2) (phrase t1 +:+ plural t2) CapFirst CapWords
  
compoundPhrase' :: NP -> NP -> NP
compoundPhrase' t1 t2 = Phrase
  (phrase t1 +:+ phrase t2) (phrase t1 +:+ plural t2) CapWords CapWords
  
compoundPhrase'' :: (NP -> Sentence) -> (NP -> Sentence) -> NP -> NP -> NP
compoundPhrase'' f1 f2 t1 t2 = Phrase
  (phrase t1 +:+ phrase t2) (f1 t1 +:+ f2 t2) CapWords CapWords
  


-- === Helpers === 

at_start, at_start' :: NounPhrase n => n -> Capitalization
at_start  n = sentenceCase n phrase
at_start' n = sentenceCase n plural

titleize, titleize' :: NounPhrase n => n -> Capitalization
titleize  n = titleCase n phrase
titleize' n = titleCase n plural

data CapitalizationRule = CapFirst
                        | CapWords
                        | Replace Sentence
data PluralRule = AddS
                | AddE
                | AddES
                | SelfPlur
                | IrregPlur (String -> String)

-- DO NOT EXPORT --                
sPlur :: Sentence -> PluralRule -> Sentence
sPlur s@(S _) AddS = s :+: S "s"
sPlur s@(S _) AddE = s :+: S "e"
sPlur s@(S _) AddES = sPlur (sPlur s AddE) AddS
sPlur s@(S _) SelfPlur = s
sPlur (S sts) (IrregPlur f) = S $ f sts --Custom pluralization
sPlur (a :+: b) pt = a :+: sPlur b pt
sPlur a _ = S "MISSING PLURAL FOR:" +:+ a

cap :: Sentence -> CapitalizationRule -> Sentence
cap _ (Replace s) = s
cap (S (s:ss))   CapFirst = S $ (toUpper s : ss)
cap (S s)        CapWords = S $ concat (intersperse " " 
  (map (\x -> (toUpper (head x) : (tail x))) (words s)))
cap ((S s1) :+: (S s2)) r = cap (S (s1++s2)) r
cap (s1 :+: s2 :+: s3)  r = cap (s1 :+: s2) r +:+ cap s3 r 
  --could change associativity of :+: instead?
cap (s1 :+: s2)  CapWords = cap s1 CapWords :+: cap s2 CapWords
cap (s1 :+: s2)  CapFirst = cap s1 CapFirst :+: s2
cap a _ = a

-- ity, ness, ion :: String -> String
-- Maybe export these for use in irregular cases?
-- ity  s = init s ++ "ity"
-- ness s = init s ++ "ness"
-- ion  s = init s ++ "ion"
