{-# LANGUAGE GADTs #-}
module Language.Drasil.NounPhrase 
  ( NounPhrase(..)
  , NP
  , ProperNoun
  , CommonNoun
  , np
  , pn, pn', pn''
  , cn, cn', cn'', cn'''
  )where

import Data.Char (toUpper)
--Linguistically, nounphrase might not be the best name (yet!), but once
-- it is fleshed out and/or we do more with it, it will likely be a good fit

--Using String for now, as it will allow us to add these to sentence 
-- very easily. If we need other options we can always change it.
class NounPhrase n where
  phrase :: n -> String -- ex. "the quick brown fox"
  plural :: n -> String -- ex. "the quick brown foxes" 
    --Could replace plural string with a function.
  sentenceCase :: n -> (String -> String) --Use a function to determine case.
    --Should this be replaced with a data type instead?
    --example of sentenceCase: "The quick brown fox" 
    --Name taken from English system of capitalization, but it might be
    --a bit confusing.
    
data NP where --Wrapper
  NP :: NounPhrase n => n -> NP

data ProperNoun where
  PN :: String -> String -> ProperNoun
  
instance NounPhrase ProperNoun where
  phrase (PN n _) = n
  plural (PN _ p) = p
  sentenceCase _ = id
  
data CommonNoun where
  CN :: String -> String -> (String -> String) -> CommonNoun

instance NounPhrase CommonNoun where
  phrase (CN n _ _) = n
  plural (CN _ p _) = p
  sentenceCase (CN _ _ s) = s

-- ===Constructors=== --
np :: NounPhrase n => n -> NP
np = NP

pn,pn' :: String -> ProperNoun
pn  n = PN n (sPlur  n)
pn' n = PN n (esPlur n)

pn'' :: String -> String -> ProperNoun
pn'' = PN

cn, cn' :: String -> CommonNoun
cn  n = CN n (sPlur n) capFirst
cn' n = CN n (esPlur n) capFirst

cn'' :: String -> String -> CommonNoun
cn'' n p = CN n p capFirst

cn''' :: String -> String -> (String -> String) -> CommonNoun
cn''' = CN 

-- === Helpers === -- DO NOT EXPORT
sPlur, esPlur, capFirst, ity, ness, ion :: String -> String
sPlur    s = s ++ "s"
esPlur   s = s ++ "es"
capFirst [] = []
capFirst (s:ss) = toUpper s : ss

-- Maybe export these for use in irregular cases?
ity  s = init s ++ "ity"
ness s = init s ++ "ness"
ion  s = init s ++ "ion"