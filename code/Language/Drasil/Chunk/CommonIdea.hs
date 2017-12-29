module Language.Drasil.Chunk.CommonIdea
  ( CommonIdea(..)
  , CI, commonIdea
  , getAcc
  ) where

import Prelude hiding (id)

import Language.Drasil.Chunk (Chunk(id))
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Spec (Sentence(S))
import Language.Drasil.NounPhrase

-- | CommonIdea is a chunk that is a 'NamedIdea' with the additional
-- constraint that it __must__ have an abbreviation.
class NamedIdea c => CommonIdea c where
  -- | Introduces abrv which necessarily provides an abbreviation.
  abrv :: c -> String

-- | The common idea (with nounPhrase) data type. It must have a 
-- 'NounPhrase' for its 'term'.
data CI = CI String String NP 

instance Chunk CI where
  id f (CI a b c) = fmap (\x -> CI x b c) (f a)
instance NamedIdea CI where
  term f (CI a b c) = fmap (\x -> CI a b x) (f c)
  getA (CI _ b _) = Just b
instance CommonIdea CI where
  abrv (CI _ b _) = b
instance NounPhrase CI where
  phrase       (CI _ _ c) = phrase c
  plural       (CI _ _ c) = plural c
  sentenceCase (CI _ _ c) = sentenceCase c
  titleCase    (CI _ _ c) = titleCase c
  
-- | The commonIdea smart constructor requires a chunk id, 
-- term (of type 'NP'), and abbreviation (as a string)
commonIdea :: String -> NP -> String -> CI
commonIdea i t a = CI i a t

getAcc :: CI -> Sentence
getAcc = S . abrv

{-
-- | Similar to commonIdea, except the abbreviation is a Symbol
commonIdea' :: String -> NP -> Symbol -> CI
commonIdea' i t sy = CI i (P sy) t 

-- | Similar to commonIdea, except the abbreviation can be anything of type 'Sentence'
-- ('Symbol', 'String', 'Greek', etc.)
commonIdea'' :: String -> NP -> Sentence -> CI
commonIdea'' i t ab = CI i ab t
-}
