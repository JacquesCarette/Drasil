module Language.Drasil.Chunk.CommonIdea
  ( CI, commonIdea
  , getAcc
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA), CommonIdea(abrv))
import Language.Drasil.Spec (Sentence(S))
import Language.Drasil.NounPhrase

-- | The common idea (with nounPhrase) data type. It must have a 
-- 'NounPhrase' for its 'term'.
data CI = CI String String NP 

instance HasUID CI     where uid f (CI a b c) = fmap (\x -> CI x b c) (f a)
instance NamedIdea CI  where term f (CI a b c) = fmap (\x -> CI a b x) (f c)
instance Idea CI       where getA (CI _ b _) = Just b
instance CommonIdea CI where abrv (CI _ b _) = b
  
-- | The commonIdea smart constructor requires a chunk id, 
-- term (of type 'NP'), and abbreviation (as a string)
commonIdea :: String -> NP -> String -> CI
commonIdea i t a = CI i a t

getAcc :: CI -> Sentence
getAcc = S . abrv
