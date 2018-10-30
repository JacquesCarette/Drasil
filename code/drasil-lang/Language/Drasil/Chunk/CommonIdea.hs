{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.CommonIdea (CI, commonIdea , getAcc) where

import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA), CommonIdea(abrv))
import Language.Drasil.Sentence (Sentence(S))
import Language.Drasil.NounPhrase (NP)

import Control.Lens (makeLenses, view)

-- | The common idea (with nounPhrase) data type. It must have a 
-- 'NounPhrase' for its 'term', and must have an abbreviation.
data CI = CI { _cid :: UID, _ni :: NP, _ab :: String}
makeLenses ''CI

instance HasUID     CI where uid = cid
instance NamedIdea  CI where term = ni
instance Idea       CI where getA = Just . view ab
instance CommonIdea CI where abrv = view ab
  
-- | The commonIdea smart constructor requires a chunk id, 
-- term (of type 'NP'), and abbreviation (as a string)
commonIdea :: String -> NP -> String -> CI
commonIdea = CI

getAcc :: CI -> Sentence
getAcc = S . abrv
