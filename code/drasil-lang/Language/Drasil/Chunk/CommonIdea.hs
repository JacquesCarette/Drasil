{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.CommonIdea
  (CI, commonIdea, getAcc, commonIdeaWithDict, prependAbrv) where

import Language.Drasil.Chunk.NamedIdea (IdeaDict)
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
 CommonIdea(abrv), ConceptDomain(cdom))
import Language.Drasil.Misc (repUnd)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Sentence (Sentence(S))
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses, (^.), view)

-- | The common idea (with nounPhrase) data type. It must have a 
-- 'NounPhrase' for its 'term', and must have an abbreviation.
data CI = CI { _cid :: UID, _ni :: NP, _ab :: String, cdom' :: [UID]}
makeLenses ''CI

instance HasUID        CI where uid  = cid
instance NamedIdea     CI where term = ni
instance Idea          CI where getA = Just . view ab
instance CommonIdea    CI where abrv = view ab
instance ConceptDomain CI where cdom = cdom'
  
-- | The commonIdea smart constructor requires a chunk id, 
-- term (of type 'NP'), and abbreviation (as a string)
commonIdea :: String -> NP -> String -> [UID] -> CI
commonIdea = CI

commonIdeaWithDict :: String -> NP -> String -> [IdeaDict] -> CI
commonIdeaWithDict x y z = CI x y z . map (^.uid)

getAcc :: CI -> Sentence
getAcc = S . abrv

prependAbrv :: CommonIdea c => c -> String -> String
prependAbrv c s = abrv c ++ (':' : repUnd s)
