{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.CommonIdea
  (CI, commonIdea, getAcc, getAccStr, commonIdeaWithDict, prependAbrv) where

import Language.Drasil.Chunk.NamedIdea (IdeaDict)
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
 CommonIdea(abrv), ConceptDomain(cdom))
import Language.Drasil.Misc (repUnd)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Sentence (Sentence(S))
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses, (^.), view)

-- | The common idea (with 'NounPhrase') data type. It must have a 'UID',
-- 'NounPhrase' for its term, an abbreviation ('String'), and a domain (['UID']).
data CI = CI { _cid :: UID, _ni :: NP, _ab :: String, cdom' :: [UID]}
makeLenses ''CI

instance HasUID        CI where uid  = cid
-- ^ Finds 'UID' of 'CI'.
instance NamedIdea     CI where term = ni
-- ^ Finds term ('NP') of 'CI'.
instance Idea          CI where getA = Just . view ab
-- ^ Finds idea of 'CI' (abbreviation).
instance CommonIdea    CI where abrv = view ab
-- ^ Finds idea of 'CI' (abbreviation).
instance ConceptDomain CI where cdom = cdom'
-- ^ Finds domain of 'CI'.
  
-- | The commonIdea smart constructor requires a chunk id ('UID'), a
-- term ('NP'), an abbreviation ('String'), and a domain (['UID']).
commonIdea :: String -> NP -> String -> [UID] -> CI
commonIdea = CI

-- | Similar to 'commonIdea', but takes a list of 'IdeaDict' (often a domain).
commonIdeaWithDict :: String -> NP -> String -> [IdeaDict] -> CI
commonIdeaWithDict x y z = CI x y z . map (^.uid)

-- | Get abbreviation in 'Sentence' form from a 'CI'.
getAcc :: CI -> Sentence
getAcc = S . abrv

-- | Get abbreviation in 'String' form from a 'CI'.
getAccStr :: CI -> String
getAccStr = abrv

-- | Prepends the abbreviation from a 'CommonIdea' to a 'String'.
prependAbrv :: CommonIdea c => c -> String -> String
prependAbrv c s = abrv c ++ (':' : repUnd s)
