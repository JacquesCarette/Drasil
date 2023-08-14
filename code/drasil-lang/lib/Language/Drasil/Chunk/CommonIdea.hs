{-# Language TemplateHaskell #-}
-- | Contains the common idea type and respective constructors.
module Language.Drasil.Chunk.CommonIdea (
  -- * Common Idea datatype
  CI, 
  -- * Constructors
  commonIdea, commonIdeaWithDict,
  -- * Functions
  getAcc, prependAbrv) where

import Language.Drasil.Chunk.NamedIdea (IdeaDict, nc)
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
 CommonIdea(abrv), ConceptDomain(cdom))
import Language.Drasil.Misc (repUnd)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Sentence (Sentence(S))
import Language.Drasil.UID (UID, HasUID(uid))

import Control.Lens (makeLenses, (^.), view)

-- | The common idea (with 'NounPhrase') data type. It must have a 'UID',
-- 'NounPhrase' for its term, an abbreviation ('String'), and a domain (['UID']).
-- It is similar to 'IdeaDict' and 'IdeaDict' in the sense that these are for things worth naming,
-- but this type also carries an abbreviation and related domains of knowledge.
--
-- Ex. The term "Operating System" has the abbreviation "OS" and comes from the domain of computer science.
data CI = CI { _nc' :: IdeaDict, _ab :: String, cdom' :: [UID]}
makeLenses ''CI

-- | Finds 'UID' of the 'IdeaDict' used to make the 'CI'.
instance HasUID        CI where uid  = nc' . uid
-- | Finds term ('NP') of the 'IdeaDict' used to make the 'CI'.
instance NamedIdea     CI where term = nc' . term
-- | Finds the idea of a 'CI' (abbreviation).
instance Idea          CI where getA = Just . view ab
-- | Finds the idea of a 'CI' (abbreviation).
instance CommonIdea    CI where abrv = view ab
-- | Finds the domain of a 'CI'.
instance ConceptDomain CI where cdom = cdom'
  
-- | The commonIdea smart constructor requires a chunk id ('String'), a
-- term ('NP'), an abbreviation ('String'), and a domain (['UID']).
commonIdea :: String -> NP -> String -> [UID] -> CI
commonIdea s np = CI (nc s np)

-- | Similar to 'commonIdea', but takes a list of 'IdeaDict' (often a domain).
commonIdeaWithDict :: String -> NP -> String -> [IdeaDict] -> CI
commonIdeaWithDict x y z = commonIdea x y z . map (^.uid)

-- | Get abbreviation in 'Sentence' form from a 'CI'.
getAcc :: CI -> Sentence
getAcc = S . abrv

-- | Prepends the abbreviation from a 'CommonIdea' to a 'String'.
prependAbrv :: CommonIdea c => c -> String -> String
prependAbrv c s = abrv c ++ (':' : repUnd s)
