{-# Language TemplateHaskell #-}
-- | Contains the common idea type and respective constructors.
-- SHOULD BE DEPRECATED. Some of the uses (for program name) should have
-- its own chunk type. Others need examined.
module Language.Drasil.Chunk.CommonIdea (
  -- * Common Idea datatype
  CI,
  -- * Constructors
  commonIdeaWithDict,
  -- * Functions
  prependAbrv
) where

import Control.Lens (makeLenses, (^.), view)

import Drasil.Database (UID, HasUID(uid), HasChunkRefs(..))
import qualified Data.Set as Set
import Utils.Drasil (repUnd)

import Language.Drasil.Chunk.NamedIdea (IdeaDict, nc)
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
 CommonIdea(abrv), ConceptDomain(cdom))
import Language.Drasil.NounPhrase.Core (NP)

-- | The common idea (with 'NounPhrase') data type. It must have a 'UID',
-- 'NounPhrase' for its term, an abbreviation ('String'), and a domain (['UID']).
-- It is similar to 'IdeaDict' and 'IdeaDict' in the sense that these are for things worth naming,
-- but this type also carries an abbreviation and related domains of knowledge.
--
-- Ex. The term "Operating System" has the abbreviation "OS" and comes from the domain of computer science.
data CI = CI { _nc' :: IdeaDict, _ab :: String, cdom' :: [UID]}
makeLenses ''CI

instance HasChunkRefs CI where
  chunkRefs c = Set.unions
    [ chunkRefs (c ^. nc')
    , Set.fromList (cdom c)
    ]
  {-# INLINABLE chunkRefs #-}

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

-- | The commonIdeaWithDict smart constructor requires a chunk id ('String'), a
-- term ('NP'), an abbreviation ('String'), and a
-- list of 'IdeaDict' (should be domains).
-- Note: should be polymorphic in 'IdeaDict', but currently causes issues with
-- ambiguous type variables, punting for now.
commonIdeaWithDict :: String -> NP -> String -> [IdeaDict] -> CI
commonIdeaWithDict x y z = CI (nc x y) z . map (^.uid)

-- | Prepends the abbreviation from a 'CommonIdea' to a 'String'.
prependAbrv :: CommonIdea c => c -> String -> String
prependAbrv c s = abrv c ++ (':' : repUnd s)
