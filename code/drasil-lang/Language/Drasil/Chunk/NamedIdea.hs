{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.NamedIdea (NamedChunk, nc, IdeaDict, nw, mkIdea) where

import Language.Drasil.UID (UID)
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA))
import Control.Lens ((^.), makeLenses)

import Language.Drasil.NounPhrase (NP)

-- === DATA TYPES/INSTANCES === --
-- | Note that a 'NamedChunk' does not have an acronym/abbreviation
-- as that's a 'CommonIdea', which has its own representation. Contains
-- a 'UID' and a term ('NP').
data NamedChunk = NC {_uu :: UID, _np :: NP}
makeLenses ''NamedChunk

-- | Equal if 'UID's are equal.
instance Eq        NamedChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds the 'UID' of the 'NamedChunk'.
instance HasUID    NamedChunk where uid = uu
-- | Finds the term ('NP') of the 'NamedChunk'.
instance NamedIdea NamedChunk where term = np
-- | Finds the idea of a 'NamedChunk' (always 'Nothing').
instance Idea      NamedChunk where getA _ = Nothing
  
-- | 'NamedChunk' constructor, takes a 'UID' and a term.
nc :: UID -> NP -> NamedChunk
nc = NC

-- | 'IdeaDict' is the canonical dictionary associated to 'Idea'.
-- Contains a 'NamedChunk' and maybe an abbreviation ('String').
-- Don't export the record accessors.
data IdeaDict = IdeaDict { _nc' :: NamedChunk, mabbr :: Maybe String }
makeLenses ''IdeaDict

-- | Equal if 'UID's are equal.
instance Eq        IdeaDict where a == b = a ^. uid == b ^. uid
-- | Finds the 'UID' of the 'NamedChunk' used to make the 'IdeaDict'.
instance HasUID    IdeaDict where uid = nc' . uid
-- | Finds the term ('NP') of the 'NamedChunk' used to make the 'IdeaDict'.
instance NamedIdea IdeaDict where term = nc' . term
-- | Finds the abbreviation of the 'IdeaDict'.
instance Idea      IdeaDict where getA = mabbr
  
-- | 'IdeaDict' constructor, takes a 'UID', 'NP', and 
-- an abbreviation in the form of 'Maybe' 'String'
mkIdea :: UID -> NP -> Maybe String -> IdeaDict
mkIdea s np' = IdeaDict (nc s np')

-- | Historical name: nw comes from 'named wrapped' from when
-- 'NamedIdea' exported 'getA' (now in 'Idea'). But there are
-- no more wrappers, instead we have explicit dictionaries. Unwraps
-- an 'Idea' and places its 'UID' and 'NP' into an 'IdeaDict' with
-- 'Nothing' for an abbreviation.
nw :: Idea c => c -> IdeaDict
nw c = IdeaDict (NC (c^.uid) (c^.term)) (getA c)
