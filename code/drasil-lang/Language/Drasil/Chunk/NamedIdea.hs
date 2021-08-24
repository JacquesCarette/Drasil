{-# LANGUAGE TemplateHaskell #-}
-- | The lowest level of chunks in Drasil. It all starts with an identifier and a term.
module Language.Drasil.Chunk.NamedIdea (
  -- * Chunk Types
  NamedChunk, IdeaDict,
  -- * Constructors
  nc, ncUID, nw, mkIdea, mkIdeaUID) where

import Language.Drasil.UID (UID)
import qualified Language.Drasil.UID.Core as UID (uid)
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA))
import Control.Lens ((^.), makeLenses)

import Language.Drasil.NounPhrase (NP)

-- === DATA TYPES/INSTANCES === --
-- | Used for anything worth naming. Note that a 'NamedChunk' does not have an acronym/abbreviation
-- as that's a 'CommonIdea', which has its own representation. Contains
-- a 'UID' and a term that we can capitalize or pluralize ('NP').
--
-- Ex. Anything worth naming must start out somewhere. Before we can assign equations
-- and values and symbols to something like the arm of a pendulum, we must first give it a name. 
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

-- TODO: Add in function to check UIDs (see #2788).
-- TODO: Any contructor that takes in a UID should be built off of this one so that
-- the UID may be checked by the first TODO.
-- | 'NamedChunk' constructor, takes a 'String' for its 'UID' and a term.
nc :: String -> NP -> NamedChunk
nc s = NC (UID.uid s)

-- | Similar to 'nc', but takes in the 'UID' in the form of a 'UID' rather than a 'String'.
ncUID :: UID -> NP -> NamedChunk
ncUID = NC

-- Don't export the record accessors.
-- | 'IdeaDict' is the canonical dictionary associated to an 'Idea'.
-- Contains a 'NamedChunk' that could have an abbreviation ('Maybe' 'String').
--
-- Ex. The project name "Double Pendulum" may have the abbreviation "DblPendulum".
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
-- an abbreviation in the form of 'Maybe' 'String'.
mkIdea :: String -> NP -> Maybe String -> IdeaDict
mkIdea s np' = IdeaDict (nc s np')

-- | Same as 'mkIdea' but takes a 'UID' rather than a 'String'.
mkIdeaUID :: UID -> NP -> Maybe String -> IdeaDict
mkIdeaUID s np' = IdeaDict (ncUID s np')

-- | Historical name: nw comes from 'named wrapped' from when
-- 'NamedIdea' exported 'getA' (now in 'Idea'). But there are
-- no more wrappers, instead we have explicit dictionaries. Unwraps
-- an 'Idea' and places its 'UID' and 'NP' into an 'IdeaDict' with
-- 'Nothing' for an abbreviation.
nw :: Idea c => c -> IdeaDict
nw c = IdeaDict (NC (c^.uid) (c^.term)) (getA c)