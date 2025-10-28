{-# LANGUAGE TemplateHaskell #-}
-- | The lowest level of chunks in Drasil. It all starts with an identifier and a term.
module Language.Drasil.Chunk.NamedIdea (
  -- * Type
  IdeaDict,
  -- * Classes
  NamedIdea(..), Idea(..),
  -- * Constructors
  nc, ncUID, nw, mkIdea, mkIdeaUID
) where

import Drasil.Database.Chunk (HasChunkRefs(..))
import Drasil.Database.UID (mkUid, UID, HasUID(..))
import Control.Lens ((^.), makeLenses)
import Control.Lens.Lens (Lens')
import qualified Data.Set as S

import Language.Drasil.NounPhrase.Core (CapitalizationRule(..), NP(..))
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Sentence.Extract (lnames, sdep, shortdep)

-- TODO: Why does a NamedIdea need a UID? It might need a UID to be registered in the chunk map.
-- | A NamedIdea is a 'term' that we've identified (has a 'UID') as 
-- being worthy of naming.
class HasUID c => NamedIdea c where
  -- | Lens to the term (a noun phrase).
  term :: Lens' c NP

-- | An 'Idea' is the combination of a 'NamedIdea' and a 'CommonIdea'.
-- In other words, it /may/ have an acronym/abbreviation.
class NamedIdea c => Idea c where
  -- | Gets the acronym/abbreviation.
  getA :: c -> Maybe String
  --Get Abbreviation/Acronym? These might need to be separated 
  --depending on contexts, but for now I don't see a problem with it.

-- === DATA TYPES/INSTANCES === --
-- TODO: Add in function to check UIDs (see #2788).
-- TODO: Any constructor that takes in a UID should be built off of this one so that
-- the UID may be checked by the first TODO.

-- | 'IdeaDict' constructor, takes a 'String' for its 'UID' and a term.
nc :: String -> NP -> IdeaDict
nc s np' = IdeaDict (mkUid s) np' Nothing

-- | Similar to 'nc', but takes in the 'UID' in the form of a 'UID' rather than a 'String'.
ncUID :: UID -> NP -> IdeaDict
ncUID u np' = IdeaDict u np' Nothing

-- Don't export the record accessors.
-- | 'IdeaDict' is the canonical dictionary associated to an 'Idea'.
-- Contains a 'UID' and a term that could have an abbreviation ('Maybe' 'String').
--
-- Ex. The project name "Double Pendulum" may have the abbreviation "DblPend".
data IdeaDict = IdeaDict {
  _uu :: UID,
  _np :: NP,
  mabbr :: Maybe String
}
makeLenses ''IdeaDict

instance HasChunkRefs IdeaDict where
  chunkRefs (IdeaDict _ np' _) = npRefs np'

-- | Equal if 'UID's are equal.
instance Eq        IdeaDict where a == b = a ^. uid == b ^. uid
-- | Finds the 'UID' of the 'IdeaDict' used to make the 'IdeaDict'.
instance HasUID    IdeaDict where uid = uu
-- | Finds the term ('NP') of the 'IdeaDict' used to make the 'IdeaDict'.
instance NamedIdea IdeaDict where term = np
-- | Finds the abbreviation of the 'IdeaDict'.
instance Idea      IdeaDict where getA = mabbr
  
-- | 'IdeaDict' constructor, takes a 'UID', 'NP', and 
-- an abbreviation in the form of 'Maybe' 'String'.
mkIdea :: String -> NP -> Maybe String -> IdeaDict
mkIdea s = IdeaDict (mkUid s)

-- | Same as 'mkIdea' but takes a 'UID' rather than a 'String'.
mkIdeaUID :: UID -> NP -> Maybe String -> IdeaDict
mkIdeaUID = IdeaDict

-- | Historical name: nw comes from 'named wrapped' from when
-- 'NamedIdea' exported 'getA' (now in 'Idea'). But there are
-- no more wrappers, instead we have explicit dictionaries. Unwraps
-- an 'Idea' and places its 'UID' and 'NP' into an 'IdeaDict' with
-- 'Nothing' for an abbreviation.
nw :: Idea c => c -> IdeaDict
nw c = IdeaDict (c ^. uid) (c ^. term) (getA c)

npRefs :: NP -> S.Set UID
npRefs (ProperNoun _ _)          = mempty
npRefs (CommonNoun _ _ capRule)  = capRuleRefs capRule
npRefs (Phrase sing plural c1 c2) =
  sentenceRefs sing `S.union`
  sentenceRefs plural `S.union`
  capRuleRefs c1 `S.union`
  capRuleRefs c2

capRuleRefs :: CapitalizationRule -> S.Set UID
capRuleRefs CapFirst     = mempty
capRuleRefs CapWords     = mempty
capRuleRefs (Replace s)  = sentenceRefs s

sentenceRefs :: Sentence -> S.Set UID
sentenceRefs s = S.fromList (lnames s ++ sdep s ++ shortdep s)
