{-# LANGUAGE TemplateHaskell #-}
-- | The lowest level of chunks in Drasil. It all starts with an identifier and a term.
module Language.Drasil.Chunk.NamedIdea (
  -- * Type
  IdeaDict,
  -- * Classes
  NamedIdea(..), Idea(..),
  -- * Constructors
  idea, idea'
) where

import Control.Lens ((^.), makeLenses, Lens')

import Drasil.Database (UID, HasUID(..), declareHasChunkRefs, Generically(..),
  IsChunk)
import Language.Drasil.NaturalLanguage.English.NounPhrase.Core (NP)
import Data.Typeable (Typeable)

-- | A NamedIdea is a 'term' that we've identified (has a 'UID') as being worthy
-- of naming.
class (Typeable c, IsChunk c) => NamedIdea c where
  -- | Lens to the term (an 'NP').
  term :: Lens' c NP

-- | An 'Idea' is the combination of a 'NamedIdea' and a 'CommonIdea'. In other
-- words, it /may/ have an acronym/abbreviation.
class NamedIdea c => Idea c where
  -- | Get the acronym/abbreviation.
  getA :: c -> Maybe String

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
declareHasChunkRefs ''IdeaDict
makeLenses ''IdeaDict

-- | Equal if 'UID's are equal.
instance Eq        IdeaDict where a == b = a ^. uid == b ^. uid
-- | Finds the 'UID' of the 'IdeaDict' used to make the 'IdeaDict'.
instance HasUID    IdeaDict where uid = uu
-- | Finds the term ('NP') of the 'IdeaDict' used to make the 'IdeaDict'.
instance NamedIdea IdeaDict where term = np
-- | Finds the abbreviation of the 'IdeaDict'.
instance Idea      IdeaDict where getA = mabbr

-- | Construct an 'IdeaDict' (/with/ an acronym/abbreviation).
idea ::
  -- | The 'UID'.
  UID ->
  -- | The 'term' being declared.
  NP ->
  -- | The 'term's acronym/abbreviation.
  String -> IdeaDict
idea u t accAbbr = IdeaDict u t (Just accAbbr)

-- | Construct an 'IdeaDict' (/without/ an acronym/abbreviation).
idea' ::
  -- | The 'UID'.
  UID ->
  -- | The 'term' being declared.
  NP -> IdeaDict
idea' u t = IdeaDict u t Nothing
