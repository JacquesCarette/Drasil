{-# Language TemplateHaskell #-}
-- | Defines a type used to hold referencing information.
module Language.Drasil.Reference (
  -- * Type
  Reference(Reference),
  -- * Class
  HasReference(..),
  -- * Constructors
  ref, refS, namedRef, complexRef, namedComplexRef
) where

import Language.Drasil.Label.Type (LblType(..), HasRefAddress(..), IRefProg(..))
import Language.Drasil.ShortName (HasShortName(..), ShortName, getSentSN)
import Language.Drasil.Sentence (Sentence(Ref, EmptyS), RefInfo(..))
import Drasil.Database.UID (UID, HasUID(..))
import Drasil.Database.Chunk (HasChunkRefs(..))

import Control.Lens ((^.), makeLenses, Lens')
import qualified Data.Set as S

import Language.Drasil.Sentence.Extract (lnames, sdep, shortdep)

-- | A Reference contains the identifier ('UID'), a reference address ('LblType'),
-- a human-readable shortname ('ShortName'), and any extra information about the reference ('RefInfo').
data Reference = Reference
  { _ui :: UID
  ,  ra :: LblType
  ,  sn :: ShortName}
makeLenses ''Reference

-- | A class that contains a list of 'Reference's.
class HasReference c where
  getReferences :: Lens' c [Reference]

instance HasChunkRefs Reference where
  chunkRefs r = S.delete (r ^. uid) $
    lblTypeRefs (ra r) `S.union` sentenceRefs (getSentSN $ sn r)

-- | Equal if 'UID's are equal.
instance Eq            Reference where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the 'UID' of a 'Reference'.
instance HasUID        Reference where uid = ui
-- | Finds the reference address contained in a 'Reference' (through a 'LblType').
instance HasRefAddress Reference where getRefAdd = ra
-- | Finds the shortname of the reference address used for the 'Reference'.
instance HasShortName  Reference where shortname = sn
{--- Finds the reference information of a 'Reference'.
instance Referable Reference where
  refAdd r = r ^. ui
  renderRef = ra-}

-------------------------------

-- | Projector function that creates a 'Reference' from something 'Referable'.
ref :: (HasUID r, HasRefAddress r, HasShortName r) => r -> Reference
ref r = Reference (r ^. uid) (getRefAdd r) (shortname r)

-- Maybe just use r ^. uid without 'ref'?
-- | Takes the reference 'UID' and wraps it into a 'Sentence'.
refS :: (HasUID r, HasRefAddress r, HasShortName r) => r -> Sentence
refS r = namedRef r EmptyS

-- | Takes a 'Reference' with a name to be displayed and wraps it into a 'Sentence'.
-- Does not overwrite the shortname contained in the reference, but will only display as the given 'Sentence'.
namedRef :: (HasUID r, HasRefAddress r, HasShortName r) => r -> Sentence -> Sentence
namedRef r s = namedComplexRef r s None

-- | Takes a 'Reference' with additional display info. Uses the internal shortname for its display name.
complexRef :: (HasUID r, HasRefAddress r, HasShortName r) => r -> RefInfo -> Sentence
complexRef r = Ref (ref r ^. uid) EmptyS

-- | Takes a 'Reference' with a name to be displayed and any additional information and wraps it into a 'Sentence'.
-- Does not overwrite the shortname contained in the reference, but will only display as the given 'Sentence' along with the given 'RefInfo'.
namedComplexRef :: (HasUID r, HasRefAddress r, HasShortName r) => r -> Sentence -> RefInfo -> Sentence
namedComplexRef r = Ref (ref r ^. uid)

sentenceRefs :: Sentence -> S.Set UID
sentenceRefs s = S.fromList $ lnames s ++ sdep s ++ shortdep s

lblTypeRefs :: LblType -> S.Set UID
lblTypeRefs (RP prog _)     = iRefProgRefs prog
lblTypeRefs (Citation _)    = mempty
lblTypeRefs (URI _)         = mempty

iRefProgRefs :: IRefProg -> S.Set UID
iRefProgRefs (Deferred u)    = S.singleton u
iRefProgRefs (RS _)          = mempty
iRefProgRefs (RConcat a b)   = iRefProgRefs a `S.union` iRefProgRefs b
iRefProgRefs Name            = mempty
