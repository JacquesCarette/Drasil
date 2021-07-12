{-# Language TemplateHaskell #-}
module Language.Drasil.DecoratedReference where

import Language.Drasil.Sentence
import Language.Drasil.Reference
import Language.Drasil.Classes.Core (HasUID(..), HasRefAddress(..))
import Language.Drasil.Classes.Core2 (HasShortName(..))
import Control.Lens ((^.), makeLenses)


-- | For holding a 'Reference' that is decorated with extra information (ex. page numbers, equation sources, etc.).
data DecRef = DR {
    _rf :: Reference,
    refInfo :: RefInfo
}
makeLenses ''DecRef

-- | Equal if 'UID's are equal.
instance Eq            DecRef where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the 'UID' of a 'Reference'.
instance HasUID        DecRef where uid = rf . uid
-- | Finds the reference address contained in a 'Reference' (through a 'LblType').
instance HasRefAddress DecRef where getRefAdd (DR r _) = getRefAdd r
-- | Finds the shortname of the reference address used for the 'Reference'.
instance HasShortName  DecRef where shortname (DR r _) = shortname r

mkDecRef :: Reference -> RefInfo -> DecRef
mkDecRef = DR