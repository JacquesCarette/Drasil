{-# Language TemplateHaskell #-}
-- | Hack in physical system descriptions for the time being. We need to remove these doc
-- concepts from the back-end and put them at the Example level (ie. in 
-- Drasil.DocumentationLanguage.X
module Language.Drasil.Chunk.PhysSystDesc
  ( PhysSystDesc
  , pSysDes
  , psd
  ) where

import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid))
import Language.Drasil.Spec (Sentence)
import Language.Drasil.RefTypes (RefAdd)
import Language.Drasil.Chunk.ShortName (HasShortName(shortname), shortname')
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Classes (HasLabel(getLabel))

import Control.Lens (makeLenses, (^.))

data PhysSystDesc = PSD
          { _did        :: UID
          , pSysDes     :: Sentence
          , _lbl        :: Label
          }

makeLenses ''PhysSystDesc

instance HasUID        PhysSystDesc where uid = did
instance Eq            PhysSystDesc where a == b = a ^. uid == b ^. uid
instance HasLabel      PhysSystDesc where getLabel = lbl
instance HasShortName  PhysSystDesc where shortname = lbl . shortname

-- | PhysSystDesc smart constructor
psd :: String -> Sentence -> Label -> PhysSystDesc
psd = PSD
