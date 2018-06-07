{-# Language TemplateHaskell #-}
-- | Hack in physical system descriptions for the time being. We need to remove these doc
-- concepts from the back-end and put them at the Example level (ie. in 
-- Drasil.DocumentationLanguage.X
module Language.Drasil.Chunk.PhysSystDesc
  ( PhysSystDesc
  , pSysDes
  , psd
  , refAddr
  ) where

import Language.Drasil.Classes (HasUID(uid))
import Language.Drasil.Spec (Sentence)
import Language.Drasil.RefTypes (RefAdd)
import Language.Drasil.Chunk.ShortName
import Control.Lens (makeLenses, (^.))

data PhysSystDesc = PSD
          { _did        :: String
          , pSysDes     :: Sentence
          , _refAddr    :: RefAdd
          }

makeLenses ''PhysSystDesc

instance HasUID        PhysSystDesc where uid = did
instance Eq            PhysSystDesc where a == b = a ^. uid == b ^. uid
instance HasShortName  PhysSystDesc where shortname p = shortname' $ p ^. refAddr

-- | PhysSystDesc smart constructor
psd :: String -> Sentence -> RefAdd -> PhysSystDesc
psd = PSD
