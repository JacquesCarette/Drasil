{-# Language TemplateHaskell #-}
-- | Hack in physical system descriptions for the time being. We need to remove these doc
-- concepts from the back-end and put them at the Example level (ie. in 
-- Drasil.DocumentationLanguage.X
module Language.Drasil.Chunk.PhysSystDesc
  ( PhysSystDesc
  , pSysDes
  , psd, psd'
  , refAddr
  ) where

import Language.Drasil.Classes (HasUID(uid), HasAttributes(attributes), HasShortName(shortname))
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.RefTypes (RefAdd)
import Language.Drasil.Chunk.Attribute (shortname')

import Control.Lens (makeLenses, (^.))

data PhysSystDesc = PSD
          { _did        :: String
          , pSysDes     :: Sentence
          , _refAddr    :: RefAdd
          , _attribs    :: Attributes 
          }

makeLenses ''PhysSystDesc

instance HasUID        PhysSystDesc where uid = did
instance HasAttributes PhysSystDesc where attributes = attribs
instance HasShortName  PhysSystDesc where shortname p = shortname' $ S $ p ^. refAddr
instance Eq            PhysSystDesc where a == b = a ^. uid == b ^. uid
  
-- | PhysSystDesc smart constructor (has no explicit 'Attributes')
psd :: String -> Sentence -> RefAdd -> PhysSystDesc
psd i g r = PSD i g r []

-- | PhysSystDesc smart constructor (with explicit 'Attributes')
psd' :: String -> Sentence -> RefAdd -> Attributes -> PhysSystDesc
psd' = PSD
