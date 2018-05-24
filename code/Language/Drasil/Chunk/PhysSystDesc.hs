{-# Language TemplateHaskell #-}
-- | Hack in physical system descriptions for the time being. We need to remove these doc
-- concepts from the back-end and put them at the Example level (ie. in 
-- Drasil.DocumentationLanguage.X
module Language.Drasil.Chunk.PhysSystDesc
  ( PhysSystDesc
  , pSysDes
  , psd
  ) where

import Language.Drasil.Classes (HasUID(uid), HasShortName(shortname))
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.Attribute.ShortName

import Control.Lens (makeLenses, (^.))

data PhysSystDesc = PSD
          { _did        :: String
          , pSysDes     :: Sentence
          --, _attribs    :: Attributes 
          , _sn          :: ShortNm
          }

makeLenses ''PhysSystDesc

instance HasUID        PhysSystDesc where uid = did
--instance HasAttributes PhysSystDesc where attributes = attribs
instance HasShortName  PhysSystDesc where shortname = sn
instance Eq            PhysSystDesc where a == b = a ^. uid == b ^. uid
  
-- | PhysSystDesc smart constructor
psd :: String -> Sentence -> ShortNm -> PhysSystDesc
psd = PSD
