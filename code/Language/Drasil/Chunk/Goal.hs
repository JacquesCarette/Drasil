{-# Language TemplateHaskell #-}
-- | Hack in goal statements for the time being. We need to remove these doc
-- concepts from the back-end and put them at the Example level (ie. in 
-- Drasil.DocumentationLanguage.X
module Language.Drasil.Chunk.Goal
  ( Goal
  , mkGoal
  ) where

import Language.Drasil.Classes (HasUID(uid), HasShortName(shortname))
import Language.Drasil.Spec (Sentence(..))
import Language.Drasil.Chunk.Attribute.ShortName

import Control.Lens (makeLenses, (^.))

data Goal = GS
          { _gid :: String
          , goal :: Sentence
          , _sn :: ShortNm
          }

makeLenses ''Goal

instance HasUID        Goal where uid = gid
--instance HasAttributes Goal where attributes = attribs
instance HasShortName  Goal where shortname = sn
instance Eq            Goal where a == b = a ^. uid == b ^. uid
  
-- | Goal smart constructor
mkGoal :: String -> Sentence -> ShortNm -> Goal
mkGoal = GS
