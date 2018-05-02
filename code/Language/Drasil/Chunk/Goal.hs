{-# Language TemplateHaskell #-}
-- | Hack in goal statements for the time being. We need to remove these doc
-- concepts from the back-end and put them at the Example level (ie. in 
-- Drasil.DocumentationLanguage.X
module Language.Drasil.Chunk.Goal
  ( Goal
  , mkGoal
  , refAddr
  ) where

import Language.Drasil.Classes (HasUID(uid))
import Language.Drasil.Chunk.Attribute (HasAttributes(attributes))
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Spec (Sentence)
import Language.Drasil.RefTypes (RefAdd)

import Control.Lens (makeLenses, (^.))

data Goal = GS
          { _gid :: String
          , goal :: Sentence
          , _refAddr :: RefAdd
          , _attribs :: Attributes -- FIXME: I doubt this is necessary for these
                                   -- but included for consistency, and since every
                                   -- chunk should eventually have the capability
                                   -- for attributes.
          }

makeLenses ''Goal

instance HasUID Goal        where uid = gid
instance HasAttributes Goal where attributes = attribs
instance Eq Goal            where a == b = a ^. uid == b ^. uid
  
-- | Goal smart constructor (with explicit 'Attributes')
mkGoal :: String -> Sentence -> RefAdd -> Attributes -> Goal
mkGoal = GS
