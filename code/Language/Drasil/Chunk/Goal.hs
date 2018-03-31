{-# Language TemplateHaskell #-}
-- | Hack in goal statements for the time being. We need to remove these doc
-- concepts from the back-end and put them at the Example level (ie. in 
-- Drasil.DocumentationLanguage.X
module Language.Drasil.Chunk.Goal
  ( Goal
  , goal
  , gs, gs'
  , refAddr
  ) where

import Language.Drasil.Chunk
import Language.Drasil.Chunk.Attribute
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

instance Chunk Goal where uid = gid
instance HasAttributes Goal where attributes = attribs
instance Eq Goal where 
  a == b = a ^. uid == b ^. uid
  
-- | Goal smart constructor (has no explicit 'Attributes')
gs :: String -> Sentence -> RefAdd -> Goal
gs i g r = GS i g r []

-- | Goal smart constructor (with explicit 'Attributes')
gs' :: String -> Sentence -> RefAdd -> Attributes -> Goal
gs' = GS
