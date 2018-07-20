{-# Language TemplateHaskell #-}
-- | Hack in goal statements for the time being. We need to remove these doc
-- concepts from the back-end and put them at the Example level (ie. in 
-- Drasil.DocumentationLanguage.X
module Language.Drasil.Chunk.Goal
  ( Goal
  , mkGoal
  , refAddr
  ) where

import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid))
import Language.Drasil.Spec (Sentence)
import Language.Drasil.RefTypes (RefAdd)

import Language.Drasil.Chunk.ShortName (HasShortName(shortname))
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Classes (HasLabel(getLabel))


import Control.Lens (makeLenses, (^.))

data Goal = GS
          { _gid :: UID
          , __ :: Sentence -- The goal
          , _refAddr :: RefAdd
          , _lbl :: Label
          }

makeLenses ''Goal

instance HasUID        Goal where uid = gid
instance Eq            Goal where a == b = a ^. uid == b ^. uid
instance HasLabel      Goal where getLabel = lbl
instance HasShortName  Goal where shortname = lbl . shortname

-- | Goal smart constructor
mkGoal :: String -> Sentence -> RefAdd -> Label -> Goal
mkGoal = GS
