{-# Language TemplateHaskell #-}
-- | Hack in goal statements for the time being. We need to remove these doc
-- concepts from the back-end and put them at the Example level (ie. in 
-- Drasil.DocumentationLanguage.X
module Language.Drasil.Chunk.Goal
  ( Goal
  , mkGoal
  ) where

import Language.Drasil.UID (UID)
import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname))
import Language.Drasil.Spec (Sentence)

import Language.Drasil.Label.Core (Label)
import Language.Drasil.Classes (HasLabel(getLabel))


import Control.Lens (makeLenses, (^.))

data Goal = GS
          { _gid :: UID
          , __ :: Sentence -- The goal
          , _lbl :: Label
          }

makeLenses ''Goal

instance HasUID        Goal where uid = gid
instance Eq            Goal where a == b = a ^. uid == b ^. uid
instance HasLabel      Goal where getLabel = lbl
instance HasShortName  Goal where shortname = lbl . shortname

-- | Goal smart constructor
mkGoal :: String -> Sentence -> Label -> Goal
mkGoal = GS
