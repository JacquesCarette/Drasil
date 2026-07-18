{-# LANGUAGE TemplateHaskell #-}
module Drasil.LessonPlan.Core (
  LessonPlan,
  mkLessonPlan,
  lsnPlanRefs
) where

import Control.Lens (makeLenses, (^.))
import qualified Data.Map.Strict as M

import Drasil.Database (UID, uid)
import Language.Drasil.Document (Reference)

import Drasil.System (SystemMeta, HasSystemMeta(..))

-- | An abstract "lesson plan."
--
-- Please refer to [Ting-Yu's thesis](https://github.com/JacquesCarette/Drasil/blob/main/People/Ting-Yu/thesis.pdf)
-- for more information.
data LessonPlan = LP {
  _sm :: SystemMeta,
  _lsnPlanRefs :: M.Map UID Reference
}
makeLenses ''LessonPlan

instance HasSystemMeta LessonPlan where
  systemMeta = sm

-- | Build a 'LessonPlan'.
mkLessonPlan :: SystemMeta -> [Reference] -> LessonPlan
mkLessonPlan m rs = LP m refs
  where
    refs = M.fromList $ map (\r -> (r ^. uid, r)) rs
