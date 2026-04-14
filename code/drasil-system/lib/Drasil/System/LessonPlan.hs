module Drasil.System.LessonPlan (
  LessonPlan,
  mkLessonPlan,
  lsnPlanRefs
) where

import Control.Lens (makeLenses, (^.))
import qualified Data.Map.Strict as M

import Drasil.Database (UID, uid)
import Language.Drasil (Reference)

import Drasil.System.Core

data LessonPlan = LP {
  _sm :: SystemMeta,
  _lsnPlanRefs :: M.Map UID Reference
}
makeLenses ''LessonPlan

instance HasSystemMeta LessonPlan where
  systemMeta = sm

mkLessonPlan :: SystemMeta -> [Reference] -> LessonPlan
mkLessonPlan m rs = LP m refs
  where
    refs = M.fromList $ map (\r -> (r ^. uid, r)) rs
