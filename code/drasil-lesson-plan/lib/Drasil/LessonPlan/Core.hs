{-# LANGUAGE TemplateHaskell #-}
module Drasil.LessonPlan.Core (
  LessonPlan,
  mkLessonPlan,
  lessonName,
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
  _lessonName :: String,
  _lsnPlanRefs :: M.Map UID Reference
}
makeLenses ''LessonPlan

instance HasSystemMeta LessonPlan where
  systemMeta = sm

-- | Build a 'LessonPlan'. The 'String' is used to derive an output folder
-- name for generated lesson artifacts (see 'caseStudyMainLsnPlan').
mkLessonPlan :: String -> SystemMeta -> [Reference] -> LessonPlan
mkLessonPlan nm m rs = LP m nm refs
  where
    refs = M.fromList $ map (\r -> (r ^. uid, r)) rs
