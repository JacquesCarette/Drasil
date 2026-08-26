{-# LANGUAGE TemplateHaskell #-}
{- HLINT ignore "Use newtype instead of data" -}
module Drasil.LessonPlan.Core (
  LessonPlan,
  mkLessonPlan
) where

import Control.Lens (makeLenses)

import Drasil.System (SystemMeta, HasSystemMeta(..))

-- | An abstract "lesson plan."
--
-- Please refer to [Ting-Yu's thesis](https://github.com/JacquesCarette/Drasil/blob/main/People/Ting-Yu/thesis.pdf)
-- for more information.
data LessonPlan = LP {
  _sm :: SystemMeta
}
makeLenses ''LessonPlan

instance HasSystemMeta LessonPlan where
  systemMeta = sm

-- | Build a 'LessonPlan'.
mkLessonPlan :: SystemMeta -> LessonPlan
mkLessonPlan = LP
