module Drasil.LessonPlan.Document (
  -- * Lesson Plan
  LsnDesc, LsnChapter(..),
  -- ** Chapter Types
) where

import Language.Drasil.Document

-- * Lesson Chapter Types

type LsnDesc = [LsnChapter]

data LsnChapter = Intro [Contents]
                | LearnObj [Contents]
                | Review [Contents] [Section]
                | CaseProb [Contents] [Section]
                | Example [Contents]
                | Smmry [Contents]
                | BibSec
                | Apndx [Contents]
