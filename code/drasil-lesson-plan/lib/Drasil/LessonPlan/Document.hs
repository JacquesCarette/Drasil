module Drasil.LessonPlan.Document (
  -- * Lesson Plan
  LsnDesc, LsnChapter(..),
  -- ** Chapter Types
  -- ** Constructors
  intro, learnObj, caseProb, example, appendix, review, reference, summary
) where

import Language.Drasil
import Language.Drasil.Document

import qualified Drasil.Metadata.Documentation as Doc (caseProb, introduction,
  learnObj, review, summary, example, appendix, reference)

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

-- * 'LsnDesc' 'Section' eliminators

-- | Section constructors for the 'LsnDesc' sections.
intro, learnObj, review, caseProb, summary, appendix, reference, example :: [Contents] -> [Section] -> Section
intro     = mkLsnSec  "Intro"      Doc.introduction
learnObj  = mkLsnSec' "LearnObj"   Doc.learnObj
review    = mkLsnSec  "Review"     Doc.review
caseProb  = mkLsnSec  "CaseProb"   Doc.caseProb
example   = mkLsnSec  "Example"    Doc.example
summary   = mkLsnSec  "Summary"    Doc.summary
appendix  = mkLsnSec  "Appendix"   Doc.appendix
reference = mkLsnSec' "References" Doc.reference

-- | Internal: Create a section of the lesson plan. Title is singular.
mkLsnSec :: Idea c => String -> c -> [Contents] -> [Section] -> Section
mkLsnSec r c cs ss = section t cs ss (makeSecRef r t)
  where t = titleize c

-- | Internal: Create a section of the lesson plan. Title is made plural.
mkLsnSec' :: Idea c => String -> c -> [Contents] -> [Section] -> Section
mkLsnSec' r c cs ss = section t cs ss (makeSecRef r t)
  where t = titleize' c
