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

-- TODO: Work on detail structure of Lesson Plan

-- * Section Constructors

-- | Section constructors for the lesson plan documents/jupyter notebooks.
-- should be fixed once we have a more concrete idea of the notebook structure
-- TODO: Remove [Section] since the structure of lesson plans should not be nested
-- maybe add a new type Chapter for lesson plans?
intro, learnObj, review, caseProb, summary, appendix, reference, example :: [Contents] -> [Section] -> Section
intro     cs ss = section (titleize Doc.introduction) cs ss introLabel
learnObj  cs ss = section (titleize' Doc.learnObj)    cs ss learnObjLabel
review    cs ss = section (titleize Doc.review)       cs ss reviewLabel
caseProb  cs ss = section (titleize Doc.caseProb)     cs ss caseProbLabel
example   cs ss = section (titleize Doc.example)      cs ss exampleLabel
summary   cs ss = section (titleize Doc.summary)      cs ss summaryLabel
appendix  cs ss = section (titleize Doc.appendix)     cs ss appendixLabel
reference cs ss = section (titleize' Doc.reference)   cs ss referenceLabel

-- * Section References

-- | Individual section reference labels. Used in creating example sections for the notebook.
introLabel, learnObjLabel, referenceLabel,
  reviewLabel, caseProbLabel, appendixLabel, summaryLabel, exampleLabel :: Reference
introLabel          = makeSecRef "Intro"            $ titleize Doc.introduction
learnObjLabel       = makeSecRef "LearnObj"         $ titleize' Doc.learnObj
referenceLabel      = makeSecRef "References"       $ titleize' Doc.reference
reviewLabel         = makeSecRef "Review"           $ titleize Doc.review
caseProbLabel       = makeSecRef "CaseProb"         $ titleize Doc.caseProb
appendixLabel       = makeSecRef "Appendix"         $ titleize Doc.appendix
summaryLabel        = makeSecRef "Summary"          $ titleize Doc.summary
exampleLabel        = makeSecRef "Example"          $ titleize Doc.example
