{-# LANGUAGE GADTs #-}
-- | Lesson plan notebook chapter types.
module Drasil.DocumentLanguage.Notebook.Core where

import Data.Generics.Multiplate (Multiplate(multiplate, mkPlate))
import Language.Drasil

-- * Lesson Chapter Types

type LsnDesc = [LsnChapter]

data LsnChapter = Intro Intro
                | LearnObj LearnObj
                | Review Review
                | CaseProb CaseProb
                | Example Example
                | Smmry Smmry
                | BibSec
                | Apndx Apndx

-- TODO: Work on detail structure of Lesson Plan

-- ** Introduction
data Intro = IntrodProg [Contents] [Section]

-- ** Learning Objectives
data LearnObj = LrnObjProg [Contents] [Section]

-- ** Review Chapter
data Review = ReviewProg [Contents] [Section]

-- ** A Case Problem
data CaseProb = CaseProbProg [Contents] [Section]

-- ** Examples of the lesson
data Example = ExampleProg [Contents] [Section]

-- ** Summary
data Smmry = SmmryProg [Contents] [Section]

-- ** Appendix
data Apndx = ApndxProg [Contents] [Section]

-- * Multiplate Definition and Type

data DLPlate f = DLPlate {
  lsnChap :: LsnChapter -> f LsnChapter,
  intro :: Intro -> f Intro,
  learnObj :: LearnObj -> f LearnObj,
  review :: Review -> f Review,
  caseProb :: CaseProb -> f CaseProb,
  example :: Example -> f Example,
  smmry :: Smmry -> f Smmry,
  apndx :: Apndx -> f Apndx
}

instance Multiplate DLPlate where
  multiplate p = DLPlate lc introd lrnObj rvw csProb exmp smry aps where
    lc (Intro x) = Intro <$> intro p x
    lc (LearnObj x) = LearnObj <$> learnObj p x
    lc (Review x) = Review <$> review p x
    lc (CaseProb x) = CaseProb <$> caseProb p x
    lc (Example x) = Example <$> example p x
    lc (Smmry x) = Smmry <$> smmry p x
    lc (Apndx x) = Apndx <$> apndx p x
    lc BibSec = pure BibSec

    introd (IntrodProg con secs) = pure $ IntrodProg con secs
    lrnObj (LrnObjProg con secs) = pure $ LrnObjProg con secs
    rvw (ReviewProg con secs) = pure $ ReviewProg con secs
    csProb (CaseProbProg con secs) = pure $ CaseProbProg con secs
    exmp (ExampleProg con secs) = pure $ ExampleProg con secs
    smry (SmmryProg con secs) = pure $ SmmryProg con secs
    aps (ApndxProg con secs) = pure $ ApndxProg con secs
  mkPlate b = DLPlate (b lsnChap) (b intro) (b learnObj) (b review)
    (b caseProb) (b example) (b smmry) (b apndx)
