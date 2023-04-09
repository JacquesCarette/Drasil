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
newtype Intro = IntrodProg [Contents]

-- ** Learning Objectives
newtype LearnObj = LrnObjProg [Contents]

-- ** Review Chapter
newtype Review = ReviewProg [Contents]

-- ** A Case Problem
newtype CaseProb = CaseProbProg [Contents]

-- ** Examples of the lesson
newtype Example = ExampleProg [Contents]
  
-- ** Summary
newtype Smmry = SmmryProg [Contents]

-- ** Appendix
newtype Apndx = ApndxProg [Contents]

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

    introd (IntrodProg con) = pure $ IntrodProg con 
    lrnObj (LrnObjProg con) = pure $ LrnObjProg con 
    rvw (ReviewProg con) = pure $ ReviewProg con
    csProb (CaseProbProg con) = pure $ CaseProbProg con 
    exmp (ExampleProg con) = pure $ ExampleProg con
    smry (SmmryProg con) = pure $ SmmryProg con 
    aps (ApndxProg con) = pure $ ApndxProg con
  mkPlate b = DLPlate (b lsnChap) (b intro) (b learnObj) (b review) 
    (b caseProb) (b example) (b smmry) (b apndx)