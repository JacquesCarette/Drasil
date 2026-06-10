{-# LANGUAGE GADTs #-}
-- | Lesson plan notebook chapter types.
module Drasil.LessonPlan.Core (
  LsnDesc, LsnChapter(..), Intro(..), LearnObj(..), Review(..), CaseProb(..),
  Example(..), Smmry(..), Apndx(..)
) where

import Language.Drasil.Document (Contents, Section)

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
data Review = ReviewProg [Contents] [Section]

-- ** A Case Problem
data CaseProb = CaseProbProg [Contents] [Section]

-- ** Examples of the lesson
newtype Example = ExampleProg [Contents]

-- ** Summary
newtype Smmry = SmmryProg [Contents]

-- ** Appendix
newtype Apndx = ApndxProg [Contents]
