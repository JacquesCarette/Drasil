-- | Lesson plan notebook section declaration types and functions.
module Drasil.DocumentLanguage.Notebook.LsnDecl where

import qualified Drasil.DocumentLanguage.Notebook.Core as NB (LsnDesc, LsnChapter(..), 
  Intro(..), LearnObj(..), Review(..), CaseProb(..), Example(..), Smmry(..), Apndx(..))

import System.Drasil (System)

-- * Types

-- | A Lesson Plan notebook declaration is made up of all necessary chapters ('LsnChapter's).
type LsnDecl  = [LsnChapter]

-- | Contains all the different chapters needed for a notebook lesson plan ('LsnDecl').
data LsnChapter = Intro NB.Intro
                | LearnObj NB.LearnObj
                | Review NB.Review
                | CaseProb NB.CaseProb
                | Example NB.Example
                | Smmry NB.Smmry
                | BibSec
                | Apndx NB.Apndx

-- * Functions

-- | Creates the lesson description (translates 'LsnDecl' into a more usable form for generating documents).
mkLsnDesc :: System -> LsnDecl -> NB.LsnDesc
mkLsnDesc _ = map sec where
  sec :: LsnChapter -> NB.LsnChapter
  sec (Intro i)     = NB.Intro i
  sec (LearnObj lo) = NB.LearnObj lo
  sec (Review r)    = NB.Review r  
  sec (CaseProb cp) = NB.CaseProb cp
  sec (Example e)   = NB.Example e  
  sec (Smmry s)     = NB.Smmry s
  sec BibSec        = NB.BibSec
  sec (Apndx a)     = NB.Apndx a
