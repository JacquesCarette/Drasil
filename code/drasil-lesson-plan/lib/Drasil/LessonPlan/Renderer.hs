{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
-- | Document language for lesson plan notebooks.
module Drasil.LessonPlan.Renderer (
  Options(..),
  render
) where

import Control.Lens ((^.))

import Drasil.Database (ChunkDB)
import Drasil.FileHandling
import Language.Drasil hiding (Options)
import Language.Drasil.Document (Section, Document(Notebook), Contents(UlC),
  ulcc, RawContent(Bib), section, makeSecRef)
import Drasil.System (HasSystemMeta(..), Render(..))
import Drasil.Metadata.Documentation (notebook)
import qualified Drasil.Metadata.Documentation as Doc (caseProb, introduction,
  learnObj, review, summary, example, appendix, reference)

import Drasil.LessonPlan.Core (LessonPlan, lsnPlanRefs)
import Drasil.LessonPlan.Document (LsnDesc, LsnChapter(..))
import Drasil.LessonPlan.ExtractBib (extractBib)
import Language.Drasil.Printers
import Language.Drasil.Printing.Import

-- FIXME: Need to rename this to something more LessonPlan-focused.
data Options = Options {
  lsnDesc :: LsnDesc,
  -- FIXME: `titleComb` seems a bit odd. We only use `S.forT`. Two things:
  --
  -- 1. We generate `Notebook for Projectile Motion Lesson` as the title for the
  -- projectile motion lesson plan. This should probably just be "Projectile
  -- Motion Lesson".
  -- 2. The title should probably just directly come from the system name.
  titleComb :: CI -> CI -> Sentence,
  lsnFileName :: String
}

instance Render LessonPlan Options where
  -- | Renders a 'LessonPlan' using a 'LsnDesc' (a description of the document
  -- contents and organization) and a title combinator merging "notebook" with the
  -- name of the 'LessonPlan'.
  render plan Options{..} =
      [file [ps|{lsnFileName}.ipynb|] $ genJupyterLessonPlan pd]
    where
      -- FIXME: We can clean this up using another `instance Render ? ? where`
      -- here. This might mean we have the wrong design for `Render`. We might
      -- need a 3rd type parameter for the output type of the 'rendering'
      -- action.
      nm = notebook `titleComb` (plan ^. sysName)
      as = foldlList Comma List $ map (S . fullName) $ plan ^. authors
      nb = Notebook nm as $ mkSections (plan ^. systemdb) lsnDesc
      -- FIXME: `piSys` is an "options" constructor. Here is another opportunity
      -- to create a `Render` instance.
      printSetting = piSys (plan ^. systemdb) (plan ^. lsnPlanRefs) Equational Engineering
      pd = makeDocument printSetting nb

-- | Helper for creating the notebook sections.
mkSections :: ChunkDB -> LsnDesc -> [Section]
mkSections db dd = map doit dd
  where
    bib = [UlC $ ulcc (Bib $ extractBib db dd)]
    doit :: LsnChapter -> Section
    doit (Intro i)        = mkLsnSec  "Intro"      Doc.introduction i   []
    doit (LearnObj lo)    = mkLsnSec' "LearnObj"   Doc.learnObj     lo  []
    doit (Review r ss)    = mkLsnSec  "Review"     Doc.review       r   ss
    doit (CaseProb cp ss) = mkLsnSec  "CaseProb"   Doc.caseProb     cp  ss
    doit (Example e)      = mkLsnSec  "Example"    Doc.example      e   []
    doit (Smmry s)        = mkLsnSec  "Summary"    Doc.summary      s   []
    doit BibSec           = mkLsnSec' "References" Doc.reference    bib []
    doit (Apndx a)        = mkLsnSec  "Appendix"   Doc.appendix     a   []

-- | Internal: Create a section of the lesson plan. Title is singular.
mkLsnSec :: Idea c => String -> c -> [Contents] -> [Section] -> Section
mkLsnSec r c cs ss = section t cs ss (makeSecRef r t)
  where t = titleize c

-- | Internal: Create a section of the lesson plan. Title is made plural.
mkLsnSec' :: Idea c => String -> c -> [Contents] -> [Section] -> Section
mkLsnSec' r c cs ss = section t cs ss (makeSecRef r t)
  where t = titleize' c
