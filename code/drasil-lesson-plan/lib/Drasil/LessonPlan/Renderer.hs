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
  -- FIXME: I don't think `LsnDesc` should be an option. It should probably be a
  -- field of `LessonPlan`.
  lsnDesc :: LsnDesc,
  -- FIXME: `titleComb` seems a bit odd. We only use `S.forT`. Two things:
  --
  -- 1. We generate `Notebook for Projectile Motion Lesson` as the title for the
  -- projectile motion lesson plan. This should probably just be "Projectile
  -- Motion Lesson".
  -- 2. The title should probably just directly come from the system name.
  titleComb :: CI -> CI -> Sentence,
  lsnFileName :: String
  -- FIXME: Output formats? This can be rendered as a literate notebook as well.
}

instance Render LessonPlan Options where
  -- | Renders a 'LessonPlan' using a 'LsnDesc' (a description of the document
  -- contents and organization) and a title combinator merging "notebook" with the
  -- name of the 'LessonPlan'.
  render plan Options{..} = files
    where
      -- Steps:

      -- 1. Transform `LessonPlan` into SDL (Semantic Document language).
      nm = notebook `titleComb` (plan ^. sysName)
      as = foldlList Comma List $ map (S . fullName) $ plan ^. authors
      nb = Notebook nm as $ mkSections (plan ^. systemdb) lsnDesc

      -- 2. Transform SDL into TDL (Typesetting Document Language).
      printSetting = piSys (plan ^. systemdb) (plan ^. lsnPlanRefs) Equational Engineering
      pd = makeDocument printSetting nb

      -- 3. Transform TDL into `Prettyprinter.Doc`.
      doc = genJupyterLessonPlan pd

      -- 4. Produce final files (with `Prettyprinter.Doc` body).
      files = [file [ps|{lsnFileName}.ipynb|] doc]

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
