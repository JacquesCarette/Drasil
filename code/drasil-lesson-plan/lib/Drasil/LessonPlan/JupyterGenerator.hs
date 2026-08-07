{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
-- | Document language for lesson plan notebooks.
module Drasil.LessonPlan.JupyterGenerator (
  JupyterGenOptions(..)
) where

import Control.Lens ((^.))

import Drasil.Database (ChunkDB)
import Drasil.FileHandling
import Language.Drasil hiding (Options)
import Language.Drasil.Document (Section, Document(Notebook), Contents(UlC),
  ulcc, RawContent(Bib), section, makeSecRef)
import Drasil.System (HasSystemMeta(..), ToFiles(..), ProjectName, projName)
import Drasil.Metadata.Documentation (notebook)
import qualified Drasil.Metadata.Documentation as Doc (caseProb, introduction,
  learnObj, review, summary, example, appendix, reference)

import Drasil.LessonPlan.Core (LessonPlan)
import Drasil.LessonPlan.Document (LsnDesc, LsnChapter(..))
import Drasil.LessonPlan.ExtractBib (extractBib)
import Language.Drasil.Printers

-- | Single-file-generating Jupyter notebook rendering options.
data JupyterGenOptions = JupyterGenOptions {
  -- | Describe the organization of the final lesson plan.
  lsnDesc :: LsnDesc,
  -- | A title combinator that mixes 'Notebook' (passed as the first argument)
  -- with the title of the 'LessonPlan'. To be used as the /actual title/ used
  -- in the final generated document.
  titleComb :: CI -> ProjectName -> Sentence,
  -- | The name of the file to be outputted (no extension, @.ipynb@ is added
  -- later).
  fileName :: String
}

instance ToFiles LessonPlan JupyterGenOptions where
  -- | Realize a 'LessonPlan' as a single Jupyter notebook file.
  toFiles plan JupyterGenOptions{..} = files
    where
      -- Steps:

      -- 1. Transform `LessonPlan` into SDL (Semantic Document language).
      nm = notebook `titleComb` (plan ^. projName)
      as = foldlList Comma List $ map (S . fullName) $ plan ^. authors
      -- FIXME: These sections should be inserted into the ChunkDB but doing so
      -- (currently) creates a "duplicate chunk insertion" error /because/ the
      -- lesson plan is often initialized with `withCommonKnowledge` (from
      -- `drasil-gen`), which by default loads the sections presumed existent in
      -- an SRS. The `Section` duplicate: References.
      nb = Notebook nm as $ mkSections (plan ^. systemdb) lsnDesc

      -- 2. Transform SDL into TDL (Typesetting Document Language).
      printSetting = piSys (plan ^. systemdb) Equational Engineering
      pd = makeDocument printSetting nb

      -- 3. Transform TDL into `Prettyprinter.Doc`.
      doc = genJupyterLessonPlan pd

      -- 4. Produce final files (with `Prettyprinter.Doc` body).
      files = [file [ps|{fileName}.ipynb|] doc]

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
