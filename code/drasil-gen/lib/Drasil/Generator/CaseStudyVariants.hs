{-# LANGUAGE QuasiQuotes #-}

-- | Case study variants.
--
-- Each case study is expected to generate files in a specific pattern that the
-- main `code/Makefile` expects for (a) testing and (b) website deployment.
module Drasil.Generator.CaseStudyVariants
  ( caseStudyMainSRS,
    caseStudyMainSRSWCode,
    caseStudyMainSRSWCodeZooWLsnPlan,
    caseStudyMainDrasilWebsite,
  )
where

import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Drasil.DocumentLanguage.Notebook (LsnDesc)
import Drasil.SRSDocument (SRSDecl)
import Drasil.System (DrasilWebsite, LessonPlan, SmithEtAlSRS)
import Language.Drasil (Document)
import Language.Drasil.Code (Choices)

import Drasil.Generator.Composed (exportSmithEtAlSrsWCode, exportSmithEtAlSrsWCodeZoo)
import Drasil.Generator.SRS (exportSmithEtAlSrs)
import Drasil.Generator.LessonPlan (exportLessonPlan)
import Drasil.Generator.Website (exportWebsite)

-- | Internal: Set system locale encoding to UTF-8.
setSystemLocale :: IO ()
setSystemLocale = setLocaleEncoding utf8

-- | A case study that only outputs an SRS in each of our supported variants.
caseStudyMainSRS :: SmithEtAlSRS -> SRSDecl -> String -> IO ()
caseStudyMainSRS syst srsDecl srsFileName = do
  setSystemLocale
  exportSmithEtAlSrs syst srsDecl srsFileName

-- | A case study that outputs both an SRS in each of our supported variants as
-- well as a single chosen software artifact in optionally many programming
-- languages.
caseStudyMainSRSWCode :: SmithEtAlSRS -> SRSDecl -> String -> Choices -> IO ()
caseStudyMainSRSWCode syst srsDecl srsFileName choices = do
  setSystemLocale
  exportSmithEtAlSrsWCode syst srsDecl srsFileName choices

-- | The same as 'caseStudyMainSRSWCode', except it also produces a
-- JupyterNotebook-based lesson plan.
caseStudyMainSRSWCodeZooWLsnPlan :: SmithEtAlSRS -> SRSDecl -> String
  -> [Choices] -> LessonPlan -> LsnDesc -> String -> IO ()
caseStudyMainSRSWCodeZooWLsnPlan syst srsDecl srsFileName  choices plan nbDecl
  lsnFileName = do
  setSystemLocale
  exportSmithEtAlSrsWCodeZoo syst srsDecl srsFileName choices
  exportLessonPlan plan nbDecl lsnFileName

-- | The Drasil website binary is expected to build a `Website/HTML/` folder
-- containing the actual website artifacts (`index.html` and `index.css`).
caseStudyMainDrasilWebsite :: DrasilWebsite -> Document -> IO ()
caseStudyMainDrasilWebsite syst websiteDoc = do
  setSystemLocale
  exportWebsite syst websiteDoc "index"
