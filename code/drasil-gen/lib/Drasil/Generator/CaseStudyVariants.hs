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

import Control.Lens ((^.))
import Data.Char (toLower)
import Data.Maybe (maybeToList)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Drasil.FileHandling (FileLayout, OverwritePolicy(..), directory, localPath, ps,
  writeFiles)
import Drasil.LessonPlan (LsnDesc)
import Drasil.SRSDocument (SRSDecl, mkDoc)
import Drasil.System (DrasilWebsite, LessonPlan, SmithEtAlSRS, programName)
import Language.Drasil.Code (Choices)
import qualified Language.Drasil.Sentence.Combinators as S

import Drasil.Generator.ChunkDump (buildDebugData)
import Drasil.Generator.Code (genCode, genCodeZoo)
import Drasil.Generator.LessonPlan (genJupyterLessonPlan)
import Drasil.Generator.SRS (genSmithEtAlSrs)
import Drasil.Generator.SRS.TypeCheck (typeCheckSI)
import Drasil.Generator.Website (genWebsite)

-- | Internal: Set system locale encoding to UTF-8.
setSystemLocale :: IO ()
setSystemLocale = setLocaleEncoding utf8

-- | Internal: The `build/` subfolder the Makefile expects each case study will
-- build in (other than the website).
caseStudyBuildFolder :: SmithEtAlSRS -> String
caseStudyBuildFolder = map toLower . (^. programName)

-- | Internal: Generate documents and construct the SRS directory layout
-- structure (and debug data) for an example.
writeSmithEtAlSrs :: SmithEtAlSRS -> SRSDecl -> String -> IO ([FileLayout], SmithEtAlSRS, String)
writeSmithEtAlSrs syst srsDecl srsFileName = do
  let exampleName = caseStudyBuildFolder syst
      (srs, syst') = mkDoc syst srsDecl S.forT
  mDbgData <- buildDebugData syst'
  typeCheckSI syst' -- FIXME: This should be done on `System` creation *or* chunk creation!
  let dbgData = maybeToList mDbgData
      layout = dbgData ++ genSmithEtAlSrs syst' srs srsFileName
  pure (layout, syst', exampleName)

-- | A case study that only outputs an SRS in each of our supported variants.
caseStudyMainSRS :: SmithEtAlSRS -> SRSDecl -> String -> IO ()
caseStudyMainSRS syst srsDecl srsFileName = do
  setSystemLocale
  (docLayouts, _, exampleName) <- writeSmithEtAlSrs syst srsDecl srsFileName
  writeFiles OverwriteAllowed localPath $ directory [ps|{exampleName}|] docLayouts

-- | A case study that outputs both an SRS in each of our supported variants as
-- well as a single chosen software artifact in optionally many programming
-- languages.
caseStudyMainSRSWCode :: SmithEtAlSRS -> SRSDecl -> String -> Choices -> IO ()
caseStudyMainSRSWCode syst srsDecl srsFileName choices = do
  setSystemLocale
  (docLayouts, syst', exampleName) <- writeSmithEtAlSrs syst srsDecl srsFileName
  srcLayout <- genCode syst' choices
  writeFiles OverwriteAllowed localPath $ directory [ps|{exampleName}|] $ srcLayout : docLayouts

-- | The same as 'caseStudyMainSRSWCode', except it also produces a
-- JupyterNotebook-based lesson plan.
caseStudyMainSRSWCodeZooWLsnPlan :: SmithEtAlSRS -> SRSDecl -> String
  -> [Choices] -> LessonPlan -> LsnDesc -> String -> IO ()
caseStudyMainSRSWCodeZooWLsnPlan syst srsDecl srsFileName choices plan nbDecl lsnFileName = do
  setSystemLocale
  (docLayouts, syst', exampleName) <- writeSmithEtAlSrs syst srsDecl srsFileName
  zooLayouts <- genCodeZoo syst' choices
  let lessonLayout = directory [ps|Lesson|] [genJupyterLessonPlan plan nbDecl lsnFileName]
      layout = directory [ps|{exampleName}|] $ lessonLayout : docLayouts ++ zooLayouts
  writeFiles OverwriteAllowed localPath layout

-- | The Drasil website binary is expected to build a `Website/HTML/` folder
-- containing the actual website artifacts (`index.html` and `index.css`).
caseStudyMainDrasilWebsite :: DrasilWebsite -> IO ()
caseStudyMainDrasilWebsite dw = do
  setSystemLocale
  writeFiles OverwriteAllowed localPath $
    directory [ps|website|] $ genWebsite dw
