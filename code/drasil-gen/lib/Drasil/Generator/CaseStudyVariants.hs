{-# LANGUAGE QuasiQuotes #-}
-- | Case study variants.
--
-- Each case study is expected to generate files in a specific pattern that the
-- main `code/Makefile` expects for (a) testing and (b) website deployment.
module Drasil.Generator.CaseStudyVariants
  ( caseStudyMainSRS,
    caseStudyMainSRSWCode,
    caseStudyMainSRSWCodeZoo,
  )
where

import Control.Lens ((^.))

import Drasil.FileHandling (FileLayout, OverwritePolicy(..), directory, localPath, ps,
  writeFiles)
import Drasil.SRS (SRSDecl, mkDoc, SmithEtAlSRS)
import Language.Drasil.Code (Choices)
import qualified Language.Drasil.Sentence.Combinators as S

import Drasil.Generator.Code (genCode, genCodeZoo)
import Drasil.Generator.SRS (genSmithEtAlSrs)
import Drasil.Generator.SRS.TypeCheck (typeCheckSI)
import Drasil.Generator.WriteSystem (setSystemLocale)
import Drasil.System (HasSystemMeta(..), HasProjectName(..))

-- | Internal: Generate documents and construct the SRS directory layout
-- structure (and debug data) for an example.
writeSmithEtAlSrs :: SmithEtAlSRS -> SRSDecl -> String -> IO ([FileLayout], SmithEtAlSRS)
writeSmithEtAlSrs syst srsDecl srsFileName = do
  let (srs, syst') = mkDoc syst srsDecl S.forT
  typeCheckSI syst' -- FIXME: This should be done on `System` creation *or* chunk creation!
  let layout = genSmithEtAlSrs syst' srs srsFileName
  pure (layout, syst')

-- | A case study that only outputs an SRS in each of our supported variants.
caseStudyMainSRS :: SmithEtAlSRS -> SRSDecl -> String -> IO ()
caseStudyMainSRS syst srsDecl srsFileName = do
  setSystemLocale
  let exampleName = syst ^. projRepoName
  (docLayouts, _) <- writeSmithEtAlSrs syst srsDecl srsFileName
  writeFiles OverwriteAllowed localPath $ directory [ps|{exampleName}|] docLayouts

-- | A case study that outputs both an SRS in each of our supported variants as
-- well as a single chosen software artifact in optionally many programming
-- languages.
caseStudyMainSRSWCode :: SmithEtAlSRS -> SRSDecl -> String -> Choices -> IO ()
caseStudyMainSRSWCode syst srsDecl srsFileName choices = do
  setSystemLocale
  let exampleName = syst ^. projRepoName
  (docLayouts, syst') <- writeSmithEtAlSrs syst srsDecl srsFileName
  srcLayout <- genCode syst' choices
  writeFiles OverwriteAllowed localPath $ directory [ps|{exampleName}|] $ srcLayout : docLayouts

-- | The same as 'caseStudyMainSRSWCode', except it also produces a
-- JupyterNotebook-based lesson plan.
caseStudyMainSRSWCodeZoo :: SmithEtAlSRS -> SRSDecl -> String -> [Choices] -> IO ()
caseStudyMainSRSWCodeZoo syst srsDecl srsFileName choices = do
  setSystemLocale
  let exampleName = syst ^. projRepoName
  (docLayouts, syst') <- writeSmithEtAlSrs syst srsDecl srsFileName
  zooLayouts <- genCodeZoo syst' choices
  let layout = directory [ps|{exampleName}|] $ docLayouts ++ zooLayouts
  writeFiles OverwriteAllowed localPath layout
