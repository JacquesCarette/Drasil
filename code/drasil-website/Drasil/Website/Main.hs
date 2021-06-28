module Drasil.Website.Main where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, DocSpec(DocSpec), DocType(Website))
import Drasil.Website.Website (mkWebsite, printSetting, FolderLocation(..))
import System.Environment (getEnv, lookupEnv)
import Data.Maybe (fromMaybe)


main :: IO()
main = do
  -- Require the Makefile (or deploy script as it will usually be) to feed locations for where to find certain
  -- files/groups of files in the staged deploy folder.
  deployLocation <- getEnv "DEPLOY_FOLDER"
  docsRoot <- getEnv "DOCS_FOLDER"
  exampleRoot <- getEnv "EXAMPLES_FOLDER"
  srsDir <- getEnv "SRS_FOLDER_FRAG"
  doxDir <- getEnv "DOX_FOLDER"
  graphRoot <- getEnv "GRAPH_FOLDER"
  analysisRoot <- getEnv "ANALYSIS_FOLDER"

  -- Env variables relating to variables exposed on CI.
  -- Because we want to be able to test site building locally, we fill in these stubs with
  -- (sometimes correct) assumptions.
  repoSlug <- fromMaybe "JacquesCarette/Drasil" <$> lookupEnv "GITHUB_REPOSITORY"
  commit <- fromMaybe "master" <$> lookupEnv "GITHUB_SHA"
  -- Next two are metadata used to produce the footer.
  buildNumber <- fromMaybe "0" <$> lookupEnv "GITHUB_RUN_NUMBER"
  buildId <- lookupEnv "GITHUB_RUN_ID"

  -- get commit root and build path
  let repoCommitRoot = "https://github.com/" ++ repoSlug ++ "/tree/" ++ commit ++ "/"
      buildPath = "https://github.com/" ++ repoSlug ++ "/actions" ++ maybe "" ("/runs/" ++) buildId

      -- organize all the possible folder locations to use in functions
      allFolders = Folder {depL = deployLocation, docsRt = deployLocation ++ docsRoot,
        exRt = deployLocation ++ exampleRoot, srsD = deployLocation ++ srsDir, 
        doxD = deployLocation ++ doxDir, graphRt = deployLocation ++ graphRoot, 
        analysisRt = deployLocation ++ analysisRoot, repoRt = repoCommitRoot, 
        buildNum = buildNumber, buildPth = buildPath}

  -- generate the html document/website.
  setLocaleEncoding utf8
  gen (DocSpec Website "index") (mkWebsite allFolders) (printSetting allFolders)