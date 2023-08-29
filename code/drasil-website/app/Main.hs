-- | Performs IO actions to get file path information
-- and then generate an updated Drasil website.
module Main where

import GHC.IO.Encoding
import Language.Drasil.Generate (gen, DocSpec(DocSpec), DocType(Website), Format(..), docChoices)
import Drasil.Website.Body (mkWebsite, printSetting, FolderLocation(..))
import System.Environment (getEnv, lookupEnv)
import Data.Maybe (fromMaybe)

-- | Collect environment variables, place them in 'FolderLocation',
-- and generate the Drasil website.
main :: IO()
main = do
  -- Require the Makefile (or deploy script as it will usually be) to feed locations for where to find certain
  -- files/groups of files in the staged deploy folder.
  deployLocation <- getEnv "DEPLOY_FOLDER"
  docsRoot <- getEnv "DOCS_FOLDER"
  exampleRoot <- getEnv "EXAMPLES_FOLDER"
  --srsDir <- getEnv "SRS_FOLDER_FRAG"
  --doxDir <- getEnv "DOX_FOLDER"
  graphRoot <- getEnv "GRAPH_FOLDER"
  analysisRoot <- getEnv "ANALYSIS_FOLDER"
  listOfPackages <- getEnv "PACKAGES"
  typeGFolder <- getEnv "TYPEGRAPH_FOLDER"
  classIFolder <- getEnv "CLASSINST_GRAPH_FOLDER"

  -- Env variables relating to variables exposed on CI.
  -- Because we want to be able to test site building locally, we fill in these stubs with
  -- (sometimes correct) assumptions.
  repoSlug <- fromMaybe "JacquesCarette/Drasil" <$> lookupEnv "GITHUB_REPOSITORY"
  tree <- fromMaybe "master" <$> lookupEnv "DRASIL_WEBSITE_TREE"
  -- Next two are metadata used to produce the footer.
  buildNumber <- fromMaybe "0" <$> lookupEnv "GITHUB_RUN_NUMBER"
  buildId <- lookupEnv "GITHUB_RUN_ID"

  -- get commit root and build path
  let repoCommitRoot = "https://github.com/" ++ repoSlug ++ "/tree/" ++ tree ++ "/"
      buildPath = "https://github.com/" ++ repoSlug ++ "/actions" ++ maybe "" ("/runs/" ++) buildId

      -- organize all the possible folder locations to use in functions
      allFolders = Folder {depL = deployLocation, docsRt = docsRoot,
        exRt = exampleRoot, graphRt = graphRoot,
        analysisRt = analysisRoot, repoRt = repoCommitRoot, 
        buildNum = buildNumber, buildPth = buildPath,
        typeGraphFolder = typeGFolder, classInstFolder = classIFolder,
        packages = words listOfPackages ++ ["example"]
        -- manually add example because it's not actually a package anymore,
        -- but the analysis scripts work nonetheless, so we display it here.
        }

  -- generate the html document/website.
  setLocaleEncoding utf8
  gen (DocSpec (docChoices Website [HTML]) "index") (mkWebsite allFolders) (printSetting allFolders)