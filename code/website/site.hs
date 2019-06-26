{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM)
import Data.Char (toUpper)
import Data.List (sort, zip4)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Monoid ((<>))
import Data.String.Utils (rstrip, endswith)
import Hakyll
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getEnv, lookupEnv)
import System.FilePath (takeBaseName, takeExtension)

-- type FilePath = String -- from System.FilePath
type Name = String
type SourceLocation = Maybe [String]
type SRSVariants = [(Name, String)]
type Description = Maybe String
data Example = E Name SourceLocation SRSVariants Description

-- Returns FilePath of the SRS
srsPath :: FilePath -> FilePath -> FilePath -> FilePath
srsPath baseDir ex srs = baseDir ++ ex ++ "/" ++ srs

-- Returns the Uppercase String of the given extension
getExtensionName :: String -> String
getExtensionName [] = error "Expected some file extension. Got none."
getExtensionName [_] = error "Expected file extension. Got a single character."
getExtensionName ('.':xs) = map toUpper xs
getExtensionName _ = error "Expected some extension."

-- returns a list of tuples, where each tuple is (Name, string) where string is "HTML" or "PDF"
getSRS :: [Name] -> SRSVariants
getSRS = map (\x -> (x, getExtensionName $ takeExtension x)) . filter (\x -> any (`endswith` x) [".pdf", ".html"])

mkExamples :: String -> FilePath -> FilePath -> IO [Example]
mkExamples repoRoot path srsDir = do
  -- names will be a list of example names (Chipmunk, GlassBR, etc. of type FilePath)
  names <- sort <$> (listDirectory path >>= filterM (\x -> doesDirectoryExist $ path ++ x))

  -- a list of Just sources or Nothing based on existence and contents of src file.
  sources <- mapM (\x -> doesFileExist (path ++ x ++ "/src") >>=
    \y -> if y then Just . map ((++) repoRoot) . lines . rstrip <$> readFile (path ++ x ++ "/src") else return Nothing) names

  -- a list of Just descriptions or Nothing based on existence and contents of desc file.
  descriptions <- mapM (\x -> doesFileExist ("descriptions/" ++ x ++ ".txt") >>=
    \y -> if y then Just . rstrip <$> readFile ("descriptions/" ++ x ++ ".txt") else return Nothing) names

  -- creates a list of SRSVariants, so a list of list of tuples.
  -- the outer list has an element for each example.
  srss <- mapM (\x -> sort . getSRS <$> listDirectory (srsPath path x srsDir)) names

  -- returns the IO list of examples with constructor E containing
  -- the name of the example, sources (if applicable), and the list of Variants.
  return $ map (\(name, source, srs, desc) -> E name source srs desc) $ zip4 names sources srss descriptions

maybeField :: String -> (Item a -> Compiler (Maybe String)) -> Context a
maybeField s f = Context $ \k _ i -> do
  val <- f i
  if k == s && isJust val then
    return $ StringField $ fromJust val
  else
    fail $ "maybeField " ++ s ++ " used when really Nothing. Wrap in `$if(" ++ s ++ ")$` block."

----------- New additions ---------
-- Could be better optimized

maybeListField :: String -> (Item a -> Compiler (Maybe [String])) -> Int -> Context a
maybeListField s f n = Context $ \k _ i -> do
  val <- f i
  if k == s && isJust val then
    return $ StringField $ (fromJust val)!!n
  else
    fail $ "maybeField " ++ s ++ " used when really Nothing. Wrap in `$if(" ++ s ++ ")$` block."

takeFromSlash :: String -> String
takeFromSlash s = takeFromSlashHelper s ""
  where
    takeFromSlashHelper begin end
      | last begin == '/' = end
      | otherwise = takeFromSlashHelper (init begin) (last begin :end)


-- maybeListField "src" (return . src . itemBody) 0 <>
-- maybeListField takeFromSlash (return . src . itemBody)

mkExampleCtx :: FilePath -> FilePath -> Context Example
mkExampleCtx exampleDir srsDir =
  listFieldWith "srs" (
    field "filename" (return . fst . srsVar) <>
    field "type" (return . snd . srsVar) <>
    field "name" (return . name . example) <>
    field "url" (\x -> return $ srsPath exampleDir (name $ example x) srsDir ++ fst (srsVar x))
  ) (\x -> mapM (\y -> makeItem (y, itemBody x)) $ srs $ itemBody x)  <>
  field "name" (return . name . itemBody) <>
  maybeField "desc" (return . desc . itemBody) <>
  -- listFieldWith "src" (
  --   field "path" (function1)
  --   field "lang" (function2)
  -- ) (function3) 
  maybeListField "src" (return . src . itemBody) 0
  where
    name (E nm _ _ _) = nm
    src (E _ s _ _) = s
    srs (E _ _ s _) = s
    desc (E _ _ _ d) = d
    srsVar = fst . itemBody
    example = snd . itemBody

mkGraphs :: FilePath -> IO [FilePath]
mkGraphs path = sort . filter (endswith ".pdf") <$> listDirectory path

mkGraphCtx :: FilePath -> Context FilePath
mkGraphCtx graphRoot =
  field "name" (return . takeBaseName . itemBody) <>
  field "url" (return . (++) graphRoot . itemBody)

main :: IO ()
main = do
  -- Require the Makefile (or deploy script as it will usually be) to feed locations for where to find certain
  -- files/groups of files in the staged deploy folder.
  deployLocation <- getEnv "DEPLOY_FOLDER"
  docsRoot <- getEnv "DOCS_FOLDER"
  exampleRoot <- getEnv "EXAMPLES_FOLDER"
  srsDir <- getEnv "SRS_FOLDER_FRAG"
  graphRoot <- getEnv "GRAPH_FOLDER"

  -- Env variables relating to variables exposed on CI.
  -- Because we want to be able to test site building locally, we fill in these stubs with
  -- (sometimes correct) assumptions.
  travisRepoSlug <- fromMaybe "JacquesCarette/Drasil" <$> lookupEnv "TRAVIS_REPO_SLUG"
  travisCommit <- fromMaybe "master" <$> lookupEnv "TRAVIS_COMMIT"
  -- Next two are metadata used to produce the footer.
  travisBuildNumber <- fromMaybe "0" <$> lookupEnv "TRAVIS_BUILD_NUMBER"
  travisBuildId <- lookupEnv "TRAVIS_BUILD_ID"

  let repoCommitRoot = "https://github.com/" ++ travisRepoSlug ++ "/tree/" ++ travisCommit ++ "/"
  let docsPath = docsRoot ++ "index.html"

  let travisBuildPath = "https://travis-ci.org/" ++ travisRepoSlug ++ maybe "" ("/builds/" ++) travisBuildId

  doesDocsExist <- doesFileExist $ deployLocation ++ docsPath
  examples <- mkExamples repoCommitRoot (deployLocation ++ exampleRoot) srsDir
  graphs <- mkGraphs $ deployLocation ++ graphRoot

  hakyll $ do
    -- These first two matches we have nothing here, but they came with the site.hs when I created
    -- the default template. I'm sure at some point there will be CSS and images as well so these rules
    -- don't hurt.
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match "index.html" $ do
      route idRoute
      compile $ do
        let indexCtx = listField "examples" (mkExampleCtx exampleRoot srsDir) (mapM makeItem examples) <>
                       listField "graphs" (mkGraphCtx graphRoot) (mapM makeItem graphs) <>
                       (if doesDocsExist then field "docsUrl" (return . const docsPath) else mempty) <>
                       field "buildNumber" (return . const travisBuildNumber) <>
                       field "buildUrl" (return . const travisBuildPath) <>
                       field "commit" (return . const travisCommit) <>
                       field "commitUrl" (return . const repoCommitRoot) <>
                       defaultContext

        getResourceBody >>=
          applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/base.html" indexCtx >>=
          relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
