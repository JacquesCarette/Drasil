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
type CodeSource = (FilePath, String)
type SRSVariants = [(Name, String)]
type Description = Maybe String
data Example = E Name [CodeSource] SRSVariants Description

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

-- returns a tuple of the link to the specific code folder and its language in the form (urlPath, Language)
getSrc :: String -> FilePath -> CodeSource
-- source comes in as a path, takeBaseName takes only rightmost folder name
getSrc repoRoot source = (repoRoot ++ source, lang $ takeBaseName source)
  where
    -- Some languages might be named differently than the folders they are stored in.
    lang "cpp"    = "C++"
    lang "csharp" = "C#"
    lang "python" = "Python"
    lang "java"   = "Java"
    lang x        = error ("No given display name for language: " ++ x)

mkExamples :: String -> FilePath -> FilePath -> IO [Example]
mkExamples repoRoot path srsDir = do

  -- names will be a sorted list of example names (GamePhysics, GlassBR, etc.) of type FilePath
  names <- sort <$> (listDirectory path >>= filterM (\x -> doesDirectoryExist $ path ++ x))

  -- a list of lists of sources based on existence and contents of src file for each example.
  -- if no src file, then the inner list will be empty
  sources <- mapM (\x -> doesFileExist (path ++ x ++ "/src") >>=
    \y -> if y then map (getSrc repoRoot) . lines . rstrip <$> readFile (path ++ x ++ "/src") else return []) names
    

  -- a list of Just descriptions or Nothing based on existence and contents of desc file.
  descriptions <- mapM (\x -> doesFileExist ("descriptions/" ++ x ++ ".txt") >>=
    \y -> if y then Just . rstrip <$> readFile ("descriptions/" ++ x ++ ".txt") else return Nothing) names

  -- creates a list of SRSVariants, so a list of list of tuples.
  -- the outer list has an element for each example.
  srss <- mapM (\x -> sort . getSRS <$> listDirectory (srsPath path x srsDir)) names

  -- returns the IO list of examples with constructor E containing
  -- the name of the example, sources, list of variants, and description
  return $ map (\(name, source, srs, desc) -> E name source srs desc) $ zip4 names sources srss descriptions

maybeField :: String -> (Item a -> Compiler (Maybe String)) -> Context a
maybeField s f = Context $ \k _ i -> do
  -- val will be of type Maybe String
  val <- f i
  -- if val is Just and the input string matches the html file paramater,
  -- then we use the String contained in val
  if k == s && isJust val then
    return $ StringField $ fromJust val
  else
    -- Otherwise, fail acts to not use the $if$ parameter in the html file
    fail $ "maybeField " ++ s ++ " used when really Nothing. Wrap in `$if(" ++ s ++ ")$` block."

-- List if non-empty
maybeListFieldWith :: String -> Context a -> (Item b -> [c]) -> (Item b -> Compiler [Item a]) -> Context b
-- takes as input: the string to be replaced, the context to be used as is,
-- a function to be used on input to return a list to check for emptiness,
-- and the function to be used regularly on input
maybeListFieldWith s contxt checkNull f = listFieldWith s contxt
  -- If the list is empty, fail is used so as not to create a specific section
  (\x -> if null (checkNull x) then fail ("No instances of " ++ s) else f x)

mkExampleCtx :: FilePath -> FilePath -> Context Example
mkExampleCtx exampleDir srsDir =
  -- each function is applied to the item containing an Example of form:
  -- E Name [CodeSource] SRSVariants Description

  -- here, listFieldWith used with multiple versions of SRS per example
  listFieldWith "srs" (
    -- field will replace the string parameters in the html with the returned string
    -- <> acts as a semigroup concatenator

    -- returns filename from ((filename, TYPE), E _ _ _ _)
    field "filename" (return . fst . srsVar) <>
    -- returns TYPE from ((_, _), E _ _ _ _)
    field "type" (return . snd . srsVar) <>
    -- returns name from ((_, _), E name [codeSource] srsVariants description)
    -- which is the same name as the outer scope example name
    field "name" (return . name . example) <>
    -- gets the URL by taking the path to the srs ++ the filename of the srs
    field "url" (\x -> return $ srsPath exampleDir (name $ example x) srsDir ++ fst (srsVar x))
  -- creates a list of items where each item contains a tuple
  -- the tuple contains (specific srs variant, general Example)
  -- in this way, each srs variant is accounted for, and all information can be accessible 
  ) (\x -> mapM (\y -> makeItem (y, itemBody x)) $ srs $ itemBody x)  <>

  -- Fills in the overall name of each example
  -- by returning name from E name [codeSource] srsVariants description
  field "name" (return . name . itemBody) <>
  -- returns the description of an example if Just, otherwise fails and does not add one
  maybeField "desc" (return . desc . itemBody) <>

  -- Lists sources if they exist
  maybeListFieldWith "src" (
    -- return filepath from (filepath, language)
    field "path" (return . fst . itemBody) <>
    -- return language from (filepath, language)
    field "lang" (return . snd . itemBody)
  -- (src . itemBody) gets the list of sources to be checked for emptiness
  -- (mapM makeItem . src . itemBody) rewraps every item in src to be used internally
  ) (src . itemBody) (mapM makeItem . src . itemBody)
  where
    name (E nm _ _ _) = nm
    src (E _ s _ _) = s
    srs (E _ _ s _) = s
    desc (E _ _ _ d) = d
    srsVar = fst . itemBody
    example = snd . itemBody

mkGraphs :: FilePath -> IO [FilePath]
-- returns an IO list of the paths to all graphs based on an input path
mkGraphs path = sort . filter (endswith ".pdf") <$> listDirectory path

mkGraphCtx :: FilePath -> Context FilePath
mkGraphCtx graphRoot =
  -- fills name with the base name of the file (ie. no paths or extensions)
  field "name" (return . takeBaseName . itemBody) <>
  -- fills url with the link to the file based on the link to the root directory concatenated with the filename
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
