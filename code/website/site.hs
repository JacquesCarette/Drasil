{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM)
import Data.Char (toUpper)
import Data.List (sort)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Monoid ((<>))
import Data.String.Utils (rstrip, endswith)
import Hakyll
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getEnv, lookupEnv)
import System.FilePath (takeBaseName, takeExtension)

type Name = String
type SourceLocation = Maybe String
type SRSVariants = [(Name, String)]
data Example = E Name SourceLocation SRSVariants

docsRoot, exampleRoot, graphRoot :: FilePath
docsRoot = "docs/"
exampleRoot = "examples/"
graphRoot = "graphs/"

srsPath :: FilePath -> FilePath
srsPath ex = exampleRoot ++ ex ++ "/" ++ "srs/"

getExtensionName :: String -> String
getExtensionName [] = error "Expected some file extension. Got none."
getExtensionName [_] = error "Expected file extension. Got a single character."
getExtensionName ('.':xs) = map toUpper xs
getExtensionName _ = error "Expected some extension."

getSRS :: [Name] -> SRSVariants
getSRS = map (\x -> (x, getExtensionName $ takeExtension x)) . filter (\x -> any (`endswith` x) [".pdf", ".html"])

mkExamples :: String -> FilePath -> IO [Example]
mkExamples repoRoot path = do
  names <- sort <$> (listDirectory path >>= filterM (\x -> doesDirectoryExist $ path ++ x))
  sources <- mapM (\x -> doesFileExist (path ++ x ++ "/src") >>=
      \y -> if y then (Just . (++) repoRoot . rstrip) <$> readFile (path ++ x ++ "/src") else return Nothing) names
  srss <- mapM (\x -> sort . getSRS <$> listDirectory (path ++ x ++ "/srs")) names
  return $ map (\(name, source, srs) -> E name source srs) $ zip3 names sources srss

maybeField :: String -> (Item a -> Compiler (Maybe String)) -> Context a
maybeField s f = Context $ \k _ i -> do
  val <- f i
  if k == s && (isJust val) then
    return $ StringField $ fromJust val
  else
    fail $ "maybeField " ++ s ++ " used when really Nothing. Wrap in `$if(" ++ s ++ ")$` block."

mkExampleCtx :: Context Example
mkExampleCtx =
  listFieldWith "srs" (
    field "filename" (return . fst . srsVar) <>
    field "type" (return . snd . srsVar) <>
    field "name" (return . name . example) <>
    field "url" (\x -> return $ srsPath (name $ example x) ++ fst (srsVar x))
  ) (\x -> mapM (\y -> makeItem (y, itemBody x)) $ srs $ itemBody x)  <>
  field "name" (return . name . itemBody) <>
  maybeField "src" (return . src . itemBody)
  where
    name (E nm _ _) = nm
    src (E _ s _) = s
    srs (E _ _ s) = s
    srsVar = fst . itemBody
    example = snd . itemBody

mkGraphs :: FilePath -> IO [FilePath]
mkGraphs path = (sort . filter (endswith ".pdf")) <$> listDirectory path

mkGraphCtx :: Context FilePath
mkGraphCtx =
  field "name" (return . takeBaseName . itemBody) <>
  field "url" (return . (++) graphRoot . itemBody)

main :: IO ()
main = do
  deployLocation <- getEnv "DEPLOY_FOLDER"
  travisRepoSlug <- fromMaybe "JacquesCarette/Drasil" <$> lookupEnv "TRAVIS_REPO_SLUG"
  travisCommit <- fromMaybe "master" <$> lookupEnv "TRAVIS_COMMIT"
  let repoCommitRoot = "https://github.com/" ++ travisRepoSlug ++ "/tree/" ++ travisCommit ++ "/"
  let docsPath = docsRoot ++ "index.html"
  doesDocsExist <- doesFileExist $ deployLocation ++ docsPath
  examples <- mkExamples repoCommitRoot $ deployLocation ++ exampleRoot
  graphs <- mkGraphs $ deployLocation ++ graphRoot

  hakyll $ do
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match "index.html" $ do
      route idRoute
      compile $ do
        let indexCtx = listField "examples" mkExampleCtx (mapM makeItem examples) <>
                       listField "graphs" mkGraphCtx (mapM makeItem graphs) <>
                       (if doesDocsExist then field "docsUrl" (return . const docsPath) else mempty) <>
                       defaultContext

        getResourceBody >>=
          applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/base.html" indexCtx >>=
          relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
