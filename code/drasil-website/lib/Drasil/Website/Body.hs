-- | Gathers and organizes all the information for the [Drasil website](https://jacquescarette.github.io/Drasil/).
module Drasil.Website.Body (
  FolderLocation(..), si, symbMap,
  gitHubRef, sections, websiteTitle,
) where

import Control.Lens ((^.))

import Drasil.Database (ChunkDB)
import Drasil.Generator (cdb)
import Drasil.System
import Language.Drasil
import Drasil.DocLang (findAllRefs)

import Drasil.Website.Introduction (introSec)
import Drasil.Website.About (aboutSec)
import Drasil.Website.CaseStudy (caseStudySec)
import Drasil.Website.Example (exampleSec, exampleRefs, allExampleSI)
import Drasil.Website.Documentation (docsSec, docRefs)
import Drasil.Website.Analysis (analysisSec, analysisRefs)
import Drasil.Website.GettingStarted (gettingStartedSec)
import Data.Drasil.Concepts.Physics (pendulum, motion, rigidBody, twoD)
import Drasil.GlassBR.Unitals (blast)
import Drasil.GlassBR.Concepts (glaSlab)
import Data.Drasil.Concepts.Thermodynamics (heatTrans)
import Drasil.SWHS.Concepts (sWHT, water, phsChgMtrl)
import Drasil.PDController.Concepts (pidC)
import Drasil.Projectile.Concepts (target, projectile)
import Drasil.SSP.Defs (crtSlpSrf, intrslce, slope, slpSrf, factor)
import Data.Drasil.Concepts.SolidMechanics (shearForce, normForce)
import Drasil.SSP.IMods (fctSfty)

-- * Functions to Generate the Website Through Drasil

-- | Folder locations based on environment variables (using 'getEnv' in "Drasil.Website.Main").
data FolderLocation = Folder {
    -- | Deploy location. Currently unused, but may be needed in the future.
    depL :: FilePath
    -- | Haddock documentation root file path. After using @make deploy@, this should be @deploy/docs@.
  , docsRt :: FilePath
    -- | Example root file path. After using @make deploy@, this should be @deploy/examples@.
  , exRt :: FilePath
    -- | Package dependency graph root file path. After using @make deploy@, this should be @deploy/graphs@.
  , graphRt :: FilePath
    -- | Analysis root file path. After using @make deploy@, this should be @deploy/analysis@.
  , analysisRt :: FilePath
    -- | Type graphs root file path. After using @make deploy@, this should be @deploy\/analysis\/TypeDependencyGraphs@.
  , typeGraphFolder :: FilePath
    -- | Class-instance graphs root file path. After using @make deploy@, this should be @deploy\/analysis\/DataTable\/packagegraphs@.
  , classInstFolder :: FilePath
    -- | Repository root, used for linking to generated code in GitHub.
  , repoRt :: FilePath
    -- | Deploy build number. Currently unused.
  , buildNum :: FilePath
    -- | Deploy build path. Currently unused.
  , buildPth :: FilePath
    -- | List of Drasil packages taken from the @Makefile@.
  , packages :: [String]
    }

-- TODO: Should the website be using a ``System''? This is primarily for the SmithEtAl template.
--       It seems like the website is primarily that functions on a chunkdb.

-- | System information.
si :: FolderLocation -> System
si fl = mkSystem
  webName Website []
  [] [] [] []
  [] [] [] []
  ([] :: [DefinedQuantityDict]) ([] :: [DefinedQuantityDict]) ([] :: [ConstrConcept]) []
  symbMap (allRefs fl)

-- | Puts all the sections in order. Basically the website version of the SRS declaration.
sections :: FolderLocation -> [Section]
sections fl = [headerSec, introSec, gettingStartedSec quickStartWiki newWorkspaceSetupWiki contribGuideWiki workflowWiki
  createProjWiki debuggingWiki, aboutSec (ref caseStudySec) (ref $ docsSec $ docsRt fl) (ref $ analysisSec (analysisRt fl)
  (typeGraphFolder fl) (classInstFolder fl) (graphRt fl) $ packages fl) gitHubRef wikiRef infoEncodingWiki chunksWiki recipesWiki
  paperGOOL papersWiki icsePositionPaper danPoster wellUnderstoodPaper, exampleSec (repoRt fl) (exRt fl), caseStudySec, docsSec (docsRt fl),
  analysisSec (analysisRt fl) (typeGraphFolder fl) (classInstFolder fl) (graphRt fl) $ packages fl, footer fl]

-- | Needed for references and terms to work.
symbMap :: ChunkDB
symbMap = cdb ([] :: [DefinedQuantityDict]) (map nw [webName, phsChgMtrl, twoD] ++
  map getSysName allExampleSI ++ map nw [pendulum, motion, rigidBody, blast,
  heatTrans, sWHT, water, pidC, target, projectile, crtSlpSrf, shearForce,
  normForce, slpSrf] ++ [nw $ fctSfty ^. defLhs] ++ [glaSlab, intrslce,
  slope, factor]) ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] []
  [] [] []

-- | Helper to get the system name as an 'IdeaDict' from 'System'.
getSysName :: System -> IdeaDict
getSysName = nw . (^. sysName)

-- | Holds all references and links used in the website.
allRefs :: FolderLocation -> [Reference]
allRefs fl = [gitHubRef, wikiRef, infoEncodingWiki, chunksWiki, recipesWiki, paperGOOL, papersWiki,
  quickStartWiki, newWorkspaceSetupWiki, contribGuideWiki, workflowWiki, createProjWiki, debuggingWiki,
  icsePositionPaper, danPoster, wellUnderstoodPaper]
  ++ exampleRefs (repoRt fl) (exRt fl)
  ++ docRefs (docsRt fl)
  ++ analysisRefs (analysisRt fl) (typeGraphFolder fl) (classInstFolder fl) (graphRt fl) (packages fl)
  ++ concatMap findAllRefs (sections fl)

-- | Used for system name and kind inside of 'si'.
webName :: CI
webName = commonIdeaWithDict "websiteName" (cn websiteTitle) "Drasil" [] -- FIXME: Improper use of a `CI`.

-- * Header Section

-- | Header section creator.
headerSec :: Section
headerSec =
  section EmptyS -- No title
  [LlC imageContent] -- Contents
  [] $ makeSecRef "Header" $ S "Header" -- Section reference

-- | For the drasil tree image on the website.
imageContent :: LabelledContent
imageContent = llccFig "Drasil" $ figNoCapWithWidth EmptyS imagePath 50

-- | Used for the repository link.
gitHubRef :: Reference
gitHubRef = makeURI "gitHubRepo" gitHubInfoURL (shortname' $ S "gitHubRepo")
wikiRef :: Reference
wikiRef = makeURI "gitHubWiki" (gitHubInfoURL ++ "/wiki") (shortname' $ S "gitHubWiki")
infoEncodingWiki :: Reference
infoEncodingWiki = makeURI "InfoEncodingWiki" (gitHubInfoURL ++ "/wiki/Information-Encoding") (shortname' $ S "InfoEncodingWiki")
chunksWiki :: Reference
chunksWiki = makeURI "chunksWiki" (gitHubInfoURL ++ "/wiki/Chunks") (shortname' $ S "chunksWiki")
recipesWiki :: Reference
recipesWiki = makeURI "recipesWiki" (gitHubInfoURL ++ "/wiki/Recipes") (shortname' $ S "recipesWiki")
paperGOOL :: Reference
paperGOOL = makeURI "GOOLPaper" (gitHubInfoURL ++ "/blob/main/Papers/GOOL/GOOL.pdf") (shortname' $ S "GOOLPaper")
papersWiki :: Reference
papersWiki = makeURI "papersWiki" (gitHubInfoURL ++ "/wiki/Drasil-Papers-and-Documents") (shortname' $ S "papersWiki")
icsePositionPaper :: Reference
icsePositionPaper = makeURI "icsePositionPaper" (danContributionPath
  ++ "/ICSE%20Workshop%20-%20SE4Science/ICSE_LiterateFrameworkForSCSoftware_LSS.pdf") (shortname' $ S "icsePositionPaper")
danPoster :: Reference
danPoster = makeURI "danPoster" (danContributionPath
  ++ "/CAS%20Poster%20Competition/Poster/DrasilPoster.pdf") (shortname' $ S "danPoster")
wellUnderstoodPaper :: Reference
wellUnderstoodPaper = makeURI "wellUnderstoodPaper" (gitHubInfoURL
  ++ "/blob/master/Papers/WellUnderstood/wu.pdf") (shortname' $ S "wellUnderstoodPaper")
quickStartWiki :: Reference
quickStartWiki = makeURI "quickStartWiki" (gitHubInfoURL ++ "#quick-start") (shortname' $ S "quickStartWiki")
newWorkspaceSetupWiki :: Reference
newWorkspaceSetupWiki = makeURI "newWorkspaceSetupWiki" (gitHubInfoURL ++ "/wiki/New-Workspace-Setup") (shortname' $ S "newWorkspaceSetupWiki")
contribGuideWiki :: Reference
contribGuideWiki = makeURI "contribGuideWiki" (gitHubInfoURL ++ "/wiki/Contributor's-Guide") (shortname' $ S "contribGuideWiki")
workflowWiki :: Reference
workflowWiki = makeURI "workflowWiki" (gitHubInfoURL ++ "/wiki/Workflow") (shortname' $ S "workflowWiki")
createProjWiki :: Reference
createProjWiki = makeURI "createProjWiki" (gitHubInfoURL ++ "/wiki/Creating-Your-Project-in-Drasil") (shortname' $ S "createProjWiki")
debuggingWiki :: Reference
debuggingWiki = makeURI "debuggingWiki" (gitHubInfoURL ++ "/wiki/Debugging-in-Drasil") (shortname' $ S "debuggingWiki")

-- | Hardcoded info for the title, URL, and image path.
websiteTitle :: String
gitHubInfoURL, imagePath, danContributionPath :: FilePath
websiteTitle = "Drasil - Generate All the Things!"
gitHubInfoURL = "https://github.com/JacquesCarette/Drasil"
danContributionPath = gitHubInfoURL ++ "/blob/master/People/Dan"
imagePath = "./images/Icon.png"

-- * Footer Section

-- | Create the footer section.
footer :: FolderLocation -> Section
footer _ = section EmptyS [mkParagraph copyrightInfo] [] $ makeSecRef "Footer" $ S "Footer"

-- | 'footer' contents.
copyrightInfo :: Sentence
copyrightInfo = S "Copyright (c) Jacques Carette, 2021. All rights reserved. This website is a software artifact generated by Drasil."
