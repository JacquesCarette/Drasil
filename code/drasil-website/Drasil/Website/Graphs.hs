module Drasil.Website.Graphs (graphSec, graphRefs) where

import Language.Drasil

----------------------------------
-- Dependency Graphs Section
----------------------------------

graphSec :: FilePath -> Section
graphSec path = section packDepGraphTitle (mkParagraph (S graphSecIntro) : map (dependencyGraphs path) drasilFolders ++ [UlC $ ulcc $ folderList path]) [] graphSecRef

packDepGraphTitle :: Sentence
drasilFolders :: [String]
drasilDepGraphPathsPDF :: FilePath -> [String]
drasilDepGraphPathsSVG :: FilePath -> FilePath -> String
drasilDepGraphRefs :: FilePath -> [Reference]
graphSecIntro :: String
graphSecIntro = "The below list contains all of the different packages used to build the Drasil Framework. \
  \Each package and its dependencies are displayed in the form of a graph. \
  \Links are available to a pdf version of each graph at the bottom."

packDepGraphTitle = S "Package Dependency Graphs"
-- TODO: Grab these package names from the makefile. Right now, we will need to update it eveery time a new package is added.
drasilFolders = map ("drasil-" ++) ["lang", "metadata", "code-base", "code", "docLang", "printers", "build", "theory", "gool", "data", "database", "example", "gen", "utils", "website"]
drasilDepGraphPathsPDF path = map (\x -> path ++ x ++ ".pdf") drasilFolders
drasilDepGraphPathsSVG path fldr = path ++ fldr ++ ".svg"
drasilDepGraphRefs path = zipWith (\x y -> Reference x (URI y) $ shortname' $ S x) drasilFolders $ drasilDepGraphPathsPDF path

dependencyGraphs :: FilePath -> String -> Contents
dependencyGraphs path mdule = LlC $ llcc (makeFigRef $ "Figure" ++ mdule) $ fig (S $ "Package: " ++ mdule) $ drasilDepGraphPathsSVG path mdule

folderList :: FilePath -> RawContent
folderList path= Enumeration $ Bullet $ zip (folderList' path) $ repeat Nothing

folderList' :: FilePath -> [ItemType]
folderList' path = map Flat (zipWith (\x y -> namedRef y $ S x) drasilFolders $ drasilDepGraphRefs path)

graphSecRef :: Reference
graphSecRef = makeSecRef "DependencyGraphs" $ S "Dependency Graphs"

graphRefs :: FilePath -> [Reference]
graphRefs path = [graphSecRef, ref $ graphSec path] ++ drasilDepGraphRefs path

